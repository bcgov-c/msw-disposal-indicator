# Copyright 2018 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(readr)
library(dplyr)
library(ggplot2)

source("R/helpers.R")

# Get current data from BC Data Catalogue:
url <- "https://catalogue.data.gov.bc.ca/dataset/d21ed158-0ac7-4afd-a03b-ce22df0096bc/resource/d2648733-e484-40f2-b589-48192c16686b/download/bcmunicipalsolidwastedisposal.csv"

old_msw <- read_csv(url)

# Add 2020 data -----------------------------------------------------------
## Data obtained from program area and put in 'data/' folder

data_2020 <- read_csv("data/2020-MSW-Disposal-ERBC1.csv") %>%
  mutate(Year = 2020,
         Member = recode(Member, "Comox Valley Regional District (Strathcona)" = "Comox-Strathcona"),
         Member = gsub("^Regional District( of)? | Regional (District|Municipality)$", "", Member))


data_2020 <- data_2020 %>% 
  mutate(Regional_District = match_rd_names(Member, old_msw$Regional_District, 1)) %>%
  select(Regional_District, Year, Population, Total_Disposed_Tonnes)


msw <- bind_rows(old_msw, data_2020)

## Combine Comox and Strathcona -----------------------------------------------

msw <- msw %>% 
  mutate(Regional_District = ifelse(grepl("Comox|Strathcona", Regional_District),
                                    "Comox-Strathcona", Regional_District)) %>%
  group_by(Regional_District, Year) %>%
  summarise(Total_Disposed_Tonnes = sum(Total_Disposed_Tonnes, na.rm = TRUE),
            Population = sum(Population)) %>%
  arrange(Regional_District, Year) %>% 
  ## Remove Stikine
  filter(Regional_District != "Stikine")


# ----------------------------------------------------------------------------
## Calculate provincial totals and calculate per-capita disposal in kg, and fill in years

msw <- msw %>% 
  group_by(Year) %>%
  filter(n() > 25) %>% # Only calculate prov totals when more than 25 RDs reported
  summarise(Regional_District = "British Columbia",
            Population = sum(Population, na.rm = TRUE),
            Total_Disposed_Tonnes = sum(Total_Disposed_Tonnes, na.rm = TRUE)) %>%
  bind_rows(msw, .) %>%
  ungroup() %>%
  mutate(Year = as.integer(Year),
         Population = as.integer(round(Population)),
         Disposal_Rate_kg = as.integer(round(Total_Disposed_Tonnes / Population * 1000))) %>%
  fill_years() %>%
  select(Regional_District, Year, Total_Disposed_Tonnes, Population, Disposal_Rate_kg)

msw %>%
  filter(Regional_District != "British Columbia") %>%
  ggplot(aes(x = Year, y = Disposal_Rate_kg)) +
  geom_point(aes(size = log10(Population))) +
  facet_wrap(~Regional_District)

# ----------------------------------------------------------------------------
## Join RD data such as Waste Management Plan links etc. for the visualization
reports.df <- read_csv('data/rd_report_links.csv')

## Format names and select columns, check links to MSW report pages
reports.df <- reports.df %>%
  rename(Regional_District = Local_Govt_Name) %>%
  select(Regional_District, swmPlan, wComposition) %>%
  mutate(swmPlan_check = check_url(swmPlan),
         wComposition_check = check_url(wComposition))

dir.create("out", showWarnings = FALSE)

## Remove BC totals for the DataBC version
msw %>%
  filter(Regional_District != "British Columbia") %>%
  write_csv(file = "out/BC_Municipal_Solid_Waste_Disposal.csv", na = "")

## Write out a file for use in d3 dataviz on the web.
msw %>% 
  left_join(reports.df, by = "Regional_District") %>% 
  select(-ends_with("_check")) %>%
  write_csv(file = "out/BC_Municipal_Solid_Waste_Disposal_viz.csv")
