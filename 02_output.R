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

source("R/helpers.R")

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
  write_csv(path = "out/BC_Municipal_Solid_Waste_Disposal.csv", na = "")

## Write out a file for use in d3 dataviz on the web.
msw %>% 
  left_join(reports.df, by = "Regional_District") %>% 
  select(-ends_with("_check")) %>%
  write_csv(path = "out/BC_Municipal_Solid_Waste_Disposal_viz.csv")