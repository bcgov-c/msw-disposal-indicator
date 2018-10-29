library(sf)
library(bcmaps)
library(bcmaps.rdata)
library(readr)
library(magrittr)
library(dplyr)
library(stringr)
library(rmapshaper)

# Read data -----------------------------------------------------------------------------------------
### indicator data btained from BC Data Catolog and place in data/
indicator <- read.csv('data/bcmunicipalsolidwastedisposal.csv')
message("change 0 to NA - check with andy")
indicator$Disposal_Rate_kg[which(indicator$Disposal_Rate_kg == 0)] <- NA_integer_

link <- read_csv('data/rd_report_links.csv')

district <- bcmaps::combine_nr_rd() %>%
  select(Regional_District = ADMIN_AREA_NAME) %>%
  # remove this for final run
  ms_simplify()

# Check/fix joins by regional district name -----------------------------------------------------------
### combine Comox and Strathcona into multipolygon
district$Regional_District[which(district$Regional_District %in% c("Comox Valley Regional District", "Strathcona Regional District"))] <- "Comox-Strathcona"
district %<>% 
  group_by(Regional_District) %>%
  summarise(do_union = FALSE) %>%
  ungroup()

### match district names by removing words and hyphenating
district$Regional_District %<>%
  str_replace(" Regional District", "") %>%
  str_replace("Regional District of ", "") %>%
  str_replace(" ", "-")

### fix those that shouldn't be hyphenated
indicator$Regional_District %<>% as.character
missing <- anti_join(indicator, district, 'Regional_District')$Regional_District %>%
  unique %>%
  str_replace(" ", "-")

district %<>% 
  mutate(Regional_District = if_else(Regional_District %in% missing,
                                     str_replace(Regional_District, "-", " "),
                                     Regional_District))

### fix Fraser Fort George and Northern Rockies
district$Regional_District[which(district$Regional_District == "Fraser Fort-George")] <- "Fraser-Fort George"
district$Regional_District[which(district$Regional_District == "Northern-Rockies Regional Municipality")] <- "Northern Rockies"

### fix Stikine for labels
district$Regional_District[which(district$Regional_District == "Stikine-(Unincorporated)")] <- "Stikine (Unincorporated)"
### NOTE: Stikine has no indicator data

### final check of data joins
# anti_join(indicator, link, c('Regional_District' = 'Local_Govt_Name'))
# anti_join(indicator, district, 'Regional_District')

district %<>%
  st_transform(4326) 

district$Regional_District %<>% 
  factor()
indicator$Regional_District %<>% 
  factor(levels = levels(district$Regional_District))

district <- district %>%
  left_join(indicator %>%
              filter(Year == 2016), "Regional_District")

# dataset including data by year, but with only one geometry per district
# district$Year <- 2016
# district <- district %>% select(Regional_District, Year) %>%
#   full_join(indicator, c("Regional_District", "Year")) 

# rename sfc column, as some plotting packages only recognise sfc column called 'geometry'
district <- district %>% 
  as_tibble %>%
  rename(geometry = SHAPE) %>%
  st_set_geometry("geometry")

# for ggiraph arg onclick, set search value of datatable
# district$Onclick <- sprintf("set_search_val(\"%s\");", as.character(district$Regional_District))

# create tooltip labels and add to data
district$Label <- sprintf(
  "<strong>%s</strong><br>2016 Population: %s<br>2016 Total Waste: %s",
  district$Regional_District,
  if_else(district$Regional_District == "Stikine (Unincorporated)", "No Data", paste(district$Population)),
  if_else(district$Regional_District == "Stikine (Unincorporated)", "No Data", paste(district$Disposal_Rate_kg, "tonnes"))
) %>% lapply(htmltools::HTML) %>%
  setNames(district$Regional_District)

# Save objects
dir.create("dataviz/app/data", showWarnings = FALSE)

saveRDS(indicator, file = "dataviz/app/data/indicator.rds")
saveRDS(district, file = "dataviz/app/data/district.rds")
saveRDS(link, file = "dataviz/app/data/link.rds")











