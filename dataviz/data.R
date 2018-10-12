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
indicator <- read_csv('data/bcmunicipalsolidwastedisposal.csv')

link <- read_csv('data/rd_report_links.csv')

district <- bcmaps::combine_nr_rd() %>%
  select(Regional_District = ADMIN_AREA_NAME) %>%
  # remove this for final run
  ms_simplify()

# Check/fix joins by regional district name -----------------------------------------------------------
### combine Comox and Strathcona into multipolygon
district$Regional_District[which(district$Regional_District %in% c("Comox Valley Regional District", "Strathcona Regional District"))] <- "Comox-Strathcona"

district %<>% group_by(Regional_District) %>%
  summarise(do_union = FALSE) %>%
  ungroup()

### match district names by removing words and hyphenating
district$Regional_District %<>%
  str_replace(" Regional District", "") %>%
  str_replace("Regional District of ", "") %>%
  str_replace(" ", "-")

### fix those that shouldn't be hyphenated
missing <- anti_join(indicator, district, 'Regional_District')$Regional_District %>%
  unique %>%
  str_replace(" ", "-")

district %<>% mutate(Regional_District = if_else(Regional_District %in% missing,
                                                 str_replace(Regional_District, "-", " "),
                                                 Regional_District))

### fix Fraser Fort George and Northern Rockies
district$Regional_District[which(district$Regional_District == "Fraser Fort-George")] <- "Fraser-Fort George"
district$Regional_District[which(district$Regional_District == "Northern-Rockies Regional Municipality")] <- "Northern Rockies"

if(nrow(anti_join(indicator, district, 'Regional_District')) != 0L){
  messge("There are still unmatched districts")
}
if(nrow(anti_join(indicator, link, c('Regional_District' = 'Local_Govt_Name'))) != 0L){
  messge("There are still unmatched districts")
}
message("Stikine Reginoal District (Unincorporated) has no indicator data because population too small")

### check link-indicator join
if(nrow(anti_join(indicator, link, c('Regional_District' = 'Local_Govt_Name'))) != 0L){
  messge("There are still unmatched districts")
}





