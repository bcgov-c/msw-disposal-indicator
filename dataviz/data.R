library(sf)
library(lwgeom)
library(bcmaps)
library(readr)
library(magrittr)
library(dplyr)
library(stringr)
library(ggplot2)
library(rmapshaper)

# Read data -----------------------------------------------------------------------------------------
### indicator data btained from BC Data Catolog and place in data/
indicator <- read_csv('out/BC_Municipal_Solid_Waste_Disposal.csv')
message("changed indicator Disposal_Rate_kg with value 0 to NA")
indicator$Disposal_Rate_kg[which(indicator$Disposal_Rate_kg == 0)] <- NA_integer_
max_year <- max(indicator$Year, na.rm = T)

link <- read_csv('data/rd_report_links.csv')

district <- bcmaps::combine_nr_rd() %>%
  select(Regional_District = ADMIN_AREA_NAME) %>%
  ms_simplify(0.005)

district %<>% st_intersection(bcmaps::bc_bound() %>%
                                 ms_simplify(keep = 0.05)) 
  
# Check/fix joins by regional district name -----------------------------------------------------------
### combine Comox and Strathcona into multipolygon
district$Regional_District[which(district$Regional_District %in% c("Comox Valley Regional District", "Strathcona Regional District"))] <- "Comox-Strathcona"
district %<>% 
  group_by(Regional_District) %>%
  summarise(do_union = FALSE) %>%
  ungroup() %>% 
  st_make_valid() %>% 
  st_collection_extract("POLYGON")

### match district names by removing words and hyphenating
district$Regional_District %<>%
  str_replace(" Regional District", "") %>%
  str_replace("Regional District of ", "") %>%
  str_replace(" ", "-") %>% 
  str_replace("Powell-River", "qathet")

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
district$Regional_District[which(district$Regional_District == "Stikine-Region (Unincorporated)")] <- "Stikine (Unincorporated)"
### NOTE: Stikine has no indicator data

### final check of data joins
# anti_join(indicator, link, c('Regional_District' = 'Local_Govt_Name'))
# anti_join(indicator, district, 'Regional_District')

district$Regional_District %<>% 
  factor()
indicator$Regional_District %<>% 
  factor(levels = levels(district$Regional_District))

district <- district %>%
  left_join(indicator %>%
              filter(Year == max_year), "Regional_District")

# function to create tooltip labels 
create_tooltip <- function(data){
  sprintf(
    "<strong>%s, %s</strong><br><strong>Waste disposal rate:</strong><br>%s<br><strong>Population:</strong> %s",
    data$Regional_District,
    if_else(data$Regional_District == "Stikine (Unincorporated)", 
            "", 
            as.character(data$Year)),
    if_else(data$Regional_District == "Stikine (Unincorporated)", 
            "No Data", 
            paste(format(data$Disposal_Rate_kg, digits = 0), "kg / person")),
    if_else(data$Regional_District == "Stikine (Unincorporated)", 
            "No Data", 
            paste(format(data$Population, big.mark = ",")))
  ) %>% lapply(htmltools::HTML) %>%
    setNames(data$Regional_District)
}

district$Label <- create_tooltip(district)
district %<>% mutate(Fill = if_else(Population > 100000, 
                                    "> 100,000",
                                    "< 100,000"))

indicator$Year %<>% factor()
indicator$Label <- create_tooltip(indicator)

indicator_summary <- indicator %>% 
  filter(!is.na(Population)) %>%
  group_by(Year) %>% 
  filter(n() >= 24) %>% # Only calculate prov totals when at least 24 RDs reported
  summarise(Regional_District = "British Columbia",
            Population = sum(Population, na.rm = TRUE),
            Total_Disposed_Tonnes = sum(Total_Disposed_Tonnes, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Disposal_Rate_kg = Total_Disposed_Tonnes/Population * 1000,
         Year = factor(Year, levels = levels(indicator$Year)))

indicator_summary$Label <- create_tooltip(indicator_summary)

indicator <- indicator[which(!is.na(indicator$Disposal_Rate_kg)),]

# fortify spatial data for ggiraph::geom_polygon_interactive
# because ggiraph::geom_sf_interactive cannot be deployed currently
district_fort <- fortify(as(district, "Spatial"), region = "Regional_District") %>%
  left_join(district %>% 
              as.data.frame %>% 
              select(Regional_District, Label, Disposal_Rate_kg, Fill, Year) %>%
              mutate(Regional_District = as.character(Regional_District)), by = c("id" = "Regional_District")) %>%
  rename(Regional_District = id)

# Save objects
dir.create("dataviz/app/data", showWarnings = FALSE)

saveRDS(indicator, file = "dataviz/app/data/indicator.rds")
saveRDS(indicator_summary, file = "dataviz/app/data/indicator_summary.rds")
saveRDS(district %>% as.data.frame, file = "dataviz/app/data/district.rds")
saveRDS(district_fort, file = "dataviz/app/data/district_fort.rds")
saveRDS(link, file = "dataviz/app/data/link.rds")











