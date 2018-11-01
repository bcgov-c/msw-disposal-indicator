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
  ms_simplify()

coastline <- bcmaps::bc_bound() %>%
  ms_simplify(keep = 0.2)

district %<>% st_intersection(coastline) %>%
  st_transform(3005)
  
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

# rename sfc column, as some plotting packages only recognise sfc column called 'geometry'
district <- district %>% 
  as_tibble %>%
  rename(geometry = SHAPE) %>%
  st_set_geometry("geometry")

# create tooltip labels and add to data
create_tooltip <- function(data){
  sprintf(
    "<strong>%s, %s</strong><br><strong>Waste disposal rate:</strong><br>%s<br><strong>Population:</strong> %s",
    data$Regional_District,
    data$Year,
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
                                    "Population > 100,000",
                                    "Population < 100,000"))

indicator$Year %<>% factor()
indicator$Label <- create_tooltip(indicator)

indicator_summary <- indicator %>% 
  filter(!is.na(Disposal_Rate_kg)) %>%
  group_by(Year) %>% 
  filter(n() > 25) %>% # Only calculate prov totals when more than 25 RDs reported
  summarise(Regional_District = "British Columbia",
            Population = sum(Population, na.rm = TRUE),
            Total_Disposed_Tonnes = sum(Total_Disposed_Tonnes, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Disposal_Rate_kg = Total_Disposed_Tonnes/Population * 1000,
         Year = factor(Year, levels = levels(indicator$Year)))

indicator_summary$Label <- create_tooltip(indicator_summary)

# Save objects
dir.create("dataviz/app/data", showWarnings = FALSE)

saveRDS(indicator, file = "dataviz/app/data/indicator.rds")
saveRDS(indicator_summary, file = "dataviz/app/data/indicator_summary.rds")
saveRDS(district, file = "dataviz/app/data/district.rds")
saveRDS(coastline, file = "dataviz/app/data/coastline.rds")
saveRDS(link, file = "dataviz/app/data/link.rds")











