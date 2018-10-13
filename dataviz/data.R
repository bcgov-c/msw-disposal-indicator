library(sf)
library(bcmaps)
library(bcmaps.rdata)
library(readr)
library(magrittr)
library(dplyr)
library(stringr)
library(rmapshaper)

dir.create("tmp", showWarnings = FALSE)

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

# Save objects
saveRDS(indicator, file = "tmp/indicator.rds")
saveRDS(district, file = "tmp/district.rds")
saveRDS(link, file = "tmp/link.rds")

# Create horizontal barcharts -----------------------------------------------------------
library(ggplot2)
library(envreportutils)
library(purrr)

# Function to generate horizontal barchart for popup
horizontal_barchart <- function(data){
  ggplot(data = data, aes(x = Year, y = Total_Disposed_Tonnes)) +
    geom_bar(stat = 'identity') +
    scale_y_continuous(expand = c(0,0)) +
    theme_soe() +
    theme(legend.position="none", 
          panel.grid.major.y = element_blank(), 
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16)) +
    labs(y = "Total Disposed Tonnes") +
    NULL
}

#For each Regional District, generate plot
rd <- unique(indicator$Regional_District)
plot_list <- map(rd, ~ {
  data <- filter(indicator, Regional_District == .) %>%
    horizontal_barchart()
})

# Save objects
saveRDS(plot_list, file = "tmp/plot_list.rds")





  









