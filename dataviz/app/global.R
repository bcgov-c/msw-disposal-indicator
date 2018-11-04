library(ggplot2)
library(shiny)
library(ggiraph)
library(patchwork)
library(forcats)

source("server_functions.R", local = TRUE)

# read data
indicator <- readRDS("data/indicator.rds") 
indicator_summary <- readRDS("data/indicator_summary.rds") 
district <- readRDS("data/district.rds")
district_fort <- readRDS("data/district_fort.rds")
coastline <- readRDS("data/coastline.rds")
link <- readRDS("data/link.rds")

stikine <- "Stikine (Unincorporated)"
bc_title <- "British Columbia Disposal Rates"
stikine_title <- "No Data for Stikine (Unincorporated)"

line_size <- 0.5
txt_size <- 13

max_year <- max(district$Year, na.rm = TRUE)

tooltip_css <- "background: white; opacity: 1; color: black; border-radius: 5px; 
                padding: 5px; box-shadow: 3px 3px 5px 0px #888888;
                font-size: 12px; border-width 2px; border-color: black;"

hex_bar2 <- "#808080"
hex_bar1_1 <- "#6ac1a5"
hex_bar1_2 <- "#fa8d67"
hex_hover <- "#808080"
hex_select <- "#808080"
hex_axis <- "#909090"
hex_na <- "white"

tinter_shader <- function(hex, steps = 10, crop = 2){
  shades <- rev(grDevices::colorRampPalette(c(hex, "black"))(steps))[-(1:crop)]
  tints <- rev(grDevices::colorRampPalette(c(hex, "white"))(steps))[-(1:crop)]
  c(tints, rev(shades))
}

hex_choro <- "#8ea1c9"
tints <- tinter_shader(hex_choro)

p1.w <- 900
p1.h <- 550
p2.w <- 800
p2.h <- 250

