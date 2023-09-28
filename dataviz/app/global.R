library(ggplot2)
library(shiny)
library(ggiraph)
library(patchwork)
library(forcats)
library(sf)
library(bcdata)

source("server_functions.R", local = TRUE)

# read data
indicator <- readRDS("data/indicator.rds") 
indicator_summary <- readRDS("data/indicator_summary.rds") 
district <- readRDS("data/district.rds") 
link <- readRDS("data/link.rds")

line_size <- 0.5
txt_size <- 13

years <- range(as.numeric(as.character(indicator$Year)), na.rm = TRUE)
min_year <- years[1]
max_year <- years[2]

stikine <- "Stikine (Unincorporated)"
bc_title <- paste0("British Columbia Disposal Rates ", 
                   "(", min_year, "-", max_year, ")")
stikine_title <- "No Data for Stikine (Unincorporated)"

tooltip_css <- "background: white; opacity: 1; color: black; border-radius: 5px; 
                padding: 5px; box-shadow: 3px 3px 5px 0px #888888;
                font-size: 12px; border-width 2px; border-color: black;"

hex_bar2 <- "#bebada"  #"#808080"
hex_bar1_1 <- "#b2df8a"
hex_bar1_2 <- "#33a02c"
hex_hover <- "#FFFF99"  #"#808080"
hex_select <- "#bebada"  #"#808080"
hex_axis <- "#909090"
hex_na <- "grey80"

tinter_shader <- function(hex, steps = 10, crop = 2){
  shades <- rev(grDevices::colorRampPalette(c(hex, "black"))(steps))[-(1:crop)]
  tints <- rev(grDevices::colorRampPalette(c(hex, "white"))(steps))[-(1:crop)]
  c(tints, rev(shades))
}

hex_choro <- "#a6cee3"
# tints <- tinter_shader(hex_choro)
tints <- c('#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#08519c','#08306b')

p1.w <- 900
p1.h <- 550
p2.w <- 800
p2.h <- 250

spaces <- function(n) {
  paste0(rep("&nbsp;", n), collapse = "")
}
