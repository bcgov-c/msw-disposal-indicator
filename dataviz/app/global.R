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
coastline <- readRDS("data/coastline.rds")
link <- readRDS("data/link.rds")

stikine <- "Stikine (Unincorporated)"
bc_title <- "British Columbia Disposal Rates"
stikine_title <- "No Data for Stikine (Unincorporated)"

tooltip_css <- "background: white; opacity: 1; color: black; border-radius: 5px; 
                padding: 5px; box-shadow: 3px 3px 5px 0px #888888;
                font-size: 12px; border-width 2px; border-color: black;"

msw_blue <- "#8EA1C9"
msw_green <- "#6ac1a5"
msw_orange <- "#fa8d67"
msw_hover <- "#808080"
msw_select <- "#808080"
msw_grey <- "#909090"
msw_na <- "white"

line_size <- 0.5
txt_size <- 12

tints <- rev(c("#485165", "#566179", "#64718D", "#7281A1", "#8091B5", "#8EA1C9", "#99AACE", "#A4B3D3", "#AFBCD8", "#BAC5DD", "#C5CEE2", "#D0D7E7", "#DBE0EC", "#E6E9F1", "#F1F2F6"))

p1.w <- 900
p1.h <- 550
p2.w <- 800
p2.h <- 300
