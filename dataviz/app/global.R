library(ggplot2)
library(purrr)
library(envreportutils)
library(shiny)
library(ggiraph)
library(patchwork)
library(shinyjs)
library(forcats)

source("server_functions.R", local = TRUE)

# read data
indicator <- readRDS("data/indicator.rds")
indicator_summary <- readRDS("data/indicator_summary.rds")
district <- readRDS("data/district.rds")
coastline <- readRDS("data/coastline.rds")
link <- readRDS("data/link.rds")

msw_blue <- "#8ea1c9"
msw_green <- "#6ac1a5"
msw_orange <- "#fa8d67"
msw_light <- "#dde2ee"
msw_dark <- "#63708c"
msw_hover <- "#808080"
msw_select <- "#808080"
msw_na <- "white"

p1.w <- 800
p1.h <- 450
p2.w <- 800
p2.h <- 300
p3.w <- 800
p3.h <- 100