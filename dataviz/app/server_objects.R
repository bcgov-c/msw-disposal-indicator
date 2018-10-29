# read data
indicator <- readRDS("data/indicator.rds")
district <- readRDS("data/district.rds")
link <- readRDS("data/link.rds")

p1.width <- 600
p1.height <- 400

p2.width <- 600
p2.height <- 200

translate_px <- function(x) paste0(x, "px")
transalte_in <- function(x) x/72