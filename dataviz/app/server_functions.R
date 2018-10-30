
# ------------------------------ utils
translate_px <- function(x) paste0(x, "px")
translate_in <- function(x) x/72

# ------------------------------ ggplots
gg_map <- function(data){
  ggplot(data = data, aes(tooltip = Label, data_id = Regional_District)) +
    geom_sf_interactive() +
    coord_sf(datum = NA) +
    theme(axis.text = element_blank())
}

gg_bar_rd <- function(data){
  ggplot(data = na.omit(data), aes(x = forcats::fct_reorder(Regional_District, Disposal_Rate_kg), 
                                   y = Disposal_Rate_kg,
                                   tooltip = Label, 
                                   data_id = Regional_District)) + 
    geom_bar_interactive(stat = "identity") +
    coord_flip() +
    labs(x = NULL, y = "2016 Disposal Rate (kg)") +
    theme(axis.text = element_text(size = 10))
}

gg_bar_year <- function(data){
  ggplot(data = data, aes(x = Year, 
                          y = Disposal_Rate_kg,
                          tooltip = Label,
                          data_id = Year)) + 
    geom_bar_interactive(stat = "identity") +
    scale_x_discrete(drop = FALSE) +
    scale_fill_discrete(drop = FALSE) +
    labs(x = NULL, y = "Disposal Rate (kg)") +
    theme(legend.title = element_blank())
}

