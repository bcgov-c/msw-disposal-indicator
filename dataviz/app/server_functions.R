
# ------------------------------ utils
translate_px <- function(x) paste0(x, "px")
translate_in <- function(x) x/72
div_css <- function(w, h, extra = ""){
  paste("width:", translate_px(w), "; height:", translate_px(h),"; text-align: center; overflow-x: visible; overflow-y: visible;", extra)
}

# ------------------------------ ggplots
gg_map <- function(data){
  ggplot() +
    geom_sf_interactive(data = data, 
                        aes(tooltip = Label, 
                            data_id = Regional_District,
                            fill = Disposal_Rate_kg),
                        size = 0.3, color = "black") +
    coord_sf(crs = 3005, datum = NA) +
    theme_void() +
    # scale_fill_distiller(palette = "Blues", direction = 1,
    #                     na.value = msw_na, name = "Disposal\n(kg / person)") +
    scale_fill_gradientn(colours = tints, na.value = msw_na, 
                         name = "Disposal\n(kg / person)") +
    theme(axis.text = element_blank(),
          legend.position = c(0.2, 0.135),
          legend.title = element_text())
}

gg_bar_rd <- function(data, hline){
  ggplot(data = na.omit(data) %>% as.data.frame,
         aes(x = Regional_District, 
             y = Disposal_Rate_kg,
             fill = factor(Fill),
             tooltip = Label, 
             data_id = Regional_District)) + 
    geom_bar_interactive(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = c(msw_green, 
                                  msw_orange)) +
    theme_bw() +
    scale_y_continuous(position = "right", 
                       breaks = c(0, cumsum(rep(1000/6, 6))),
                       labels = c(0, "", "", "", "", "", "1,000"), 
                       limits = c(0, 1000)) +
    scale_x_discrete(expand = expand_scale(0.03)) +
    labs(x = NULL, y = "Disposal (kg / person)") +
    theme(axis.text = element_text(size = txt_size),
          legend.position = "bottom",
          legend.title = element_blank(),
          # legend.text = element_text(size = txt_size),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(color = msw_grey),
          axis.line.x = element_line(color = msw_grey, size = line_size),
          panel.border = element_blank(),
          axis.title = element_text(size = txt_size),
          panel.grid = element_blank(), 
          plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
          ) +
    geom_hline(yintercept = hline, size = 0.3, color = msw_grey) +
    annotate("text", 1.5, hline + 220, 
                  label = paste0("B.C.\n(", format(hline, digits = 0), " kg / person)"),
              size = 4, color = msw_grey) 
}

gg_bar_year <- function(data){
  ggplot(data = data, 
         aes(x = Year, 
             y = Disposal_Rate_kg,
             tooltip = Label,
             data_id = Year)) + 
    geom_bar_interactive(stat = "identity", fill = msw_blue) +
    scale_x_discrete(drop = FALSE, 
                     breaks = paste(seq(1990, 2016, 2)),
                     labels = paste(seq(1990, 2016, 2)),
                     expand = expand_scale(0.04)) +
    scale_y_continuous(expand = c(0,0), 
                       breaks = c(0, 500, 1000, 1500, 2000),
                       labels = c("0", "500", "1,000", "1,500", "2,000"),
                       limits = c(0, max(indicator$Disposal_Rate_kg, na.rm = T))) +
    scale_fill_manual(drop = FALSE) +
    labs(x = NULL, y = "Disposal (kg / person)") +
    theme_bw() +
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = msw_grey, size = line_size),
          axis.ticks = element_line(color = msw_grey),
          axis.ticks.length=unit(.25, "cm"),
          axis.text = element_text(size = txt_size),
          axis.title = element_text(size = txt_size),
          plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
}


sort_data <- function(data, x){
  if(x == "pop"){
    return(forcats::fct_reorder(data$Regional_District, data$Population))
  }
  if(x == "name"){
    return(factor(data$Regional_District))
  }
  if(x == "rate"){
    return(forcats::fct_reorder(data$Regional_District, data$Disposal_Rate_kg))
  }
}

prepare_links <- function(data){
  data <- t(data[c("swmPlan", "wComposition")]) %>% 
    as.data.frame()
  colnames(data) <- "web"
  data$label <- c("Solid Waste Management Plan",
                  "Waste Composition Report")
  data$icon <- c("file",
                 "stats")
  data$lib <- c("font-awesome",
                "glyphicon")
  data <- data[which(!is.na(data$web)),]
  data
}
