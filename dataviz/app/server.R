# Copyright 2018 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# source("server_objects.R", local = TRUE)

shinyServer(function(input, output, session) {
  
  sort_by <- reactiveValues(clicked = "rate")
  observeEvent(input$sort_name, {sort_by$clicked = "name"})
  observeEvent(input$sort_rate, {sort_by$clicked = "rate"})
  observeEvent(input$sort_population, {sort_by$clicked = "pop"})
  
  yearly <- reactiveValues(data = indicator_summary)
  observe({
    if(!is.null(input$plot_rd_selected)){
      yearly$data = indicator[which(indicator$Regional_District %in% input$plot_rd_selected),]
    }
  })
  observeEvent(input$show_bc, {
    yearly$data = indicator_summary})
  
  district_data <- reactive({
    district$Regional_District <- sort_data(district, sort_by$clicked)
    district
  })
  
  links <- reactiveValues(data = NULL)
  observe({
    if(!is.null(input$plot_rd_selected)){
      links$data = link[which(link$Local_Govt_Name %in% input$plot_rd_selected),]
    }
  })
  observeEvent(input$show_bc, {
    links$data = NULL
    session$sendCustomMessage(type = 'plot_rd_set', message = character(0))})
  observe({
    if(!is.null(links$data)){toggle("show_bc")}
    })

  output$ui_resources <- renderUI({
    req(links$data)
    HTML("<strong>Resources:</strong>")
  })
  
  output$ui_dl <- renderUI({
    req(links$data)
    data <- links$data
    data <- prepare_links(data)
    lapply(1:nrow(data), function(x){
      x <- data[x,]
      actionButton(inputId = row.names(x), label = x$label,
                   onclick = paste0("window.open('", x$web, "')"),
                   class = "msw-button")
    })
  })
  
  output$ui_info <- renderUI({
    if(is.null(links$data)){
      return(h2(HTML(paste("Disposal rates in British Columbia (kg per person)"))))
    }
    data <- yearly$data 
    data <- data[order(data$Year),]
    data <- data[1,]
    rd <- h2(HTML(paste("Disposal rates in", data$Regional_District, "(kg per person)")))
    pop <- paste("Population, 2016:", format(data$Population, big.mark = ","))
    rate <- paste("Disposal Rate, 2016:", data$Disposal_Rate_kg, "(kg / person)")
    HTML(paste(rd, pop, "<br>", rate))
  })
  
  output$ui_show <- renderUI({
    req(links$data)
    actionLink(inputId = "show_bc", "Back to British Columbia rates", class = 'msw-link')
  })
  
  output$plot_rd <- renderGirafe({
    data <- district_data()
    hline <- indicator_summary$Disposal_Rate_kg[indicator_summary$Year == 2016]
    girafe(code = print(gg_map(data) - gg_bar_rd(data, hline) + plot_layout(ncol = 2,
                                                                            widths = c(8, 3))), 
           width_svg = translate_in(p1.w), 
           height_svg = translate_in(p1.h)) %>%
      girafe_options(opts_selection(type = "single", 
                                    css = paste0("fill: ", msw_select, ";")),
                     opts_hover(css = paste0("fill: ", msw_hover, ";")), 
                     opts_tooltip(css = tooltip_css, opacity = 1))
  })
  
  output$plot_year <- renderGirafe({
    data <- yearly$data
    girafe(code = print(gg_bar_year(data)),
           width_svg = translate_in(p2.w), 
           height_svg = translate_in(p2.h)) %>%
      girafe_options(opts_hover(css = paste0("fill: ", msw_hover, ";")),
                     opts_tooltip(css = tooltip_css, opacity = 1),
                     opts_selection(type = "none"))
  })
  
  output$ui_header <- renderUI({
    data <- yearly$data
    h2(HTML(paste("Disposal rates in", data$Regional_District[1], "(kg per person)")))
  })
})