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

source("server_functions.R", local = TRUE)
source("server_objects.R", local = TRUE)

shinyServer(function(input, output, session) {

  yearly_data <- reactive({
    if(is.null(input$plots_selected)){
      return(indicator_summary)
    }
    indicator[which(indicator$Regional_District %in% input$plots_selected),]
  })
  
  # link_data <- reactive({
  #   link[which(link$Local_Govt_Name %in% input$plots_selected),]
  # })
  
  output$plots <- renderGirafe({
    girafe(code = print(gg_map(district) - gg_bar_rd(district) + plot_layout(ncol = 2,
                                                                              widths = c(3, 1))), 
            width_svg = translate_in(900), 
            height_svg = translate_in(550)) %>%
      girafe_options(opts_selection(type = "single"))
  })
  
  output$plot <- renderGirafe({
    data <- yearly_data()
    girafe(ggobj = gg_bar_year(data),
           width_svg = translate_in(900), 
           height_svg = translate_in(250)) 
  })
})