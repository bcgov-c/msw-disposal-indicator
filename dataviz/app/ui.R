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

shinyUI(
  fluidPage(
    includeCSS("www/style.css"),
    fluidRow(align = "center",
      div(style = div_css(p1.w, p1.h),
          conditionalPanel(condition = "output.plot_rd", 
            h2(HTML("2016 Regional District disposal rates (kg per person)")),
            div(class = "div-link", style = paste0("width:", translate_px(p1.w - 60), ";"),
                HTML(paste0("Click to sort by: ",
                            actionButton("sort_name", "Name", class = 'msw-button'),
                            " / ",
                            actionButton("sort_rate", "Disposal Rate", class = 'msw-button'),
                            " / ",
                            actionButton("sort_population", "Population", class = 'msw-button')
                )))
          ),
          girafeOutput(outputId = 'plot_rd'))
    ),
    fluidRow(align = "center",
             uiOutput("ui_info"),
               uiOutput("ui_resources", style = "display: inline-block;"),
               uiOutput("ui_dl", style = "display: inline-block;"),
             br(),
             div(style = paste("width:", translate_px(p2.w - 100), "; text-align: right;"),
             uiOutput("ui_show")),
             div(style = div_css(p2.w, p2.h),
                 girafeOutput(outputId = 'plot_year'))
    ))
)
  