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
    fluidRow(
      div(style = div_css(p1.w, p1.h),
          h2(HTML("2016 Regional District disposal rates (kg per person)")),
          div(class = "div-link",
              HTML(paste0("Click to sort by: ",
                          actionLink("sort_name", "Name", class = 'msw-link'),
                          " / ",
                          actionLink("sort_rate", "Disposal Rate", class = 'msw-link'),
                          " / ",
                          actionLink("sort_population", "Population", class = 'msw-link')
              ))
          ),
          girafeOutput(outputId = 'plot_rd'))
    ),
    fluidRow(
    div(style = div_css(p2.w, p2.h),
                 uiOutput('ui_header'),
                 div(class = "div-link",
                 # actionLink(inputId = "show_bc", "Show British Columbia", class = 'msw-link')),
                 girafeOutput(outputId = 'plot_year')),
             div(style= div_css(p3.w, p3.h, ""),
                 br(),
                 br(),
                 h2("Download reports"),
                 uiOutput("ui_dl")
             )
      ) 
    ))
)
