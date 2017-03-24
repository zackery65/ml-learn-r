library(shiny)
library(leaflet)
library(rCharts)

 navbarPage("运行监控",
                  theme = shinythemes::shinytheme("united"),
                  tabPanel("司机",
                           fluidPage(
                             sidebarPanel(
                               textInput("txt1", "城市:", "北京"),
                               numericInput("obs1", "月份:",12),
                               sliderInput("slider","天:",1,31,1),
                               dateInput("date1",label = h4("天:"),value = Sys.Date() - 11,width = 200)
                                         ),
                             mainPanel(
                               tabsetPanel(
                                 tabPanel("运行情况",
                                          showOutput('plot',lib='polycharts')
                                          ),
                                 tabPanel("地图展示",
                                          leafletOutput("map")
                                          )
                                          )
                                     )
                                  )
                        ),
                 
                  navbarMenu("乘客",
                             tabPanel("个人用户", 
                                      fluidPage(
                                        sidebarPanel(
                                          textInput("txt2", "城市:", "北京"),
                                          numericInput("obs2","月份:",12)
                                                    ),
                                        mainPanel(
                                          tabsetPanel(
                                            tabPanel("运行情况"),
                                            tabPanel("地图展示")
                                                     )
                                                 )
                                             )
                                      ),
                             tabPanel("企业用户", 
                                      fluidPage(
                                        sidebarPanel(
                                          textInput("txt3", "城市:", "北京"),
                                          numericInput("obs3","月份:",12)
                                        ),
                                        mainPanel(
                                          tabsetPanel(
                                            tabPanel("运行情况")
                                          )
                                        )
                                      )
                                      )
                  )              
  )

