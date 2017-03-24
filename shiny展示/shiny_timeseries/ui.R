library(shiny)
library(rCharts)


navbarPage("运营数据预测",
  theme = shinythemes::shinytheme("yeti"),
  
  tabPanel("预测图表",
    sidebarPanel( 
      sliderInput("slider","预测天数:",1,31,1)
    ),
  
    mainPanel(
      textOutput('tx', container=h4),
      h3('分城市数据'),
      showOutput('citys',lib="morris"),
      h3('分车型数据'),
      showOutput('car',lib="morris"),
      h3('用户和司机'),
      showOutput('user',lib="morris"),
      h3('GMV'),
      showOutput('gmv',lib="morris"),
      h3('客单价'),
      showOutput('price',lib="morris")
    
    )
  ),
  
  tabPanel("预测数据",
    tableOutput("view")
  )
)
