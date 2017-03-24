library(data.table)
library(shiny)
library(sqldf)
library(leaflet)
library(rCharts)
library(dplyr)
library(rCharts)


function(input, output) {
  output$map <- renderLeaflet({
    df1 = as.data.frame(fread("F:/jingwei.csv",header=T))
    names(df1)=c("user_id","longitude","latitude")
    df1=sqldf("select longitude,latitude from df1 limit 1000")
    m<-leaflet(df1) %>%
      setView(lng =116.40, lat =39.95, zoom = 11) %>%
      addTiles('http://webrd02.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}',
               tileOptions(tileSize=256, minZoom=9, maxZoom=17),
               attribution = '&copy; <a href="http://ditu.amap.com/">高德地图</a>',) %>%
      addCircles()
    
    return(m)
    })
  
  output$plot <- renderChart2({
    df2<-read.csv("F:/leixing.csv")
    df2_0<-as.Date(b[,1])
    df2_1<-cbind(b0,b[,2:4])
    selectrow <- df2_1$b0 == input$date1
    df2<-df2_1[selectrow,]
    p <- rPlot(number~type,data=df2,type="bar")
    return(p)
  })  
}