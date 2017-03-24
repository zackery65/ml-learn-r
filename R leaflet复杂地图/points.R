library(sqldf)
library(leaflet)


#处理数据
data=read.csv("F:/起止点频繁度信息.csv",header=T)
data_start=sqldf("select start_lat,start_lon,sum(amount) from data group by start_idx")
names(data_start)=c('latitude','longitude','amount')


data_line1=sqldf('select start_idx,start_lat as lat,start_lon as lon from data ')
data_line2=sqldf('select start_idx,end_lat as lat,end_lon as lon from data')
data_line=sqldf('select *  from data_line1 UNION ALL select *  from data_line2 order by start_idx')

#辅助函数
points2spline <- function(df, x_field, y_field, id_field){
  data <- as.matrix(df[,c(x_field, y_field)])
  id = df[1, id_field]
  Lines(list(Line(data)), ID=id)
} 

#将点处理为线
link_list <- split(data_line, data_line$start_idx)
names(link_list) <- NULL
Sl <- SpatialLines(plyr::llply(link_list, points2spline, "start_idx", "lon", "lat"))



m <- leaflet() %>%
  addTiles(
    'http://webrd02.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}',
    tileOptions(tileSize=256, minZoom=9, maxZoom=17)
  ) %>%
  addCircleMarkers(lng=~longitude, lat=~latitude, radius=12, data=data_start, stroke=FALSE, fill=TRUE, fillColor=rgb(max(data_start$amount)/3,data_start$amount,5000,maxColorValue=max(data_start$amount)), fillOpacity=0.5)%>%
  addPolylines(weight=0.8,opacity=1,data=Sl)
