select
from_unixtime(b.end_time,'yyyy-MM-dd HH') dates,
from_unixtime(b.end_time,'HH') hour,
round(b.start_latitude,2) lat,
round(b.start_longitude,2) lng,
count(*) cnt
from (
  select
  a.service_order_id,
  a.start_latitude,
  a.start_longitude,
  a.end_time
  from yc_bit.ods_service_order a
  where a.dt>=20160915
  and a.city='bj'
  ) as b
where 
b.end_time>=unix_timestamp('2016-09-15 00:00:00')
and b.end_time<=unix_timestamp('2017-03-26 00:00:00')
and start_latitude>=39.6
and start_latitude<=40.2
and start_longitude>=116.1
and start_longitude<=116.75
group by
from_unixtime(b.end_time,'yyyy-MM-dd HH'),
from_unixtime(b.end_time,'HH'),
round(b.start_latitude,2),
round(b.start_longitude,2)
order by
dates,
hour,
lat,
lng


select
from_unixtime(b.end_time,'yyyy-MM-dd HH') dates,
from_unixtime(b.end_time,'HH') hour,
count(*) cnt
from (
  select
  a.service_order_id,
  a.start_latitude,
  a.start_longitude,
  a.end_time
  from yc_bit.ods_service_order a
  where a.dt>=20160915
	and a.city='bj'
  ) as b
where 
b.end_time>=unix_timestamp('2016-09-15 00:00:00')
and start_latitude>=39.6
and start_latitude<=40.2
and start_longitude>=116.1
and start_longitude<=116.75
group by
from_unixtime(b.end_time,'yyyy-MM-dd HH'),
from_unixtime(b.end_time,'HH')
order by
dates,
hour



library(data.table)
library(sqldf)
library(leaflet)
library(rCharts)
library(xts)
library(forecast)
library(tseries)
library(prophet)

data=fread('F:/heatmap/order.csv',header=T)
total=fread('F:/heatmap/total.csv',header=T)
points=fread('F:/heatmap/points.csv',header=T)
points=sqldf('select 
          a.dates,
              a.hour,
              b.lat,
              b.lng,
              b.cnt
              from
             (
              select dates,hour from total
              ) as a  
              left join
              (
              select dates,lat,lng,cnt from data where lat="40.08" and lng="116.59"
              ) as b
              on a.dates = b.dates
            ')
names(points)=c('dates','hour','lat','lng','cnt')
points$cnt[is.na(points$cnt)] <- 0
points$lat[is.na(points$lat)] <- 40.08
points$lng[is.na(points$lng)] <- 116.59

#画图
leaflet()%>%
         setView(lng=116.38,lat=39.9,zoom=9)%>% 
         addTiles()%>% 
         addCircleMarkers(data=points,lng=points$lng,lat=points$lat,
                          color='red',weight = 1,opacity =0 ,fillOpacity = 0.35,
                          popup=paste('LAT',points$lat,'LNG',points$lng,'T',points$cnt),
                          radius=as.double(points$cnt)/8000)


h1 <- Highcharts$new()
h1$chart(type = "spline")
h1$series(data =as.double(points$cnt), dashStyle = "longdash")
h1$legend(symbolWidth = 80)
h1



#预测
ts<-xts(as.double(unlist(points[5])),seq(as.POSIXct("2016-09-15 00"),len=nrow(points),by="hour"))
ts1=ts['2017-02-10 00/']
plot.ts(ts1)
adf.test(ts1)
ts2=ts1['/2017-03-24 00']
data.fit <- arima(ts2,order=c(168,2,0),seasonal=list(order=c(1,2,0), period=168))
forecast <- forecast.Arima(data.fit,h=48,level=c(99.5))
forecast$mean

date<- seq.Date(from = as.Date("1970/01/01",format = "%Y/%m/%d"), by = "day", length.out =nrow(points))
ts=cbind(as.data.frame(date),as.double(unlist(points[5])))
names(ts)=c("ds","y")
m <- prophet(ts)
future <- make_future_dataframe(m, periods = 24)
tail(future)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast)
prophet_plot_components(m, forecast)
