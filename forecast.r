library(xts)
library(forecast)

flag=data.frame(flag=c(1,2,3,4,5,6,7))
#预测函数
yuce<-function(ts,d,od,ss)
{
  ts=ts[2]
  ts2<-xts(ts,seq(as.POSIXct("2016-09-01"),len=nrow(ts),by="day"))
  data.fit <- arima(ts2,order= od,seasonal= ss)
  forecast <- forecast.Arima(data.fit,h=d,level=c(65))
  dt=as.data.frame(forecast)
}

:
total=read.csv('F:/data/total.csv',header=T,sep=',',encoding='UTF-8',na.string = "NULL")
total_ct=read.csv('F:/data/total_ct.csv',header=T,sep=',',encoding='UTF-8',na.string = "NULL")
driver=read.csv('F:/data/driver.csv',header=T,sep=',',encoding='UTF-8',na.string = "NULL")
driver_avg=read.csv('F:/data/driver_avg.csv',header=T,sep=',',encoding='UTF-8',na.string = "NULL")
user=read.csv('F:/data/user.csv',header=T,sep=',',encoding='UTF-8',na.string = "NULL")
user_avg=read.csv('F:/data/user_avg.csv',header=T,sep=',',encoding='UTF-8',na.string = "NULL")
price=read.csv('F:/data/price.csv',header=T,sep=',',encoding='UTF-8',na.string = "NULL")
gmv=read.csv('F:/data/gmv.csv',header=T,sep=',',encoding='UTF-8',na.string = "NULL")


dt_total=yuce(total,7,c(7,1,0),list(order=c(1,1,0), period=7))
names(dt_total)=c('forecast_total','low_total','high_total')

dt_total_ct=yuce(total_ct,7,c(7,1,0),list(order=c(1,1,0), period=7))
names(dt_total_ct)=c('forecast_total_ct','low_total_ct','high_total_ct')

dt_driver=yuce(driver,7,c(7,1,0),list(order=c(1,1,0), period=7))
names(dt_driver)=c('forecast_driver','low_driver','high_driver')

dt_driver_avg=yuce(driver_avg,7,c(7,1,0),list(order=c(1,1,0), period=7))
names(dt_driver_avg)=c('forecast_driver_avg','low_driver_avg','high_driver_avg')

dt_user=yuce(user,7,c(7,1,0),list(order=c(1,1,0), period=7))
names(dt_user)=c('forecast_user','low_user','high_user')

dt_user_avg=yuce(user_avg,7,c(7,1,0),list(order=c(1,1,0), period=7))
names(dt_user_avg)=c('forecast_user_avg','low_user_avg','high_user_avg')

dt_price=yuce(price,7,c(7,1,0),list(order=c(1,1,0), period=7))
names(dt_price)=c('forecast_price','low_price','high_price')

dt_gmv=yuce(gmv,7,c(7,1,0),list(order=c(1,1,0), period=7))
names(dt_gmv)=c('forecast_gmv','low_gmv','high_gmv')

dt=cbind(flag,dt_total[1],dt_total_ct[1],dt_driver[1],dt_driver_avg[1],dt_user[1],dt_user_avg[1],dt_price[1],dt_gmv[1])

write.csv(dt,'F:/result.csv',row.names=FALSE)

