
library(xts)
library(forecast)

ts=read.csv('F:/yuce/citys.csv',header=T,encoding='UTF-8')



yuce<-function(n)
{
      ts2<-xts(ts[,n],seq(as.POSIXct("2016-01-01"),len=length(ts[,n]),by="day"))
      ts3<-ts2['2016-09-01/']
      data.fit <- arima(ts3['/2017-02-11'],order=c(7,1,0),seasonal=list(order=c(1,1,0), period=7))
      forecast <- forecast.Arima(data.fit,h=14,level=c(99.5))
      as.data.frame(forecast$mean[1:14])
}


x=as.data.frame(matrix(nrow=14,))
for(i in 2:ncol(ts)){
                     x[i-1]=yuce(i)
}
write.csv(x,'F:/x.csv')




ts2<-xts(ts[,2],seq(as.POSIXct("2016-01-01"),len=length(ts[,2]),by="day"))
ts3<-ts2['2016-09-01/']
data.fit <- arima(ts3['/2017-02-11'],order=c(7,1,0),seasonal=list(order=c(1,1,0), period=7))
forecast <- forecast.Arima(data.fit,h=14,level=c(99.5))
plot.forecast(forecast,main='未来14天预测图')
as.data.frame(forecast$mean[1:14])



