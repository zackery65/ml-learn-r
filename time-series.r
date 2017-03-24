
library(xts)
library(forecast)
ts=read.csv('F:/data.csv',header=T,encoding='UTF-8')
ts4=read.csv('F:/data1.csv',header=T,encoding='UTF-8')
name=c('date','number')

names(ts)=name

ts

ts2<-xts(ts[,2],seq(as.POSIXct("2016-08-01"),len=length(ts[,2]),by="day"))

ts3<-ts2['2016-11-12/']

plot(ts2,main='订单量',sub='2016-08-01至今',xlab='时间',ylab='订单数')

ts2_diff1<-diff(ts2,differences=1)
plot(ts2_diff1,main='一阶差分图')

ts2_diff2<-diff(ts2,differences=2)
plot(ts2_diff2,main='二阶差分图')

ts2_diff1

acf <- acf(ts2_diff1[2:195],lag.max=100,plot=TRUE)

pacf <- pacf(ts2_diff1[2:195],lag.max=100,plot=TRUE)

data.fit <- arima(ts2,order=c(7,1,0),seasonal=list(order=c(1,1,0), period=7))

data.fit

forecast <- forecast.Arima(data.fit,h=7,level=c(99.5))

plot.forecast(forecast,main='未来7天预测图')

forecast

ts3

data.fit1 <- arima(ts3,order=c(8,1,0),seasonal=list(order=c(1,1,0), period=7))
forecast2 <- forecast.Arima(data.fit1,h=7,level=c(99.5))
forecast2

names(ts4)=name
ts4

ts5<-xts(ts4[1:775,2],seq(as.POSIXct("2015-01-01"),len=length(ts4[1:775,2]),by="day"))
ts5

plot(ts4)

data.fit2 <- arima(ts5,order=c(8,1,0),seasonal=list(order=c(1,1,0), period=7))
forecast3 <- forecast.Arima(data.fit2,h=7,level=c(99.5))
forecast3

ts6<-ts5['2016-06-08/']
data.fit3 <- arima(ts6,order=c(8,1,0),seasonal=list(order=c(1,1,0), period=7))
forecast4 <- forecast.Arima(data.fit3,h=7,level=c(99.5))
forecast4

tsdiag(data.fit)

index.fit<-auto.arima(ts3,trace=T)#自动获取参数，并拟合模型
index.fit#可查看模型拟合验证指标信息等

forecast<-forecast(index.fit,h=7,level=c(99.5))#设置预测期数h与置信度水平
forecast

forecast1 <- HoltWinters(ts2,gamma=FALSE) 

plot(forecast1)

forecasts1 <- forecast.HoltWinters(forecast1, h=7, level=c(99.5))  
plot.forecast(forecasts1)  

forecasts1 


