library(shiny)
library(xts)
library(forecast)
library(rCharts)
library(xtable)

#预测函数
yuce<-function(ts,d,od,ss)
{
  ts2<-xts(ts,seq(as.POSIXct("2016-09-01"),len=nrow(ts),by="day"))
  data.fit <- arima(ts2,order= od,seasonal= ss)
  forecast <- forecast.Arima(data.fit,h=d,level=c(99.5))
  as.data.frame(forecast$mean[1:d])
}


#生成预测数据
dt<-function(ts,d,od,ss)
{ 
  x=as.data.frame(matrix(nrow=d,ncol=(ncol(ts)-1)))
  for(i in 2:ncol(ts)){
    ts0 = ts[i]           
    x[i-1] = yuce(ts0,d,od,ss)
  }
  names(x)=names(as.data.frame(ts[2:ncol(ts)]))
  rbind(as.data.frame(ts[2:ncol(ts)]),x)     
} 


#画图
plt<-function(a,c)
{
  ts2<-xts(a[,1:ncol(a)],seq(as.POSIXct("2016-09-01"),len=length(a[,1]),by="day"))
  ts3<-ts2['2017-01-01/']
  df <- transform(as.data.frame(ts3), date = as.character(time(ts3)))
  m1 <- mPlot(x = 'date', y = names(df[1:ncol(df)-1]), type = 'Line', data = df)
  m1$set(pointSize = 2, lineWidth = 1,labels=c)
  m1
}

#返回星期六
saturday<-function(){
  a<-Sys.Date()
  if (length(grep('星期一',weekdays(a)))
  ){a<- a-2}
  else if (length(grep('星期二',weekdays(a)))
  ){a<- a-3}
  else if (length(grep('星期三',weekdays(a)))
  ){a<- a-4}
  else if (length(grep('星期四',weekdays(a)))
  ){a<- a-5}
  else if (length(grep('星期五',weekdays(a)))
  ){a<- a-6}
  else if (length(grep('星期六',weekdays(a)))
  ){a<- a}
  else{a<- a-1}
  return (a)
}

paste('F:/forecast/data/car_',format(saturday(), "%Y%m%d"),'.csv',sep="")

total=read.csv(paste('F:/forecast/data/total_',format(saturday(), "%Y%m%d"),'.csv',sep=""),header=F,sep='\t',encoding='UTF-8',na.string = "NULL")
citys=read.csv(paste('F:/forecast/data/cities_',format(saturday(), "%Y%m%d"),'.csv',sep=""),header=F,sep='\t',encoding='UTF-8',na.string = "NULL")
car=read.csv(paste('F:/forecast/data/car_',format(saturday(), "%Y%m%d"),'.csv',sep=""),header=F,sep='\t',encoding='UTF-8',na.string = "NULL")
user=read.csv(paste('F:/forecast/data/user_',format(saturday(), "%Y%m%d"),'.csv',sep=""),header=F,sep='\t',encoding='UTF-8',na.string = "NULL")
gmv=read.csv(paste('F:/forecast/data/gmv_',format(saturday(), "%Y%m%d"),'.csv',sep=""),header=F,sep='\t',encoding='UTF-8',na.string = "NULL")
price=read.csv(paste('F:/forecast/data/price_',format(saturday(), "%Y%m%d"),'.csv',sep=""),header=F,sep='\t',encoding='UTF-8',na.string = "NULL")


function(input, output, session) {
  
  total1=reactive({dt(total,input$slider,c(7,1,1),list(order=c(1,1,0), period=7))})
  citys1=reactive({dt(citys,input$slider,c(7,1,0),list(order=c(1,1,0), period=7))})
  car1  =reactive({dt(car,input$slider,c(7,1,0),list(order=c(1,1,0), period=7))})
  user1 =reactive({dt(user,input$slider,c(7,1,0),list(order=c(1,1,0), period=7))})
  gmv1  =reactive({dt(gmv,input$slider,c(7,1,0),list(order=c(1,1,0), period=7))})
  price1=reactive({dt(price,input$slider,c(7,1,0),list(order=c(1,1,0), period=7))})
  
  output$tx <- renderText({
    c('预测开始时间:',as.character(saturday()))
  })
  
  output$citys <- renderChart2({
    a1=cbind(total1(),citys1())
    plt(a1,c('完成订单量','北京','广州','上海','深圳'))
  })    
  
  output$car <- renderChart2({
    a2=cbind(total1(),car1())
    plt(a2,c('完成订单量','young','舒适'))
  })
  
  output$user <- renderChart2({
    plt(user1(),c('司机激活数量','用户激活数量'))
  })
  output$gmv <- renderChart2({
    plt(gmv1(),c('GMV','服务成本'))
  })
  
  output$price <- renderChart2({
    plt(price1(),c(' '))
  })
  
  output$view <- renderTable({
    d=as.character(saturday())
    date=as.data.frame(as.character(seq(as.POSIXct(d),len=input$slider,by="day")))
    tb=cbind(total1(),gmv1(),price1(),citys1(),car1(),user1())
    tb=cbind(date,tb[(nrow(tb)- input$slider +1):nrow(tb),])
    names(tb)=c('日期','完成订单量','GMV','服务成本','客单价','北京','广州','上海','深圳','young','舒适','司机激活数量','用户激活数量')
    xtable(tb)
  })
  
}
