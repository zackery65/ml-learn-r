#ggplot2$time series
library(zoo)
library(ggplot2)

d=read.csv("F:/123.csv",header=T)
l=ts(d[,2:7])
s=as.Date("2016-07-06")
dates=seq(from=s,by=1,length.out=108)
df=zoo(l,dates)
plot(df)

#¿˚”√ggplot2
dt=fortify(df[,1],melt=T)
g=ggplot(aes(x=Index ,y=Value),data=dt)
g=g+geom_line(color="red")+geom_smooth()

rm(list=ls())
gc()