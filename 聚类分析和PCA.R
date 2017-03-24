library(data.table)
library(reshape2)
library(sqldf)
library(densityClust)
library(clustMD)

d0=as.data.frame(fread("F:/用户分析.csv",header=T,encoding="UTF-8"))
str(d0)
head(d0,3)

#相关性分析
plot(d0[,"月均订单数"],d0[,"月均消费金额"],ylab="月均订单数",xlab="月均消费金额",col=rgb(0,0,100,100,maxColorValue=255))
plot(d0[,2:21],pch=19,col=rgb(0,0,100,50,maxColorValue=255))
a0=cor(d0[,2:21])
a1=as.data.frame(a0)
write.csv(a1,"F:/相关系数.csv")
a2=read.csv("F:/相关系数.csv",header=T)
a3=melt(a2)
a4=sqldf("select * from a3 where value>=0.1 and value<=-0.1")

#数据标准化
fk=function(x){scale(x,center=F,scale=T)}
b0=apply(d0[,2:21],2,fk)
head(b0)
d1=cbind(d0[,1],b0,d0[,22:24])
names(d1)[1]="用户ID"
head(d1)
write.csv(d1,"F:/标准化.csv")

rm(list=ls())
gc()

#聚类分析

#PCA
dt=as.data.frame(fread("F:/标准化.csv",header=T))
val=sample(1:nrow(dt),500)
dt0=as.data.frame(dt[val,])
c0=cor(dt[,2:21])
c1=eigen(c0) #计算特征值
plot(c1$values,xlab='i',ylab='lambda',type="o",col=rgb(200,0,100,200,maxColorValue=255))
dt1=prcomp(dt[,2:21])

pt=fread("F:/用户分析.csv",header=T)
val=sample(1:nrow(pt),14000)
pt0=as.data.frame(pt[val,])
pt1=prcomp(dt[,2:21])

#densityClust聚类
dtDist=dist(pt0[,2:21])
dtClust=densityClust(dtDist,gaussian=TRUE)
plot(dtClust)



#kmeans聚类
library(fpc)
library(data.table)
library(reshape2)
library(sqldf)
library(densityClust)
library(clustMD)

d0=as.data.frame(fread("F:/data.csv",header=T))
fk=function(x){scale(x,center=F,scale=T)}
d1=apply(d0[,2:5],2,fk)
d2=cbind(d0[,1],d1)
kmeans=kmeans(d2[,2:5],4)
d3=cbind(kmeans$cluster,d0)
write.csv(d3,'F:/kmeans.csv')

