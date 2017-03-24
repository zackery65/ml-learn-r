
library(MASS)
library(lars)

data=read.csv('F:/regression.csv',header=T)

names(data)=c('dates','y','x1','x2','x3','x4')

dt=data[2:6]

summary(lm(y~.,data=dt)) 

#样本方阵的条件数(kappa值) ，可以判断变量间具有多重共线性。
#(k<100,说明共线性程度小,如果100< k< 1000,有较强的多重共线性,k>1000,在严重的多重共线）
kappa(cor(dt),exact=TRUE)

library(MASS)
lm.ridge=lm.ridge(y~.,dt,lambda=seq(0,0.1,0.001))
plot(lm.ridge(y~.,dt,lambda=seq(0,0.1,0.001))) 

lmRid=lm.ridge(y~.,dt,lambda=seq(0,0.1,0.001))
select(lmRid)

#去掉x4后，x3检查不过
lm=lm(y~.-x4,dt)
summary(lm)

#去掉x3
lm=lm(y~.-x3,dt)
summary(lm)

lmRid=lm.ridge(y~.-x3,dt,lambda=seq(0,0.1,0.001))
select(lmRid)

lmRid=lm.ridge(y~.,dt,lambda=0.034)
lmRid

lmRid=lm.ridge(y~.-x3,dt,lambda=0.034)
lmRid

#去掉x3,x4
lm=lm(y~.-x3-x4,dt)
summary(lm)

lmRid=lm.ridge(y~.-x3-x4,dt,lambda=seq(0,0.1,0.001))
select(lmRid)

lmRid=lm.ridge(y~.-x3-x4,dt,lambda=0.018)
lmRid

head(dt)

dtMat=as.matrix(dt)
lar =lars(dtMat[,2:5],dtMat[,1])

lar

summary(lar)

plot(lars(dtMat[, 2:5],dtMat[, 1]))

coef.lars(lar,mode="step",s=5)

lar$Cp[which.min(lar$Cp)]

predict(lar,data.frame(x1=0,x2=0,x3=0,x4=0),s=5)

head(dt)

n=nrow(dt)
set.seed(1234)
trainindex <- sample(1:n,0.7*n)
trainset <- dt[trainindex,]
testsed <- dt[-trainindex,]

lmRid=lm.ridge(y~.,dt,lambda=0.034)
lmRid

l=lm(y~.,data=dt)

l

#相关系数矩阵
cor(data[,2:6])

mt=cor(data[,2:6])
e=eigen(mt)#想关矩阵的特征值（PCA）

e

#标准化的主成分得分
scale(as.matrix(data[,2:6])) %*% e$vectors

plot(e$values,xlab='i',ylab='lambda',type="o",col=rgb(200,0,100,200,maxColorValue=255))


