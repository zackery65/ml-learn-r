library(data.table)
library(reshape2)
train=fread("D:/R语言/train.csv",header=T)
#读取数据  read data


val=sample(1:nrow(train),300)
train.1=as.data.frame(train[val,]) 
#取样本  a sample,in fact,you need to deal with all of the data in real circumstance

train.2=melt(train.1,id=c("uid","iid"))                            #melt输入的类型必须为data.frame(must data.frame type))
train.3=acast(train.2,iid~uid~variable,fill=0)
train.4=train.3[,,1]
write.csv(train.4, file = "d:/R语言/train.3.csv", row.names = T, quote = F)
#生成评分矩阵并输出(generate rank matrix)

#item_based CF
#生成相似度矩阵，在普通情况下，一种可行的方法是直接利用循环将两个物品的相似性放入矩阵（generate similarity matrix，one simple way is use loop）
A=fread("D:/R语言/train.4.csv",header=T)
sim=matrix(nrow=nrow(A),ncol=nrow(A))
for(i in 1:nrow(sim){
    for(j in 1:ncol(sim)){
        while(i<=j){
              sim[i,j]=1/(1+sqrt(sum((A[i,]-A[j,])^2)))
                    } 
                         }
                   } 
#欧几里得相似性 EcludSim
for(i in 1:nrow(sim){
    for(j in 1:ncol(sim)){
        while(i<=j){
              sim[i,j]=crossprod(A[i,],A[j,])/(crossprod(A[i,],A[i,])*crossprod(A[j,]，A[j,]))
                    } 
                         }
                   } 
#余弦相似性  cosSim
for(i in 1:nrow(sim){
    for(j in 1:ncol(sim)){
        while(i<=j){
              sim[i,j]=cor(A[i,],A[j,])
                    } 
                         }
                   } 
#皮尔逊相关性pearsSim{or sim=cov()/sum(sqrt(var())}

#实际上，R语言对循环的支持不是很好，下面用矩阵方式计算相似矩阵(in fact ,using loop in R is not a good choose,we need to re—calculate them by matrix way)
#利用cos函数的向量计算方法，可以很容易的计算出余弦相似性(we can easily calculate the cosSim by the vector algorithm of trigonometric function)
#cos theta=<a,b>/(|a||b|)

norm=function(x){1/sum(x^2)}      #向量的模
B=diag(apply(A,1,norm))           #生成模对角阵
cosSim=B%*%A%*%t(A)%*%B           #余弦相似矩阵

#EcludSim
#计算欧几里得相似性的关键是计算两个向量的距离。可以用余弦定理。推演后EcludSim=C+t(C)-2A%*%t(A),其中C是模按行并列的矩阵
#I use cosine law to calculate the distance of every two vector.you need some knowledge of linear algebra,and can get the algorithm.
C=matrix(apply(a,1,norm),nrow(A),nrow(A))  
elcudeDsit=C+t(C)-2A%*%t(A)       
ecludeSim=1/(1+ecludeDist)                                  #欧几里得相似阵


#pearsSim    I didn't get a good way to vectorlise the algorithm. if somebody see my code,I hope he/she can have a good idea.






#At last ,I can mark a good by the similarity matrix by knn algorithm or even weighting knn algorithm by a kernel.I think use passion distribute as a kernel is a 
nice choice. Maybe a good that someone bought recently is more representative. We can use package-KKNN to accomplish it#
