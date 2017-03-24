library(data.table)
library(arules)
library(arulesViz)

c=fread("F:/3.csv",header=F,encoding="UTF-8")

val1=sample(1:nrow(a),100000)
a0=as.data.frame(a[val1,])
val2=sample(1:nrow(b),200000)
b0=as.data.frame(b[val2,])
val3=sample(1:nrow(c),100000)
c0=as.data.frame(c[val3,])

names(c0)=c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","v14","V15")
d=rbind(a0,b0,c0)
write.csv(d,"F:/d.csv")


a=data.frame(read.csv("F:/d1.csv",header=T))
a1=as.matrix(a[,1]))
a2=as.matrix(cut(a[,2],breaks=c(0,200,300,42680),labels=c("200天以内","200到300","300以上")))
a3=as.matrix(cut(a[,3],breaks=c(0,200,260,17110),labels=c("200以内","200到260","260以上")))
a4=as.matrix(cut(a[,4],breaks=c(1,9,23,50,100,3454),labels=c("1到9","9到23","23到50","50到100","100以上")))
a5=as.matrix(cut(a[,5],breaks=c(0,4,9,20,100,1315),labels=c("0到4","4到9","9到20","20到100","100以上")))
a6=as.matrix(cut(a[,6],breaks=c(0,8,20,50,100,2629),labels=c("0到8","8到20","20到50","50到100","100以上")))
a7=as.matrix(cut(a[,7],breaks=c(0,0.75,0.9,1,1.1),labels=c("75%以下","75%到90%","90%以上","无单")))
a8=as.matrix(a[,8])
a9=as.matrix(a[,9])
a10=as.matrix(cut(a[,10],breaks=c(0,5,10,20,50,170),labels=c("5次以内","5到10次","10到20","20到50","50以上")))
a11=as.matrix(a[,11])
a12=as.matrix(cut(a[,12],breaks=c(0,10,20,87),labels=c("10次以内","10到20次","20次以上")))
a13=as.matrix(a[,13])
rs=cbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13)
write.csv(a13,"F:/jieguo/a13.csv")


c=read.transactions("F:/d3.csv",sep=",",rm.duplicates=TRUE)
frequentsets=eclat(c,parameter=list(support=0.5,maxlen=10))
summary(frequentsets)
inspect(frequentsets)
rules=apriori(c,parameter=list(supp=0.06,conf=0.25,minlen=2))
inspect(rules)
summary(rules)
ordered_y=sort(rules,by="lift")
inspect(ordered_y)
v1_rules=subset(rules,rhs %in% "用户类型_潜在忠诚","用户类型_忠诚用户","用户类型_潜在回流","用户类型_近期流失用户","用户类型_回流用户"
)
inspect(v1_rules)
#apriori算法

subset.matrix <- is.subset(v1_rules, v1_rules)
lower.tri(subset.matrix, diag = T)
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
subset.matrix
redundant <- colSums(subset.matrix, na.rm = T) >= 1
which(redundant)
rules.pruned <- v1_rules[!redundant]
inspect(rules.pruned)
z=as(rules.pruned ,"data.frame")
write.csv(z,"F:/f1.csv")
#去除冗余项

x1=subset(rules.pruned,rhs %in% "用户类型_近期流失用户") 
inspect(x1)

plot(rules.pruned)
plot(x1,main="回流用户与特征图",method="graph", control=list(type="items"))
plot(rules.pruned, main="分类用户与特征关系图",method="paracoord", control=list(reorder=TRUE))
#画图