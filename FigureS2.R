require(MASS)
require(ggplot2)
require(MVN)

res<-res2<-matrix(NA, 1000,2)

for(i in 1:1000){
ma<-matrix(0,3,3)
diag(ma)<-c(0.5,1,1)
ma[1,2]<-ma[2,1]<-0.5
ma[1,3]<-ma[3,1]<-ma[2,3]<-ma[3,2]<-0.25
m<-as.data.frame(mvrnorm(1000, c(0,0,0), ma))
res[i,1]<-mean((m[,1]-mean(m[,1]))*(m[,2]-mean(m[,2]))*(m[,3]-mean(m[,3])))
res2[i,1]<-mvn(m)$multivariateNormality$Result[1]

ma2<-matrix(0.5,3,3)
diag(ma2)[2]<-1
ma[1,2]<-ma[2,1]<-0.5
ma2[1,3]<-ma2[3,1]<-ma2[2,3]<-ma2[3,2]<-0.2

m2<-as.data.frame(mvrnorm(1000, c(0,0,0.1), ma2))
m2$zp<-m2[,1]*m2[,2] + rnorm(1000,0,sqrt(1-0.6^2))

res[i,2]<-mean((m2[,1]-mean(m2[,1]))*(m2[,2]-mean(m2[,2]))*(m2[,4]-mean(m2[,4])))
res2[i,1]<-mvn(m2[,c(1,2,4)])$multivariateNormality$Result[1]
  
}

apply(res,2,mean)
apply(res,2,sd)

m<-as.data.frame(mvrnorm(1000, c(0,0,0), ma))
m2<-as.data.frame(mvrnorm(1000, c(0,0,0.1), ma2))
m2$zp<-m2[,1]*m2[,2] + rnorm(1000,0,sqrt(1-0.6^2))

layout(matrix(c(1,2),1,2))
scatterplot3d::scatterplot3d(m, xlab="a", ylab="", zlab="z'", xlim=c(-3,3), ylim=c(-3,3), zlim=c(-6,6), main="(A)")
dims <- par("usr")
x <- dims[1]+ 0.9*diff(dims[1:2])
y <- dims[3]+ 0.08*diff(dims[3:4])
text(x,y,"z",srt=45)

scatterplot3d::scatterplot3d(m2[,c(1,2,4)], xlab="a", ylab="", zlab="z'", xlim=c(-3,3), ylim=c(-3,3), zlim=c(-6,6), main="(B)")
dims <- par("usr")
x <- dims[1]+ 0.9*diff(dims[1:2])
y <- dims[3]+ 0.08*diff(dims[3:4])
text(x,y,"z",srt=45)

