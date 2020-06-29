require(lattice)
library(latticeExtra)
require(gridExtra)

zi<-seq(0,1, by=0.05)
zj<-seq(0,1, by=0.05)
df<-expand.grid(zi, zj)
colnames(df)<-c("zi", "zj")

Bn<-0.01
Bn2<-0
Bs<-0
Bs2<-0
Bsn<--0.023
df$wi<-1+Bn*df$zi + Bs*df$zj +  Bsn*df$zi*df$zj

SS<-wireframe(df$wi~df$zj*df$zi, xlab="z'", ylab=expression(z), zlab=expression(w),
          colorkey = FALSE, zlim=c(1+-0.01,1+0.018), xlim=c(0,1), ylim=c(0,1),
          screen = list(z = 15, x = -67), 
          scales = list( arrows = FALSE, col="black", cex.axis=0.8, z=list(
            at=seq(0.99,1.018,0.004),
            labels=rep("", length(seq(0.99,1.018,0.004))))), 
           aspect = c(1.1, 1),
          drape = FALSE,
          par.settings = list(axis.line = list(col = "transparent")), main="(A)")
          
n.years=300
n.sim=1
m<-matrix(NA,n.years+1, n.sim)
s<-wm<-matrix(NA, n.years, n.sim)
m[1,]<-0

for(j in 1:n.sim){
for(i in 1:n.years){
  zi<-rnorm(100, m[i], sqrt(1))
  wi<-1 +Bn*zi + Bsn*zi*m[i]
  m[i+1,j]<-m[i]+cov(zi,wi)
  wm[i,j]<-mean(wi)
  s[i,j]<-cov(zi,wi)
  }
}


m<-as.vector(m)
wm<-as.vector(wm)
col<-cx<-pc<-rep(NA, length(wm))
pc[]<-21
cx[]<-0.3
pc[length(wm)]<-8
cx[length(wm)]<-1.5

Bsn<--0.015
m2<-matrix(NA,n.years+1, n.sim)
wm2<-matrix(NA, n.years, n.sim)
m2[1,]<-0

for(j in 1:n.sim){
  for(i in 1:n.years){
    zi<-rnorm(100, m2[i], sqrt(1))
    wi<-1+Bn*zi + Bsn*zi*m2[i]
    m2[i+1,j]<-m2[i]+cov(zi,wi)
    wm2[i,j]<-mean(wi)
    s[i,j]<-cov(zi,wi)
  }
}


m2<-as.vector(m2)
wm2<-as.vector(wm2)

m3<-c(m,m2)

rbPal <- colorRampPalette(c('red','blue'))
col <- rbPal(20)[as.numeric(cut(m3,breaks = 20))]

SS2<-cloud(wm~m*m, xlab="z'", ylab=expression(z), zlab=expression(w),
           colorkey = FALSE,  zlim=c(1+-0.01,1+0.018), xlim=c(0,1), ylim=c(0,1),
           screen = list(z = 15, x = -67), 
           scales = list( arrows = FALSE, col="black", cex.axis=0.8, z=list(
             at=seq(0.99,1.018,0.004),
             labels=rep("", length(seq(0.99,1.018,0.004))))), 
           aspect = c(1.1, 1),
           drape = FALSE,
           par.settings = list(axis.line = list(col = "transparent")), pch=pc, col=col[1:n.years], cex=cx) 

SS+as.layer(SS2)

zi<-seq(0,1, by=0.05)
zj<-seq(0,1, by=0.05)
df<-expand.grid(zi, zj)
colnames(df)<-c("zi", "zj")

Bn<-0.01
Bn2<-0
Bs<-0
Bs2<-0
Bsn<--0.015
df$wi<-1+Bn*df$zi + Bs*df$zj +  Bsn*df$zi*df$zj


SS3<-wireframe(df$wi~df$zj*df$zi, xlab="z'", ylab=expression(z), zlab=expression(w),
              colorkey = FALSE, zlim=c(1+-0.01,1+0.018), xlim=c(0,1), ylim=c(0,1),
              screen = list(z = 15, x = -67), 
              scales = list( arrows = FALSE, col="black", cex.axis=0.8, z=list(
                at=seq(0.99,1.018,0.004),
                labels=rep("", length(seq(0.99,1.018,0.004))))),
              aspect = c(1.1, 1),
              drape = FALSE,
              par.settings = list(axis.line = list(col = "transparent")), main="(B)",layout.widths = list(ylab.axis.padding = 0.5))

col[length(col)]<-"red"
SS4<-cloud(wm2~m2*m2, xlab="z'", ylab=expression(z), zlab=expression(w),
           colorkey = FALSE,  zlim=c(1+-0.01,1+0.018), xlim=c(0,1), ylim=c(0,1),
           screen = list(z = 15, x = -67), 
           scales = list( arrows = FALSE, col="black", cex.axis=0.8, z=list(
             at=seq(0.99,1.018,0.004),
             labels=rep("", length(seq(0.99,1.018,0.004))))), 
           aspect = c(1.1, 1),
           drape = FALSE,
           par.settings = list(axis.line = list(col = "transparent")), pch=pc, col=col[(n.years+2):((n.years+2)*2)], cex=cx) 


grid.arrange(SS+as.layer(SS2), SS3+as.layer(SS4), ncol=2)
