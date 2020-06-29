#Simulation for figure 3 and figure S1.

##Function to simulate data
sim<-function(n.g,n.ind,BN,BS,BI,Va,Ve,Vew,Vap,Vep,psi,pisp,m1,mp1,Vr,r1)
{
dl<-list()
m<-matrix(NA, n.g, 2)
b<-matrix(NA, n.g, 2)
b2<-matrix(NA, n.g, 3)
w<-rep(0,n.g)
m[1,]<-c(m1, mp1)
b[1,]<-c(BN,BS)

r.hat<-rep(0,n.g)
zz<-rep(0,n.g)
r.hat[1]<-r1

for(i in 1:(n.g-1)){
  a<-rnorm(n.ind, m[i,1], sqrt(Va))
  r<-rnorm(n.ind, r.hat[i], sqrt(Vr))
  ap<-r*a + rnorm(n.ind, m[i,1], sqrt(Vap-r.hat[i]^2))
  d<-data.frame(a=a, ap=ap, r=r)
  d$z<-d$a + rnorm(n.ind, 0, sqrt(Ve))
  d$zp<-d$ap + rnorm(n.ind, 0, sqrt(Vep))
  mw<-BN*m[i,1] + BS*m[i,2] + BI*m[i,1]*m[i,2]
  d$zz<-d$z*d$zp
  d$w<-(1-mw)+ d$z*BN + d$zp*BS + d$zz*BI + rnorm(n.ind, 0, sqrt(Ve)) 
  d$rw<-d$w/mean(d$w)
  
  dl[[i]]<-d
  m[i+1,1]<-m[i,1] + cov(d$a,d$w)/mean(d$w)
  m[i+1,2]<-m[i,1] + cov(d$a,d$w)/mean(d$w)
  b[i,]<-coef(lm(rw~z + zp, data=d))[2:3]
  b2[i,]<-coef(lm(rw~z * zp, data=d))[2:4]
  r.hat[i+1]<-r.hat[i]+ cov(d$r,d$w)
  zz[i]<-cov(d$a, d$zz)
  w[i]<-mean(d$w)
  
  }

res<-list(d=dl, m=m,b=b,b2=b2,r.hat=r.hat, zz=zz, w=w)
res
}

##Simulations for figure 3.
n.g=200      ###Number of generations
n.ind=150000 ###Number of individuals
BN=0.01      ###Direct selection gradient
BS= 0        ###Social selection gradient
BI= -0.15    ###Interactive fitness effects
Va<-1        ###Genetic variance in focal trait
Ve<-0        ###Environmental variance in focal trait   
Vew<-0.1     ###Environmental variance in fitness
Vap<-1       ###Genetic variance in partners trait
Vep<-1       ###Environmental variance in partners trait
psi<-0       ###Social responsivness
pisp<-0      ###Social impact
Vr=0         ###Variance in relatedness
r1=0         ###Relatedness of the first generation
m1=0         ###Mean focal phenotype of the first generation
mp1=0        ###Mean social phenotype of the first generation


res<-sim(n.g,n.ind,BN,BS,BI,Va,Ve,Vew,Vap,Vep,psi,pisp,m1,mp1,Vr,r1)
  

BN=0.01
BS= 0
BI= -0.15
res2<-sim(n.g,n.ind,BN,BS,BI,Va,Ve,Vew,Vap,Vep,psi,pisp,m1,mp1,Vr,r1)

jpeg("/home/yi/Dropbox/Social evolution/Figures/hawkdove.jpg", res=300, width=8.27, height=4.5, units="in")
par(mfrow=c(1,2), mar=c(5,6,2,2))

plot(res$m[-1,1], ylab="", xlab="Generations", type="l", yaxt="n", cex.lab=1.4, cex.axis=1.3)
abline(h=-0.01/-0.15,lty=2)
axis(2, seq(0,0.1,0.01), seq(0,0.1,0.01), las=2, cex.axis=1.3)
points(res2$m[,1], type="l", col="red")
abline(h=-0.01/-0.2,lty=2, col="red")
mtext("Average aggression",2,4, cex=1.4)
mtext("(A)",3,line=0.5, at=10, cex=1.4)

plot(res$b[-1,1], ylab="", xlab="Generations", type="l", yaxt="n", cex.lab=1.4, cex.axis=1.3)
axis(2, seq(-0.004,0.012,0.002), seq(-0.004,0.012,0.002), las=2, cex.axis=1.3)
abline(h=0, lty=2)
points(res2$b[,1], type="l", col="red")
mtext("Selection diferential",2,4, cex=1.4)
mtext("(B)",3,line=0.5, at=10, cex=1.4)
dev.off()


##Simulations for figure S1.
n.g=100
n.ind=20000
BN=-0.001
BS= 0.05
BI= 0.0
Vr=0
res3<-sim(n.g,n.ind,BN,BS,BI,Va,Ve,Vew,Vap,Vep,psi,pisp,m1,mp1,Vr,r1)

BN=-0.001
BS= 0.05
BI= 0.0
Vr=0
r1=0.05
res4<-sim(n.g,n.ind,BN,BS,BI,Va,Ve,Vew,Vap,Vep,psi,pisp,m1,mp1,Vr,r1)

BN=-0.001
BS= 0.05
BI= 0.01
r1=0
Vr=0.1
res5<-sim(n.g,n.ind,BN,BS,BI,Va,Ve,Vew,Vap,Vep,psi,pisp,m1,mp1,Vr,r1)

jpeg("//home/yi/Dropbox/Social evolution/Submssion last!/Figures/CooperationSexualOrnament.jpg", res=300, width=8.27, height=4.5, units="in")
par(mfrow=c(1,2), mar=c(5,6,2,2))

plot(res3$m[,1], ylab="", xlab="Generations", type="l", yaxt="n", cex.lab=1.4, 
cex.axis=1.3, ylim=c(-0.2,0.20))
axis(2, seq(-0.2,0.2,0.1), seq(-0.2,0.2,0.1), las=2, cex.axis=1.3)
points(res4$m[,1], type="l", col="red")
points(res5$m[,1], type="l", col="dodgerblue")
mtext("Social trait",2,4, cex=1.4)
mtext("(A)",3,line=0.5, at=10, cex=1.4)

plot(res3$r.hat, ylab="", xlab="Generations", type="l", yaxt="n", cex.lab=1.4,cex.axis=1.3, ylim=c(0,0.15))
axis(2, seq(0,0.5,0.05), seq(0,0.5,0.05), las=2, cex.axis=1.3)
points(res4$r.hat, type="l", col="red")
points(res5$r.hat, type="l", col="dodgerblue")
mtext(bquote(bar(r)~"or"~bar("ψ")),2,4, cex=1.4)
mtext("(B)",3,line=0.5, at=10, cex=1.4)
dev.off()





