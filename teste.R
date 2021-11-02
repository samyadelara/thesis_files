library(foreign)
library(stats)
library(MASS)
library(nlme)
library(splines)
library(mgcv)
library(survival)
library(spam)
library(fields)
library(RColorBrewer)
library(latticeExtra)
library(pastecs)
library(zoo)
library(mvtnorm)
library(msm)
library(sfsmisc)
library(polycor)
library(ltm)
library(Formula)
library(Hmisc)
library(colorspace)
library(vcd)
library(epicalc)
library(plotrix)

#save.image("/Users/samyadelara/Desktop/Thesis/dados&analises/teste.RData")
load("/Users/samyadelara/Desktop/Thesis/dados&analises/teste.RData")


setwd("/Users/samyadelara/Desktop/Thesis/dados&analises/dados")

cardio<-read.csv("cardiodata.csv",sep=",",na.strings="NA")
respira<-read.csv("respiradata.csv",sep=",",na.strings="NA")

names(cardio)
head(cardio)
names(respira)
head(respira)

#new variables
#*match by temp*#
cardio$mattemp1<-round(cardio$tmed)-1
cardio$mattemp2<-round(cardio$tmed)+1
respira$mattemp1<-round(respira$tmed)-1
respira$mattemp2<-round(respira$tmed)+1

#*match by pollu*#

cardio$matmp10_1<-round(cardio$mp10_av_lag0)-1
cardio$matmp10_2<-round(cardio$mp10_av_lag0)+1
cardio$matno2_1<-round(cardio$no2_av_lag0)-1
cardio$matno2_2<-round(cardio$no2_av_lag0)+1
cardio$mato3_1<-round(cardio$o3_av_lag0mov2)-1
cardio$mato3_2<-round(cardio$o3_av_lag0mov2)+1

respira$matmp10_1<-round(respira$mp10_av_lag0)-1
respira$matmp10_2<-round(respira$mp10_av_lag0)+1
respira$matno2_1<-round(respira$no2_av_lag0)-1
respira$matno2_2<-round(respira$no2_av_lag0)+1
respira$mato3_1<-round(respira$o3_av_lag5)-1
respira$mato3_2<-round(respira$o3_av_lag5)+1

#$moving averages --> cardio$#
cardio$tmaxmov2<-c(NA,rollapply(cardio$tmax,2,function(x) median(x, na.rm = TRUE)))
cardio$tmaxmov3<-c(NA,NA,rollapply(cardio$tmax,3, function(x) median(x, na.rm = TRUE)))
cardio$tmaxmov4<-c(NA,NA,NA,rollapply(cardio$tmax,4, function(x) median(x, na.rm = TRUE)))
cardio$tmaxmov5<-c(NA,NA,NA,NA,rollapply(cardio$tmax,5, function(x) median(x, na.rm = TRUE)))
cardio$tmaxmov6<-c(NA,NA,NA,NA,NA,rollapply(cardio$tmax,6, function(x) median(x, na.rm = TRUE)))
cardio$tmaxmov7<-c(NA,NA,NA,NA,NA,NA,rollapply(cardio$tmax,7, function(x) median(x, na.rm = TRUE)))

cardio$tminmov2<-c(NA,rollapply(cardio$tmin,2, function(x) median(x, na.rm = TRUE)))
cardio$tminmov3<-c(NA,NA,rollapply(cardio$tmin,3, function(x) median(x, na.rm = TRUE)))
cardio$tminmov4<-c(NA,NA,NA,rollapply(cardio$tmin,4, function(x) median(x, na.rm = TRUE)))
cardio$tminmov5<-c(NA,NA,NA,NA,rollapply(cardio$tmin,5, function(x) median(x, na.rm = TRUE)))
cardio$tminmov6<-c(NA,NA,NA,NA,NA,rollapply(cardio$tmin,6, function(x) median(x, na.rm = TRUE)))
cardio$tminmov7<-c(NA,NA,NA,NA,NA,NA,rollapply(cardio$tmin,7, function(x) median(x, na.rm = TRUE)))

cardio$tmedmov2<-c(NA,rollapply(cardio$tmed,2, function(x) median(x, na.rm = TRUE)))
cardio$tmedmov3<-c(NA,NA,rollapply(cardio$tmed,3, function(x) median(x, na.rm = TRUE)))
cardio$tmedmov4<-c(NA,NA,NA,rollapply(cardio$tmed,4, function(x) median(x, na.rm = TRUE)))
cardio$tmedmov5<-c(NA,NA,NA,NA,rollapply(cardio$tmed,5, function(x) median(x, na.rm = TRUE)))
cardio$tmedmov6<-c(NA,NA,NA,NA,NA,rollapply(cardio$tmed,6, function(x) median(x, na.rm = TRUE)))
cardio$tmedmov7<-c(NA,NA,NA,NA,NA,NA,rollapply(cardio$tmed,7, function(x) median(x, na.rm = TRUE)))

cardio$Urnoonmov2<-c(NA,rollapply(cardio$Urnoon,2, function(x) median(x, na.rm = TRUE)))
cardio$Urnoonmov3<-c(NA,NA,rollapply(cardio$Urnoon,3, function(x) median(x, na.rm = TRUE)))
cardio$Urnoonmov4<-c(NA,NA,NA,rollapply(cardio$Urnoon,4, function(x) median(x, na.rm = TRUE)))
cardio$Urnoonmov5<-c(NA,NA,NA,NA,rollapply(cardio$Urnoon,5, function(x) median(x, na.rm = TRUE)))
cardio$Urnoonmov6<-c(NA,NA,NA,NA,NA,rollapply(cardio$Urnoon,6, function(x) median(x, na.rm = TRUE)))
cardio$Urnoonmov7<-c(NA,NA,NA,NA,NA,NA,rollapply(cardio$Urnoon,7, function(x) median(x, na.rm = TRUE)))

cardio$Urminmov2<-c(NA,rollapply(cardio$Urmin,2, function(x) median(x, na.rm = TRUE)))
cardio$Urminmov3<-c(NA,NA,rollapply(cardio$Urmin,3, function(x) median(x, na.rm = TRUE)))
cardio$Urminmov4<-c(NA,NA,NA,rollapply(cardio$Urmin,4, function(x) median(x, na.rm = TRUE)))
cardio$Urminmov5<-c(NA,NA,NA,NA,rollapply(cardio$Urmin,5, function(x) median(x, na.rm = TRUE)))
cardio$Urminmov6<-c(NA,NA,NA,NA,NA,rollapply(cardio$Urmin,6, function(x) median(x, na.rm = TRUE)))
cardio$Urminmov7<-c(NA,NA,NA,NA,NA,NA,rollapply(cardio$Urmin,7, function(x) median(x, na.rm = TRUE)))

cardio$mp10_av_lag0mov2<-c(NA,rollapply(cardio$mp10_av_lag0,2, function(x) median(x, na.rm = TRUE)))
cardio$mp10_av_lag0mov3<-c(NA,NA,rollapply(cardio$mp10_av_lag0,3, function(x) median(x, na.rm = TRUE)))
cardio$mp10_av_lag0mov4<-c(NA,NA,NA,rollapply(cardio$mp10_av_lag0,4, function(x) median(x, na.rm = TRUE)))
cardio$mp10_av_lag0mov5<-c(NA,NA,NA,NA,rollapply(cardio$mp10_av_lag0,5, function(x) median(x, na.rm = TRUE)))
cardio$mp10_av_lag0mov6<-c(NA,NA,NA,NA,NA,rollapply(cardio$mp10_av_lag0,6, function(x) median(x, na.rm = TRUE)))
cardio$mp10_av_lag0mov7<-c(NA,NA,NA,NA,NA,NA,rollapply(cardio$mp10_av_lag0,7, function(x) median(x, na.rm = TRUE)))

cardio$no2_av_lag0mov2<-c(NA,rollapply(cardio$no2_av_lag0,2, function(x) median(x, na.rm = TRUE)))
cardio$no2_av_lag0mov3<-c(NA,NA,rollapply(cardio$no2_av_lag0,3, function(x) median(x, na.rm = TRUE)))
cardio$no2_av_lag0mov4<-c(NA,NA,NA,rollapply(cardio$no2_av_lag0,4, function(x) median(x, na.rm = TRUE)))
cardio$no2_av_lag0mov5<-c(NA,NA,NA,NA,rollapply(cardio$no2_av_lag0,5, function(x) median(x, na.rm = TRUE)))
cardio$no2_av_lag0mov6<-c(NA,NA,NA,NA,NA,rollapply(cardio$no2_av_lag0,6, function(x) median(x, na.rm = TRUE)))
cardio$no2_av_lag0mov7<-c(NA,NA,NA,NA,NA,NA,rollapply(cardio$no2_av_lag0,7, function(x) median(x, na.rm = TRUE)))

cardio$o3_av_lag0mov2<-c(NA,rollapply(cardio$o3_av_lag0,2, function(x) median(x, na.rm = TRUE)))
cardio$o3_av_lag0mov3<-c(NA,NA,rollapply(cardio$o3_av_lag0,3, function(x) median(x, na.rm = TRUE)))
cardio$o3_av_lag0mov4<-c(NA,NA,NA,rollapply(cardio$o3_av_lag0,4, function(x) median(x, na.rm = TRUE)))
cardio$o3_av_lag0mov5<-c(NA,NA,NA,NA,rollapply(cardio$o3_av_lag0,5, function(x) median(x, na.rm = TRUE)))
cardio$o3_av_lag0mov6<-c(NA,NA,NA,NA,NA,rollapply(cardio$o3_av_lag0,6, function(x) median(x, na.rm = TRUE)))
cardio$o3_av_lag0mov7<-c(NA,NA,NA,NA,NA,NA,rollapply(cardio$o3_av_lag0,7, function(x) median(x, na.rm = TRUE)))

#$moving averages --> respira$#
respira$tmaxmov2<-c(NA,rollapply(respira$tmax,2, function(x) median(x, na.rm = TRUE)))
respira$tmaxmov3<-c(NA,NA,rollapply(respira$tmax,3, function(x) median(x, na.rm = TRUE)))
respira$tmaxmov4<-c(NA,NA,NA,rollapply(respira$tmax,4, function(x) median(x, na.rm = TRUE)))
respira$tmaxmov5<-c(NA,NA,NA,NA,rollapply(respira$tmax,5, function(x) median(x, na.rm = TRUE)))
respira$tmaxmov6<-c(NA,NA,NA,NA,NA,rollapply(respira$tmax,6, function(x) median(x, na.rm = TRUE)))
respira$tmaxmov7<-c(NA,NA,NA,NA,NA,NA,rollapply(respira$tmax,7, function(x) median(x, na.rm = TRUE)))

respira$tminmov2<-c(NA,rollapply(respira$tmin,2, function(x) median(x, na.rm = TRUE)))
respira$tminmov3<-c(NA,NA,rollapply(respira$tmin,3, function(x) median(x, na.rm = TRUE)))
respira$tminmov4<-c(NA,NA,NA,rollapply(respira$tmin,4, function(x) median(x, na.rm = TRUE)))
respira$tminmov5<-c(NA,NA,NA,NA,rollapply(respira$tmin,5, function(x) median(x, na.rm = TRUE)))
respira$tminmov6<-c(NA,NA,NA,NA,NA,rollapply(respira$tmin,6, function(x) median(x, na.rm = TRUE)))
respira$tminmov7<-c(NA,NA,NA,NA,NA,NA,rollapply(respira$tmin,7, function(x) median(x, na.rm = TRUE)))

respira$tmedmov2<-c(NA,rollapply(respira$tmed,2, function(x) median(x, na.rm = TRUE)))
respira$tmedmov3<-c(NA,NA,rollapply(respira$tmed,3, function(x) median(x, na.rm = TRUE)))
respira$tmedmov4<-c(NA,NA,NA,rollapply(respira$tmed,4, function(x) median(x, na.rm = TRUE)))
respira$tmedmov5<-c(NA,NA,NA,NA,rollapply(respira$tmed,5, function(x) median(x, na.rm = TRUE)))
respira$tmedmov6<-c(NA,NA,NA,NA,NA,rollapply(respira$tmed,6, function(x) median(x, na.rm = TRUE)))
respira$tmedmov7<-c(NA,NA,NA,NA,NA,NA,rollapply(respira$tmed,7, function(x) median(x, na.rm = TRUE)))

respira$Urnoonmov2<-c(NA,rollapply(respira$Urnoon,2, function(x) median(x, na.rm = TRUE)))
respira$Urnoonmov3<-c(NA,NA,rollapply(respira$Urnoon,3, function(x) median(x, na.rm = TRUE)))
respira$Urnoonmov4<-c(NA,NA,NA,rollapply(respira$Urnoon,4, function(x) median(x, na.rm = TRUE)))
respira$Urnoonmov5<-c(NA,NA,NA,NA,rollapply(respira$Urnoon,5, function(x) median(x, na.rm = TRUE)))
respira$Urnoonmov6<-c(NA,NA,NA,NA,NA,rollapply(respira$Urnoon,6, function(x) median(x, na.rm = TRUE)))
respira$Urnoonmov7<-c(NA,NA,NA,NA,NA,NA,rollapply(respira$Urnoon,7, function(x) median(x, na.rm = TRUE)))

respira$Urminmov2<-c(NA,rollapply(respira$Urmin,2, function(x) median(x, na.rm = TRUE)))
respira$Urminmov3<-c(NA,NA,rollapply(respira$Urmin,3, function(x) median(x, na.rm = TRUE)))
respira$Urminmov4<-c(NA,NA,NA,rollapply(respira$Urmin,4, function(x) median(x, na.rm = TRUE)))
respira$Urminmov5<-c(NA,NA,NA,NA,rollapply(respira$Urmin,5, function(x) median(x, na.rm = TRUE)))
respira$Urminmov6<-c(NA,NA,NA,NA,NA,rollapply(respira$Urmin,6, function(x) median(x, na.rm = TRUE)))
respira$Urminmov7<-c(NA,NA,NA,NA,NA,NA,rollapply(respira$Urmin,7, function(x) median(x, na.rm = TRUE)))

respira$mp10_av_lag0mov2<-c(NA,rollapply(respira$mp10_av_lag0,2, function(x) median(x, na.rm = TRUE)))
respira$mp10_av_lag0mov3<-c(NA,NA,rollapply(respira$mp10_av_lag0,3, function(x) median(x, na.rm = TRUE)))
respira$mp10_av_lag0mov4<-c(NA,NA,NA,rollapply(respira$mp10_av_lag0,4, function(x) median(x, na.rm = TRUE)))
respira$mp10_av_lag0mov5<-c(NA,NA,NA,NA,rollapply(respira$mp10_av_lag0,5, function(x) median(x, na.rm = TRUE)))
respira$mp10_av_lag0mov6<-c(NA,NA,NA,NA,NA,rollapply(respira$mp10_av_lag0,6, function(x) median(x, na.rm = TRUE)))
respira$mp10_av_lag0mov7<-c(NA,NA,NA,NA,NA,NA,rollapply(respira$mp10_av_lag0,7, function(x) median(x, na.rm = TRUE)))

respira$no2_av_lag0mov2<-c(NA,rollapply(respira$no2_av_lag0,2, function(x) median(x, na.rm = TRUE)))
respira$no2_av_lag0mov3<-c(NA,NA,rollapply(respira$no2_av_lag0,3, function(x) median(x, na.rm = TRUE)))
respira$no2_av_lag0mov4<-c(NA,NA,NA,rollapply(respira$no2_av_lag0,4, function(x) median(x, na.rm = TRUE)))
respira$no2_av_lag0mov5<-c(NA,NA,NA,NA,rollapply(respira$no2_av_lag0,5, function(x) median(x, na.rm = TRUE)))
respira$no2_av_lag0mov6<-c(NA,NA,NA,NA,NA,rollapply(respira$no2_av_lag0,6, function(x) median(x, na.rm = TRUE)))
respira$no2_av_lag0mov7<-c(NA,NA,NA,NA,NA,NA,rollapply(respira$no2_av_lag0,7, function(x) median(x, na.rm = TRUE)))

respira$o3_av_lag0mov2<-c(NA,rollapply(respira$o3_av_lag0,2, function(x) median(x, na.rm = TRUE)))
respira$o3_av_lag0mov3<-c(NA,NA,rollapply(respira$o3_av_lag0,3, function(x) median(x, na.rm = TRUE)))
respira$o3_av_lag0mov4<-c(NA,NA,NA,rollapply(respira$o3_av_lag0,4, function(x) median(x, na.rm = TRUE)))
respira$o3_av_lag0mov5<-c(NA,NA,NA,NA,rollapply(respira$o3_av_lag0,5, function(x) median(x, na.rm = TRUE)))
respira$o3_av_lag0mov6<-c(NA,NA,NA,NA,NA,rollapply(respira$o3_av_lag0,6, function(x) median(x, na.rm = TRUE)))
respira$o3_av_lag0mov7<-c(NA,NA,NA,NA,NA,NA,rollapply(respira$o3_av_lag0,7, function(x) median(x, na.rm = TRUE)))


##read cox files

cardio_cox_dow<-read.csv("/Users/samyadelara/Desktop/Thesis/dados&analises/dados/sas/cardio_mattdow.csv",sep=",",na.strings="NA")
respira_cox_dow<-read.csv("/Users/samyadelara/Desktop/Thesis/dados&analises/dados/sas/respira_mattdow.csv",sep=",",na.strings="NA")
cardio_cox_temp<-read.csv("/Users/samyadelara/Desktop/Thesis/dados&analises/dados/sas/cardio_mattemp.csv",sep=",",na.strings="NA")
respira_cox_temp<-read.csv("/Users/samyadelara/Desktop/Thesis/dados&analises/dados/sas/respira_mattemp.csv",sep=",",na.strings="NA")



###descreptive analysis###

write.csv(stat.desc(cardio[,c(20,29,30,31,32,33,39,49,61,71:118)]),file="statisticacardio.csv")
write.csv(stat.desc(respira[,c(20,25,26,27,28,29,35,45,57,67:114)]),file="statisticarespira.csv")

#write.csv(cor(data.frame(c_district$tmed,c_district$Urnoon,c_district$mp10_av_lag0),use="pairwise.complete.obs"),file='corr.csv')

###descreptive###

#correlation#
ccorrelaspe<-rcorr(as.matrix(cardio[sapply(cardio, is.numeric)]),type="spearman")
ccs1<-ccorrelaspe$r[,19]
ccs2<-ccorrelaspe$P[,19]
ccorrelapea<-rcorr(as.matrix(cardio[sapply(cardio, is.numeric)]),type="pearson")
ccp1<-ccorrelapea$r[,19]
ccp2<-ccorrelapea$P[,19]
rcorrelaspe<-rcorr(as.matrix(respira[sapply(respira, is.numeric)]),type="spearman")
rcs1<-rcorrelaspe$r[,19]
rcs2<-rcorrelaspe$P[,19]
rcorrelapea<-rcorr(as.matrix(respira[sapply(respira, is.numeric)]),type="pearson")
rcp1<-rcorrelapea$r[,19]
rcp2<-rcorrelapea$P[,19]

c1<-cor.test(cardio$tmed,cardio$Urmin)
c2<-cor.test(cardio$tmed,cardio$mp10_av_lag0)
c3<-cor.test(cardio$tmed,cardio$no2_av_lag0)
c4<-cor.test(cardio$tmed,cardio$o3_av_lag0)
c5<-cor.test(cardio$Urmin,cardio$mp10_av_lag0)
c6<-cor.test(cardio$Urmin,cardio$no2_av_lag0)
c7<-cor.test(cardio$Urmin,cardio$o3_av_lag0)
c8<-cor.test(cardio$mp10_av_lag0,cardio$no2_av_lag0)
c9<-cor.test(cardio$mp10_av_lag0,cardio$o3_av_lag0)
c10<-cor.test(cardio$obitos_total,cardio$mp10_av_lag0)
c11<-cor.test(cardio$obitos_total,cardio$no2_av_lag0)
c12<-cor.test(cardio$obitos_total,cardio$o3_av_lag0)
c13<-cor.test(cardio$obitos_total,cardio$tmed)
c14<-cor.test(cardio$obitos_total,cardio$Urmin)
c15<-cor.test(respira$obitos_total,cardio$mp10_av_lag0)
c16<-cor.test(respira$obitos_total,cardio$no2_av_lag0)
c17<-cor.test(respira$obitos_total,cardio$o3_av_lag0)
c18<-cor.test(respira$obitos_total,cardio$tmed)
c19<-cor.test(respira$obitos_total,cardio$Urmin)

pcorrela<-c(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19)

write.csv(ccs1,file="ccs1.csv")
write.csv(ccs2,file="ccs2.csv")
write.csv(ccp1,file="ccp1.csv")
write.csv(ccp2,file="ccp2.csv")
write.csv(rcs1,file="rcs1.csv")
write.csv(rcs2,file="rcs2.csv")
write.csv(rcp1,file="rcp1.csv")
write.csv(rcp2,file="rcp2.csv")

#correlation of exposures#

write.csv(cor(x<-data.frame(cardio$tmed,cardio$tmin,cardio$tmax,cardio$Urmin,cardio$Urnoon,cardio$mp10_av_lag0,cardio$no2_av_lag0,cardio$o3_av_lag0),use="pairwise.complete.obs",method="pearson"),file='exposurecorr.csv')

write.csv(rcorr(as.matrix(x[sapply(x, is.numeric)]),type="pearson")$P,file="exposurecorrtest.csv")


#poisson tests#

#####CARDIO#####
cmean<-mean(cardio$obitos_total)
cvar<-var(cardio$obitos_total)

hist(cardio$obitos_total,freq=FALSE,xlim=c(20,120),ylim=c(0,0.06),main="Distribuição da Mortalidade Diária por
doenças cardiovasculares (acima de 40 anos) - 1998 a 2008",xlab="Mortalidade Diária", ylab="Densidade de probabilidade",col="darkblue",border="darkgrey")
abline(v=cmean,col="darkred",lwd=2)
lines(x<-seq(20,100,1),dpois(x,lambda=cmean),lty=2,col="black",lwd=2)
legend(x=80,y=0.03, bty="n", lwd=2, col=c("black","darkred"), legend=c("Distribuição de Probabilidade Poisson
com média igual à 
média da Mortalidade Diária","Mortalidade Diária Média"),lty=c(2,1),ncol=1,cex=0.6)

cpoisson = rpois(4018, lambda=cmean)
qqplot(cardio$obitos_total, cpoisson, main="Q-Q Plot
Mortalidade Diária - doenças cardiovasculares
1998-2008", xlab="Mortalidade Diária")
abline(a=0, b=1, lty=2, col="red")

cgf <- goodfit(cardio$obitos_total,type="poisson", method = "MinChisq")
summary(cgf)
cgf <- goodfit(cardio$obitos_total,type="poisson", method = "ML")
summary(cgf)

#####RESPIRA#####
rmean<-mean(respira$obitos_total)
rvar<-var(respira$obitos_total)

hist(respira$obitos_total,freq=FALSE,xlim=c(0,50),ylim=c(0,0.1),main="Distribuição da Mortalidade Diária por
doenças respiratórias (acima de 60 anos) - 1998 a 2008",xlab="Mortalidade Diária", ylab="Densidade de probabilidade",col="darkblue",border="darkgrey")
abline(v=rmean,col="darkred",lwd=2)
lines(x<-seq(0,50,1),dpois(x,lambda=rmean),lty=2,col="black",lwd=2)
legend(x=30,y=0.03, bty="n", lwd=2, col=c("black","darkred"), legend=c("Distribuição de Probabilidade Poisson
com média igual à 
média da Mortalidade Diária","Mortalidade Diária Média"),lty=c(2,1),ncol=1,cex=0.6)

rpoisson = rpois(4018, lambda=rmean)
qqplot(respira$obitos_total, rpoisson, main="Q-Q Plot
Mortalidade Diária - doenças respiratórias
1998-2008", xlab="Mortalidade Diária")
abline(a=0, b=1, lty=2, col="red")

rgf <- goodfit(respira$obitos_total,type="poisson", method = "MinChisq")
summary(rgf)
rgf <- goodfit(respira$obitos_total,type="poisson", method = "ML")
summary(rgf)





				###### Match by dow #####
				
				
###### CARDIO #######
#cardio mp10
cmod1_mp10<-gam(obitos_total~mp10_av_lag0+Urmin+I(as.factor(yy):as.factor(mm):as.factor(dow))+s(tmed,fx=T,k=5),data=cardio,family=poisson,na.action=na.exclude)


a1_mp10<-gam(obitos_total~mp10_av_lag0+s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmed,fx=T,k=5)+Urmin,data=cardio,family=poisson,na.action=na.exclude)


#cardio no2

cmod1_no2<-gam(obitos_total~no2_av_lag0+I(as.factor(yy):as.factor(mm):as.factor(dow))+s(tmed,fx=T,k=5)+Urmin,data=cardio,family=poisson,na.action=na.exclude)


a1_no2<-gam(obitos_total~no2_av_lag0+s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmed,fx=T,k=5)+Urmin,data=cardio,family=poisson,na.action=na.exclude)


#cardio o3

cmod1_o3<-gam(obitos_total~o3_av_lag0mov2+I(as.factor(yy):as.factor(mm):as.factor(dow))+s(tmed,fx=T,k=5)+Urmin,data=cardio,family=poisson,na.action=na.exclude)

a1_o3<-gam(obitos_total~o3_av_lag0mov2+s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmed,fx=T,k=5)+Urmin,data=cardio,family=poisson,na.action=na.exclude)


###### RESPIRA #######
#respira mp10

rmod2_mp10<-gam(obitos_total~mp10_av_lag0+I(as.factor(yy):as.factor(mm):as.factor(dow))+s(tmedmov2,fx=T,k=5)+Urmin,data=respira,family=poisson,na.action=na.exclude)

a2_mp10<-gam(obitos_total~mp10_av_lag0+s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov2,fx=T,k=5)+Urmin,data=respira,family=poisson,na.action=na.exclude)

#respira no2

rmod2_no2<-gam(obitos_total~no2_av_lag0+I(as.factor(yy):as.factor(mm):as.factor(dow))+s(tmedmov2,fx=T,k=5)+Urmin,data=respira,family=poisson,na.action=na.exclude)


a2_no2<-gam(obitos_total~no2_av_lag0+s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov2,fx=T,k=5)+Urmin,data=respira,family=poisson,na.action=na.exclude)


#respira o3

rmod2_o3<-gam(obitos_total~o3_av_lag5+I(as.factor(yy):as.factor(mm):as.factor(dow))+s(tmedmov2,fx=T,k=5)+Urmin,data=respira,family=poisson,na.action=na.exclude)

a2_o3<-gam(obitos_total~o3_av_lag5+s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov2,fx=T,k=5)+Urmin,data=respira,family=poisson,na.action=na.exclude)



#*COX*#
cmodcox1_mp10<-coxph(Surv(time,case)~mp10_av_lag0+ns(tmed,df=4)+Urmin+strata(date),weights=obitos_total,data=cardio_cox_dow,singular.ok=T)
cmodcox1_no2<-coxph(Surv(time,case)~no2_av_lag0+ns(tmed,df=4)+Urmin+strata(date),weights=obitos_total,data=cardio_cox_dow,singular.ok=T)
cmodcox1_o3<-coxph(Surv(time,case)~o3_av_lag0mov2+ns(tmed,df=4)+Urmin+strata(date),weights=obitos_total,data=cardio_cox_dow,singular.ok=T) #erro no cox arquivo sem rollmeans


#### extraction ###
#mp10
beta_mp10<-c(a1_mp10$coef[2],cmod1_mp10$coef[2],a2_mp10$coef[2],rmod2_mp10$coef[2])
se_mp10<-sqrt(c(a1_mp10$Vp[2,2],cmod1_mp10$Vp[2,2],a2_mp10$Vp[2,2],rmod2_mp10$Vp[2,2]))
risk_mp10<-exp(beta_mp10*10)
loIC_mp10<-exp((beta_mp10-1.96*se_mp10)*10)
hiIC_mp10<-exp((beta_mp10+1.96*se_mp10)*10)
perc_mp10<-(risk_mp10-1)*100
percloIC_mp10<-(loIC_mp10-1)*100
perchiIC_mp10<-(hiIC_mp10-1)*100
p_mp10<-c(summary(a1_mp10)$p.table[2,4],summary(cmod1_mp10)$p.table[2,4],summary(a2_mp10)$p.table[2,4],summary(rmod2_mp10)$p.table[2,4])
mp10<-data.frame(beta_mp10,se_mp10,loIC_mp10,risk_mp10,hiIC_mp10,percloIC_mp10,perc_mp10,perchiIC_mp10,p_mp10)
row.names(mp10)<-c("cardio_ts","cardio_co","respira_ts","respira_co")
mp10<-t(mp10)


#no2
beta_no2<-c(a1_no2$coef[2],cmod1_no2$coef[2],a2_no2$coef[2],rmod2_no2$coef[2])
se_no2<-sqrt(c(a1_no2$Vp[2,2],cmod1_no2$Vp[2,2],a2_no2$Vp[2,2],rmod2_no2$Vp[2,2]))
risk_no2<-exp(beta_no2*10)
loIC_no2<-exp((beta_no2-1.96*se_no2)*10)
hiIC_no2<-exp((beta_no2+1.96*se_no2)*10)
perc_no2<-(risk_no2-1)*100
percloIC_no2<-(loIC_no2-1)*100
perchiIC_no2<-(hiIC_no2-1)*100
p_no2<-c(summary(a1_no2)$p.table[2,4],summary(cmod1_no2)$p.table[2,4],summary(a2_no2)$p.table[2,4],summary(rmod2_no2)$p.table[2,4])
no2<-data.frame(beta_no2,se_no2,loIC_no2,risk_no2,hiIC_no2,percloIC_no2,perc_no2,perchiIC_no2,p_no2)
row.names(no2)<-c("cardio_ts","cardio_co","respira_ts","respira_co")
no2<-t(no2)

#o3
beta_o3<-c(a1_o3$coef[2],cmod1_o3$coef[2],a2_o3$coef[2],rmod2_o3$coef[2])
se_o3<-sqrt(c(a1_o3$Vp[2,2],cmod1_o3$Vp[2,2],a2_o3$Vp[2,2],rmod2_o3$Vp[2,2]))
risk_o3<-exp(beta_o3*10)
loIC_o3<-exp((beta_o3-1.96*se_o3)*10)
hiIC_o3<-exp((beta_o3+1.96*se_o3)*10)
perc_o3<-(risk_o3-1)*100
percloIC_o3<-(loIC_o3-1)*100
perchiIC_o3<-(hiIC_o3-1)*100
p_o3<-c(summary(a1_o3)$p.table[2,4],summary(cmod1_o3)$p.table[2,4],summary(a2_o3)$p.table[2,4],summary(rmod2_o3)$p.table[2,4])
o3<-data.frame(beta_o3,se_o3,loIC_o3,risk_o3,hiIC_o3,percloIC_o3,perc_o3,perchiIC_o3,p_o3)
row.names(o3)<-c("cardio_ts","cardio_co","respira_ts","respira_co")
o3<-t(o3)

#scatterplot

#cardio
par(mfrow=c(1,1))


plotCI(c(15,25,55,65,95,105),c(mp10[7,1],mp10[7,2],no2[7,1],no2[7,2],o3[7,1],o3[7,2]),ui=c(mp10[8,1],mp10[8,2],no2[8,1],no2[8,2],o3[8,1],o3[8,2]),li=c(mp10[6,1],mp10[6,2],no2[6,1],no2[6,2],o3[6,1],o3[6,2]),pch=16,xaxt="n",xlab="",ylim=c(-1,2.5),ylab="Mudança percentual (%)",xlim=c(0,120),col=c("#6495ED","#00008B","#6495ED","#00008B","#6495ED","#00008B"))
text(20,-0.8,expression(MP[10]),cex=1.3)
text(60,-0.8,expression(NO[2]),cex=1.3)
text(100,-0.8,expression(O[3]),cex=1.3)
mtext("Mudança percentual no risco",side=3,line=2.2,cex=1.2)
mtext(expression(paste("por 10",mu,"g/",m^3," de aumento na concentração do poluente")),side=3,line=1,cex=1.2)
mtext("doenças cardiovasculares",side=3,line=0.3)

abline(h=0,col="#2E8B57",lwd=1.5,lty=2)


#respira
par(mfrow=c(1,1))


plotCI(c(15,25,55,65,95,105),c(mp10[7,3],mp10[7,4],no2[7,3],no2[7,4],o3[7,3],o3[7,4]),ui=c(mp10[8,3],mp10[8,4],no2[8,3],no2[8,4],o3[8,3],o3[8,4]),li=c(mp10[6,3],mp10[6,4],no2[6,3],no2[6,4],o3[6,3],o3[6,4]),pch=16,xaxt="n",xlab="",ylim=c(-1,2.5),ylab="Mudança percentual (%)",xlim=c(0,120),col=c("#6495ED","#00008B","#6495ED","#00008B","#6495ED","#00008B"))
text(20,-0.8,expression(MP[10]),cex=1.3)
text(60,-0.8,expression(NO[2]),cex=1.3)
text(100,-0.8,expression(O[3]),cex=1.3)
mtext("Mudança percentual no risco",side=3,line=2.2,cex=1.2)
mtext(expression(paste("por 10",mu,"g/",m^3," de aumento na concentração do poluente")),side=3,line=1,cex=1.2)
mtext("doenças respiratórias",side=3,line=0.3)

abline(h=0,col="#2E8B57",lwd=1.5,lty=2)

######temp########

p_temp<-c(summary(a1_mp10)$s.table[2,4],summary(cmod1_mp10)$s.table[1,4],summary(a2_mp10)$s.table[2,4],summary(rmod2_mp10)$s.table[1,4],summary(a1_no2)$s.table[2,4],summary(cmod1_no2)$s.table[1,4],summary(a2_no2)$s.table[2,4],summary(rmod2_no2)$s.table[1,4],summary(a1_o3)$s.table[2,4],summary(cmod1_o3)$s.table[1,4],summary(a2_o3)$s.table[2,4],summary(rmod2_o3)$s.table[1,4])

predtemp1<-predict(a1_mp10,type='terms',se.fit=TRUE)
predtemp2<-predict(cmod1_mp10,type='terms',se.fit=TRUE)
predtemp3<-predict(a2_mp10,type='terms',se.fit=TRUE)
predtemp4<-predict(rmod2_mp10,type='terms',se.fit=TRUE)
predtemp5<-predict(a1_no2,type='terms',se.fit=TRUE)
predtemp6<-predict(cmod1_no2,type='terms',se.fit=TRUE)
predtemp7<-predict(a2_no2,type='terms',se.fit=TRUE)
predtemp8<-predict(rmod2_no2,type='terms',se.fit=TRUE)
predtemp9<-predict(a1_o3,type='terms',se.fit=TRUE)
predtemp10<-predict(cmod1_o3,type='terms',se.fit=TRUE)
predtemp11<-predict(a2_o3,type='terms',se.fit=TRUE)
predtemp12<-predict(rmod2_o3,type='terms',se.fit=TRUE)

temp1<-(exp(predtemp1$fit[,6])-1)*100
temp2<-(exp(predtemp2$fit[,4])-1)*100
temp3<-(exp(predtemp3$fit[,6])-1)*100
temp4<-(exp(predtemp4$fit[,4])-1)*100
temp5<-(exp(predtemp5$fit[,6])-1)*100
temp6<-(exp(predtemp6$fit[,4])-1)*100
temp7<-(exp(predtemp7$fit[,6])-1)*100
temp8<-(exp(predtemp8$fit[,4])-1)*100
temp9<-(exp(predtemp9$fit[,6])-1)*100
temp10<-(exp(predtemp10$fit[,4])-1)*100
temp11<-(exp(predtemp11$fit[,6])-1)*100
temp12<-(exp(predtemp12$fit[,4])-1)*100

lotemp1<-(exp(predtemp1$fit[,6]-1.96*predtemp1$se.fit[,6])-1)*100
lotemp2<-(exp(predtemp2$fit[,4]-1.96*predtemp2$se.fit[,4])-1)*100
lotemp3<-(exp(predtemp3$fit[,6]-1.96*predtemp3$se.fit[,6])-1)*100
lotemp4<-(exp(predtemp4$fit[,4]-1.96*predtemp4$se.fit[,4])-1)*100
lotemp5<-(exp(predtemp5$fit[,6]-1.96*predtemp5$se.fit[,6])-1)*100
lotemp6<-(exp(predtemp6$fit[,4]-1.96*predtemp6$se.fit[,4])-1)*100
lotemp7<-(exp(predtemp7$fit[,6]-1.96*predtemp7$se.fit[,6])-1)*100
lotemp8<-(exp(predtemp8$fit[,4]-1.96*predtemp8$se.fit[,4])-1)*100
lotemp9<-(exp(predtemp9$fit[,6]-1.96*predtemp9$se.fit[,6])-1)*100
lotemp10<-(exp(predtemp10$fit[,4]-1.96*predtemp10$se.fit[,4])-1)*100
lotemp11<-(exp(predtemp11$fit[,6]-1.96*predtemp11$se.fit[,6])-1)*100
lotemp12<-(exp(predtemp12$fit[,4]-1.96*predtemp12$se.fit[,4])-1)*100

hitemp1<-(exp(predtemp1$fit[,6]+1.96*predtemp1$se.fit[,6])-1)*100
hitemp2<-(exp(predtemp2$fit[,4]+1.96*predtemp2$se.fit[,4])-1)*100
hitemp3<-(exp(predtemp3$fit[,6]+1.96*predtemp3$se.fit[,6])-1)*100
hitemp4<-(exp(predtemp4$fit[,4]+1.96*predtemp4$se.fit[,4])-1)*100
hitemp5<-(exp(predtemp5$fit[,6]+1.96*predtemp5$se.fit[,6])-1)*100
hitemp6<-(exp(predtemp6$fit[,4]+1.96*predtemp6$se.fit[,4])-1)*100
hitemp7<-(exp(predtemp7$fit[,6]+1.96*predtemp7$se.fit[,6])-1)*100
hitemp8<-(exp(predtemp8$fit[,4]+1.96*predtemp8$se.fit[,4])-1)*100
hitemp9<-(exp(predtemp9$fit[,6]+1.96*predtemp9$se.fit[,6])-1)*100
hitemp10<-(exp(predtemp10$fit[,4]+1.96*predtemp10$se.fit[,4])-1)*100
hitemp11<-(exp(predtemp11$fit[,6]+1.96*predtemp11$se.fit[,6])-1)*100
hitemp12<-(exp(predtemp12$fit[,4]+1.96*predtemp12$se.fit[,4])-1)*100



#plot(cardio$tmed[is.na(cardio$mp10_av_lag0)==F],temp1,pch=17,ylim=c(min(lotemp1),max(hitemp1)),xlab="tmed",ylab="Percent Change")
#legend("bottomright", bty="n", lwd=2, col=c("#000000","#8B0000","#B22222","#EE0000","#FF4040","#8B4500","#FF7F00","#FF4500"), legend=c("TOTAL", "Aricanduva","Butantã","Campo Limpo","Capela do Socorro","Casa Verde","Cidade Ademar","Cidade Tiradentes"),ncol=2,cex=0.4)
#lines(smooth.spline(mod1_mp10_ari$model$tmed , r1$fit[,4]) , lwd = 3 , col = "#8B0000")


par(mfrow=c(1,3))

#cardio+mp10

plot(0, type="n", bty="n", main="", xlab="Temperatura (˚C)",ylim=c(-4,10), lwd=1.5, xlim=c(5,30),ylab="Mudança percentual no risco (%)")
mtext("Função de alisamento para Temp. média",3,line=2,cex=1.0)
mtext(expression("doenças cardiovasculares - modelo MP"[10]),3,line=0.5,cex=0.8)
abline(h=1,col="black",lwd=1.5,lty=2)

polygon(c(xcA$cardio.tmed,rev(xcA$cardio.tmed)),c(xcA$lotemp1,rev(xcA$hitemp1)),col="#FF8C0055"
,border=NA)
points(xcA$cardio.tmed,xcA$temp1,type="l",lwd=2.5,col="#FF8C00")
polygon(c(xcA$cardio.tmed,rev(xcA$cardio.tmed)),c(xcA$lotemp2,rev(xcA$hitemp2)),col="#8B450055"
,border=NA)
points(xcA$cardio.tmed,xcA$temp2,type="l",lwd=2.5,col="#8B4500")

#cardio+no2
plot(0, type="n", bty="n", main="", xlab="Temperatura (˚C)",ylim=c(-4,10), lwd=1.5, xlim=c(5,30),ylab="Mudança percentual no risco (%)")
mtext("Função de alisamento para Temp. média",3,line=2,cex=1.0)
mtext(expression("doenças cardiovasculares - modelo NO"[2]),3,line=0.5,cex=0.8)
abline(h=1,col="black",lwd=1.5,lty=2)

polygon(c(xcB$cardio.tmed,rev(xcB$cardio.tmed)),c(xcB$lotemp5,rev(xcB$hitemp5)),col="#BF3EFF55"
,border=NA)
points(xcB$cardio.tmed,xcB$temp5,type="l",lwd=2.5,col="#BF3EFF")
polygon(c(xcB$cardio.tmed,rev(xcB$cardio.tmed)),c(xcB$lotemp6,rev(xcB$hitemp6)),col="#68228B55"
,border=NA)
points(xcB$cardio.tmed,xcB$temp6,type="l",lwd=2.5,col="#68228B")

#cardio+o3
plot(0, type="n", bty="n", main="", xlab="Temperatura (˚C)",ylim=c(-4,10), lwd=1.5, xlim=c(5,30),ylab="Mudança percentual no risco (%)")
mtext("Função de alisamento para Temp. média",3,line=2,cex=1.0)
mtext(expression("doenças cardiovasculares - modelo O"[3]),3,line=0.5,cex=0.8)
abline(h=1,col="black",lwd=1.5,lty=2)

polygon(c(xcC$cardio.tmed,rev(xcC$cardio.tmed)),c(xcC$lotemp9,rev(xcC$hitemp9)),col="#FF3E9655"
,border=NA)
points(xcC$cardio.tmed,xcC$temp9,type="l",lwd=2.5,col="#FF3E96")
polygon(c(xcC$cardio.tmed,rev(xcC$cardio.tmed)),c(xcC$lotemp10,rev(xcC$hitemp10)),col="#83225255"
,border=NA)
points(xcC$cardio.tmed,xcC$temp10,type="l",lwd=2.5,col="#832252")


par(mfrow=c(1,3))

#respira+mp10

plot(0, type="n", bty="n", main="", xlab="Temperatura (˚C)",ylim=c(-6,12), lwd=1.5, xlim=c(5,30),ylab="Mudança percentual no risco (%)")
mtext("Função de alisamento para Temp. média",3,line=2.5,cex=1.0)
mtext("móvel 2 dias",3,line=1.5,cex=1.0)
mtext(expression("doenças respiratórias - modelo MP"[10]),3,line=0.1,cex=0.8)
abline(h=1,col="black",lwd=1.5,lty=2)

polygon(c(xrA$respira.tmedmov2,rev(xrA$respira.tmedmov2)),c(xrA$lotemp3,rev(xrA$hitemp3)),col="#FF8C0055"
,border=NA)
points(xrA$respira.tmedmov2,xrA$temp3,type="l",lwd=2.5,col="#FF8C00")
polygon(c(xrA$respira.tmedmov2,rev(xrA$respira.tmedmov2)),c(xrA$lotemp4,rev(xrA$hitemp4)),col="#8B450055"
,border=NA)
points(xrA$respira.tmedmov2,xrA$temp4,type="l",lwd=2.5,col="#8B4500")

#respira+no2
plot(0, type="n", bty="n", main="", xlab="Temperatura (˚C)",ylim=c(-6,12), lwd=1.5, xlim=c(5,30),ylab="Mudança percentual no risco (%)")
mtext("Função de alisamento para Temp. média",3,line=2.5,cex=1.0)
mtext("móvel 2 dias",3,line=1.5,cex=1.0)
mtext(expression("doenças respiratórias - modelo NO"[2]),3,line=0.1,cex=0.8)
abline(h=1,col="black",lwd=1.5,lty=2)

polygon(c(xrB$respira.tmedmov2,rev(xrB$respira.tmedmov2)),c(xrB$lotemp7,rev(xrB$hitemp7)),col="#BF3EFF55"
,border=NA)
points(xrB$respira.tmedmov2,xrB$temp7,type="l",lwd=2.5,col="#BF3EFF")
polygon(c(xrB$respira.tmedmov2,rev(xrB$respira.tmedmov2)),c(xrB$lotemp8,rev(xrB$hitemp8)),col="#68228B55"
,border=NA)
points(xrB$respira.tmedmov2,xrB$temp8,type="l",lwd=2.5,col="#68228B")

#respira+o3
plot(0, type="n", bty="n", main="", xlab="Temperatura (˚C)",ylim=c(-6,12), lwd=1.5, xlim=c(5,30),ylab="Mudança percentual no risco (%)")
mtext("Função de alisamento para Temp. média",3,line=2.5,cex=1.0)
mtext("móvel 2 dias",3,line=1.5,cex=1.0)
mtext(expression("doenças respiratórias - modelo O"[3]),3,line=0.1,cex=0.8)
abline(h=1,col="black",lwd=1.5,lty=2)

polygon(c(xrC$respira.tmedmov2,rev(xrC$respira.tmedmov2)),c(xrC$lotemp11,rev(xrC$hitemp11)),col="#FF3E9655"
,border=NA)
points(xrC$respira.tmedmov2,xrC$temp11,type="l",lwd=2.5,col="#FF3E96")
polygon(c(xrC$respira.tmedmov2,rev(xrC$respira.tmedmov2)),c(xrC$lotemp12,rev(xrC$hitemp12)),col="#83225255"
,border=NA)
points(xrC$respira.tmedmov2,xrC$temp12,type="l",lwd=2.5,col="#832252")





#points(x$cardio.tmed,x$temp5,type="l",lwd=2,col="#000022")
#points(x$cardio.tmed,x$temp9,type="l",lwd=2,col="#00BB00")
#lines(smooth.spline(cardio$tmed[is.na(cardio$mp10_av_lag0)==F],temp1[is.na(temp1)==F]),lwd = 2 , col = "darkred")


###### Match by temp ######


##mp10
ctmod1_mp10<-gam(obitos_total~mp10_av_lag0+Urmin+I(as.factor(mattemp1):as.factor(mattemp2):as.factor(mm):as.factor(yy))+as.factor(dow),data=cardio,family=poisson,na.action=na.exclude)
rtmod2_mp10<-gam(obitos_total~mp10_av_lag0+Urmin+I(as.factor(mattemp1):as.factor(mattemp2):as.factor(mm):as.factor(yy))+as.factor(dow),data=respira,family=poisson,na.action=na.exclude)

#ctmodcox1_mp10<-coxph(Surv(time,case)~mp10_av_lag0+wd1+wd2+wd3+wd4+wd5+wd6+wd7+Urmin+strata(date),weights=obitos_total,data=cardio_cox_temp,singular.ok=T)
#rtmodcox2_mp10<-coxph(Surv(time,case)~mp10_av_lag0+as.factor(dow)+Urmin+strata(date),weights=obitos_total,data=respira_cox_temp,singular.ok=T)

##no2
ctmod1_no2<-gam(obitos_total~no2_av_lag0+Urmin+I(as.factor(mattemp1):as.factor(mattemp2):as.factor(mm):as.factor(yy))+as.factor(dow),data=cardio,family=poisson,na.action=na.exclude)
rtmod2_no2<-gam(obitos_total~no2_av_lag0+Urmin+I(as.factor(mattemp1):as.factor(mattemp2):as.factor(mm):as.factor(yy))+as.factor(dow),data=respira,family=poisson,na.action=na.exclude)

##o3
ctmod1_o3<-gam(obitos_total~o3_av_lag0mov2+Urmin+I(as.factor(mattemp1):as.factor(mattemp2):as.factor(mm):as.factor(yy))+as.factor(dow),data=cardio,family=poisson,na.action=na.exclude)
rtmod2_o3<-gam(obitos_total~o3_av_lag5+Urmin+I(as.factor(mattemp1):as.factor(mattemp2):as.factor(mm):as.factor(yy))+as.factor(dow),data=respira,family=poisson,na.action=na.exclude)


##extract results

beta_mattemp<-c(ctmod1_mp10$coef[2],rtmod2_mp10$coef[2],ctmod1_no2$coef[2],rtmod2_no2$coef[2],ctmod1_o3$coef[2],rtmod2_o3$coef[2])
se_mattemp<-sqrt(c(ctmod1_mp10$Vp[2,2],rtmod2_mp10$Vp[2,2],ctmod1_no2$Vp[2,2],rtmod2_no2$Vp[2,2],ctmod1_o3$Vp[2,2],rtmod2_o3$Vp[2,2]))
risk_mattemp<-exp(beta_mattemp*10)
loIC_mattemp<-exp((beta_mattemp-1.96*se_mattemp)*10)
hiIC_mattemp<-exp((beta_mattemp+1.96*se_mattemp)*10)
perc_mattemp<-(risk_mattemp-1)*100
percloIC_mattemp<-(loIC_mattemp-1)*100
perchiIC_mattemp<-(hiIC_mattemp-1)*100
p_mattemp<-c(summary(ctmod1_mp10)$p.table[2,4],summary(rtmod2_mp10)$p.table[2,4],summary(ctmod1_no2)$p.table[2,4],summary(rtmod2_no2)$p.table[2,4],summary(ctmod1_o3)$p.table[2,4],summary(rtmod2_o3)$p.table[2,4])
mattemp<-data.frame(beta_mattemp,se_mattemp,loIC_mattemp,risk_mattemp,hiIC_mattemp,percloIC_mattemp,perc_mattemp,perchiIC_mattemp,p_mattemp)
row.names(mattemp)<-c("cardio_mp10","respira_mp10","cardio_no2","respira_no2","cardio_o3","respira_o3")
mattemp<-t(mattemp)

#extract table
mp10<-cbind(mp10,mattemp[,1:2])
no2<-cbind(no2,mattemp[,3:4])
o3<-cbind(o3,mattemp[,5:6])

write.csv(mp10,file="mp10.csv")
write.csv(no2,file="no2.csv")
write.csv(o3,file="o3.csv")

#scatterplot

#cardio
pdf(file="pollu_cardio.pdf", onefile=T)
par(mfrow=c(1,1))


plotCI(c(15,25,35,65,75,85,115,125,135),c(mp10[7,1],mp10[7,2],mp10[7,5],no2[7,1],no2[7,2],no2[7,5],o3[7,1],o3[7,2],o3[7,5]),ui=c(mp10[8,1],mp10[8,2],mp10[8,5],no2[8,1],no2[8,2],no2[8,5],o3[8,1],o3[8,2],o3[8,5]),li=c(mp10[6,1],mp10[6,2],mp10[6,5],no2[6,1],no2[6,2],no2[6,5],o3[6,1],o3[6,2],o3[6,5]),pch=16,xaxt="n",ylim=c(-1,2.5),xlab="",yaxt="n",ylab="Mudança percentual (%)",xlim=c(0,150),col=c("#6495ED","#00008B","#20B2AA" ,"#6495ED","#00008B","#20B2AA","#6495ED","#00008B","#20B2AA"))
axis(2,at=pretty(-1:2.5),labels=chartr(".", ",", as.character(pretty(-1:2.5)
)))
text(25,-0.8,expression(MP[10]),cex=1.3)
text(75,-0.8,expression(NO[2]),cex=1.3)
text(125,-0.8,expression(O[3]),cex=1.3)
#mtext("Mudança percentual no risco",side=3,line=2.2,cex=1.2)
#mtext(expression(paste("por 10",mu,"g/",m^3," de aumento na concentração do poluente")),side=3,line=1,cex=1.2)
#mtext("doenças cardiovasculares",side=3,line=0.3)

abline(h=0,col="#2E8B57",lwd=1.5,lty=2)

legend(95,2.6, bty="n", lwd=2, col=c("#6495ED","#00008B","#20B2AA"), legend=c("Série temporal", "Case-crossover - dia da semana","Case-crossover - temperatura"),ncol=1,cex=0.7)

dev.off()

#respira
pdf(file="pollu_respira.pdf", onefile=T)
par(mfrow=c(1,1))


plotCI(c(15,25,35,65,75,85,115,125,135),c(mp10[7,3],mp10[7,4],mp10[7,6],no2[7,3],no2[7,4],no2[7,6],o3[7,3],o3[7,4],o3[7,6]),ui=c(mp10[8,3],mp10[8,4],mp10[8,6],no2[8,3],no2[8,4],no2[8,6],o3[8,3],o3[8,4],o3[8,6]),li=c(mp10[6,3],mp10[6,4],mp10[6,6],no2[6,3],no2[6,4],no2[6,6],o3[6,3],o3[6,4],o3[6,6]),pch=16,xaxt="n",yaxt="n",xlab="",ylim=c(-1,2.5),ylab="Mudança percentual (%)",xlim=c(0,150),col=c("#6495ED","#00008B","#20B2AA" ,"#6495ED","#00008B","#20B2AA","#6495ED","#00008B","#20B2AA"))
axis(2,at=pretty(-1:2.5),labels=chartr(".", ",", as.character(pretty(-1:2.5)
)))
text(25,-0.8,expression(MP[10]),cex=1.3)
text(75,-0.8,expression(NO[2]),cex=1.3)
text(125,-0.8,expression(O[3]),cex=1.3)
#mtext("Mudança percentual no risco",side=3,line=2.2,cex=1.2)
#mtext(expression(paste("por 10",mu,"g/",m^3," de aumento na concentração do poluente")),side=3,line=1,cex=1.2)
#mtext("doenças respiratórias",side=3,line=0.3)

abline(h=0,col="#2E8B57",lwd=1.5,lty=2)

legend(95,2.6, bty="n", lwd=2, col=c("#6495ED","#00008B","#20B2AA"), legend=c("Série temporal", "Case-crossover - dia da semana","Case-crossover - temperatura"),ncol=1,cex=0.7)

dev.off()


###### Match by pollu ######


##mp10
cpmod1_mp10<-gam(obitos_total~Urmin+I(as.factor(matmp10_1):as.factor(matmp10_2):as.factor(mm):as.factor(yy))+as.factor(dow)+s(tmed,fx=T,k=5),data=cardio,family=poisson,na.action=na.exclude)
rpmod2_mp10<-gam(obitos_total~Urmin+I(as.factor(matmp10_1):as.factor(matmp10_2):as.factor(mm):as.factor(yy))+as.factor(dow)+s(tmedmov2,fx=T,k=5),data=respira,family=poisson,na.action=na.exclude)


##no2
cpmod1_no2<-gam(obitos_total~Urmin+I(as.factor(matno2_1):as.factor(matno2_2):as.factor(mm):as.factor(yy))+as.factor(dow)+s(tmed,fx=T,k=5),data=cardio,family=poisson,na.action=na.exclude)
rpmod2_no2<-gam(obitos_total~Urmin+I(as.factor(matno2_1):as.factor(matno2_2):as.factor(mm):as.factor(yy))+as.factor(dow)+s(tmedmov2,fx=T,k=5),data=respira,family=poisson,na.action=na.exclude)

##o3
cpmod1_o3<-gam(obitos_total~Urmin+I(as.factor(mato3_1):as.factor(mato3_2):as.factor(mm):as.factor(yy))+as.factor(dow)+s(tmed,fx=T,k=5),data=cardio,family=poisson,na.action=na.exclude)
rpmod2_o3<-gam(obitos_total~Urmin+I(as.factor(mato3_1):as.factor(mato3_2):as.factor(mm):as.factor(yy))+as.factor(dow)+s(tmedmov2,fx=T,k=5),data=respira,family=poisson,na.action=na.exclude)

#### extract

p_temp<-data.frame(summary(a1_mp10)$s.table[2,4],summary(cmod1_mp10)$s.table[1,4],summary(a2_mp10)$s.table[2,4],summary(rmod2_mp10)$s.table[1,4],summary(a1_no2)$s.table[2,4],summary(cmod1_no2)$s.table[1,4],summary(a2_no2)$s.table[2,4],summary(rmod2_no2)$s.table[1,4],summary(a1_o3)$s.table[2,4],summary(cmod1_o3)$s.table[1,4],summary(a2_o3)$s.table[2,4],summary(rmod2_o3)$s.table[1,4])


p_temp_2<-data.frame(summary(cpmod1_mp10)$s.table[1,4],summary(cpmod1_no2)$s.table[1,4],summary(cpmod1_o3)$s.table[1,4],summary(rpmod2_mp10)$s.table[1,4],summary(rpmod2_no2)$s.table[1,4],summary(rpmod2_o3)$s.table[1,4])

predtemp13<-predict(cpmod1_mp10,type='terms',se.fit=TRUE)
predtemp14<-predict(rpmod2_mp10,type='terms',se.fit=TRUE)
predtemp15<-predict(cpmod1_no2,type='terms',se.fit=TRUE)
predtemp16<-predict(rpmod2_no2,type='terms',se.fit=TRUE)
predtemp17<-predict(cpmod1_o3,type='terms',se.fit=TRUE)
predtemp18<-predict(rpmod2_o3,type='terms',se.fit=TRUE)

temp13<-(exp(predtemp13$fit[,4])-1)*100
temp14<-(exp(predtemp14$fit[,4])-1)*100
temp15<-(exp(predtemp15$fit[,4])-1)*100
temp16<-(exp(predtemp16$fit[,4])-1)*100
temp17<-(exp(predtemp17$fit[,4])-1)*100
temp18<-(exp(predtemp18$fit[,4])-1)*100


lotemp13<-(exp(predtemp13$fit[,4]-1.96*predtemp13$se.fit[,4])-1)*100
lotemp14<-(exp(predtemp14$fit[,4]-1.96*predtemp14$se.fit[,4])-1)*100
lotemp15<-(exp(predtemp15$fit[,4]-1.96*predtemp15$se.fit[,4])-1)*100
lotemp16<-(exp(predtemp16$fit[,4]-1.96*predtemp16$se.fit[,4])-1)*100
lotemp17<-(exp(predtemp17$fit[,4]-1.96*predtemp17$se.fit[,4])-1)*100
lotemp18<-(exp(predtemp18$fit[,4]-1.96*predtemp18$se.fit[,4])-1)*100

hitemp13<-(exp(predtemp13$fit[,4]+1.96*predtemp13$se.fit[,4])-1)*100
hitemp14<-(exp(predtemp14$fit[,4]+1.96*predtemp14$se.fit[,4])-1)*100
hitemp15<-(exp(predtemp15$fit[,4]+1.96*predtemp15$se.fit[,4])-1)*100
hitemp16<-(exp(predtemp16$fit[,4]+1.96*predtemp16$se.fit[,4])-1)*100
hitemp17<-(exp(predtemp17$fit[,4]+1.96*predtemp17$se.fit[,4])-1)*100
hitemp18<-(exp(predtemp18$fit[,4]+1.96*predtemp18$se.fit[,4])-1)*100

xc<-data.frame(cardio$tmed,temp1,lotemp1,hitemp1,temp2,lotemp2,hitemp2,temp5,lotemp5,hitemp5,temp6,lotemp6,hitemp6,temp9,lotemp9,hitemp9,temp10,lotemp10,hitemp10,temp13,lotemp13,hitemp13,temp15,lotemp15,hitemp15,temp17,lotemp17,hitemp17)
xcA<-xc[,c(1,2,3,4,5,6,7,20,21,22)]
xcB<-xc[,c(1,8,9,10,11,12,13,23,24,25)]
xcC<-xc[,c(1,14,15,16,17,18,19,26,27,28)]

xc<-xc[is.na(xc$temp1)==F,]
xc<-xc[order(xc$cardio.tmed),]


xcA<-xcA[is.na(xcA$temp1)==F,]
xcA<-xcA[order(xcA$cardio.tmed),]
xcB<-xcB[is.na(xcB$temp15)==F,]
xcB<-xcB[order(xcB$cardio.tmed),]
xcC<-xcC[is.na(xcC$temp17)==F,]
xcC<-xcC[order(xcC$cardio.tmed),]

xr<-data.frame(respira$tmedmov2,temp3,lotemp3,hitemp3,temp4,lotemp4,hitemp4,temp7,lotemp7,hitemp7,temp8,lotemp8,hitemp8,temp11,lotemp11,hitemp11,temp12,lotemp12,hitemp12,temp14,lotemp14,hitemp14,temp16,lotemp16,hitemp16,temp18,lotemp18,hitemp18)
xrA<-xr[,c(1,2,3,4,5,6,7,20,21,22)]
xrB<-xr[,c(1,8,9,10,11,12,13,23,24,25)]
xrC<-xr[,c(1,14,15,16,17,18,19,26,27,28)]

xr<-xr[is.na(xr$temp11)==F,]
xr<-xr[order(xr$respira.tmedmov2),]


xrA<-xrA[is.na(xrA$temp3)==F,]
xrA<-xrA[order(xrA$respira.tmedmov2),]
xrB<-xrB[is.na(xrB$temp7)==F,]
xrB<-xrB[order(xrB$respira.tmedmov2),]
xrC<-xrC[is.na(xrC$temp11)==F,]
xrC<-xrC[order(xrC$respira.tmedmov2),]

pdf(file="temp_effect.pdf",onefile=T)
par(mfrow=c(1,1))

#cardio+mp10

pdf(file="temp_effect1.pdf",onefile=T)
plot(0, type="n", bty="n", main="", xlab=expression("Temperatura ("~degree~C~")"),ylim=c(-4,10), lwd=1.5, xlim=c(5,30),ylab="Mudança percentual no risco (%)")

abline(h=0,col="black",lwd=1.5,lty=2)

polygon(c(xcA$cardio.tmed,rev(xcA$cardio.tmed)),c(xcA$lotemp1,rev(xcA$hitemp1)),col="#FF8C0055"
,border=NA)
points(xcA$cardio.tmed,xcA$temp1,type="l",lwd=2.5,col="#FF8C00")
polygon(c(xcA$cardio.tmed,rev(xcA$cardio.tmed)),c(xcA$lotemp2,rev(xcA$hitemp2)),col="#8B450055"
,border=NA)
points(xcA$cardio.tmed,xcA$temp2,type="l",lwd=2.5,col="#8B4500")
points(xcA$cardio.tmed,xcA$temp13,type="l",lwd=2.5,col="#20B2AA")
points(xcA$cardio.tmed,xcA$hitemp13,type="l",lty=2,lwd=0.5,col="#20B2AA")
points(xcA$cardio.tmed,xcA$lotemp13,type="l",lty=2,lwd=0.5,col="#20B2AA")

legend(21,-3, bty="n", lwd=2, col=c("#FF8C00","#8B4500","#20B2AA"), legend=c("Série temporal", "Case-crossover - dia da semana","Case-crossover - poluente"),ncol=1,cex=0.7)

dev.off()

#cardio+no2

pdf(file="temp_effect2.pdf",onefile=T)
plot(0, type="n", bty="n", main="", xlab=expression("Temperatura ("~degree~C~")"),ylim=c(-4,10), lwd=1.5, xlim=c(5,30),ylab="Mudança percentual no risco (%)")

abline(h=0,col="black",lwd=1.5,lty=2)

polygon(c(xcB$cardio.tmed,rev(xcB$cardio.tmed)),c(xcB$lotemp5,rev(xcB$hitemp5)),col="#BF3EFF55"
,border=NA)
points(xcB$cardio.tmed,xcB$temp5,type="l",lwd=2.5,col="#BF3EFF")
polygon(c(xcB$cardio.tmed,rev(xcB$cardio.tmed)),c(xcB$lotemp6,rev(xcB$hitemp6)),col="#68228B55"
,border=NA)
points(xcB$cardio.tmed,xcB$temp6,type="l",lwd=2.5,col="#68228B")
points(xcB$cardio.tmed,xcB$temp15,type="l",lwd=2.5,col="#20B2AA")
points(xcB$cardio.tmed,xcB$hitemp15,type="l",lty=2,lwd=0.5,col="#20B2AA")
points(xcB$cardio.tmed,xcB$lotemp15,type="l",lty=2,lwd=0.5,col="#20B2AA")

legend(21,-3, bty="n", lwd=2, col=c("#BF3EFF","#68228B","#20B2AA"), legend=c("Série temporal", "Case-crossover - dia da semana","Case-crossover - poluente"),ncol=1,cex=0.7)

dev.off()

#cardio+o3

pdf(file="temp_effect3.pdf",onefile=T)
plot(0, type="n", bty="n", main="", xlab=expression("Temperatura ("~degree~C~")"),ylim=c(-4,10), lwd=1.5, xlim=c(5,30),ylab="Mudança percentual no risco (%)")
#mtext("Função de alisamento para Temp. média",3,line=2,cex=1.0)
#mtext(expression("doenças cardiovasculares - modelo O"[3]),3,line=0.5,cex=0.8)
abline(h=0,col="black",lwd=1.5,lty=2)

polygon(c(xcC$cardio.tmed,rev(xcC$cardio.tmed)),c(xcC$lotemp9,rev(xcC$hitemp9)),col="#FF3E9655"
,border=NA)
points(xcC$cardio.tmed,xcC$temp9,type="l",lwd=2.5,col="#FF3E96")
polygon(c(xcC$cardio.tmed,rev(xcC$cardio.tmed)),c(xcC$lotemp10,rev(xcC$hitemp10)),col="#83225255"
,border=NA)
points(xcC$cardio.tmed,xcC$temp10,type="l",lwd=2.5,col="#832252")
points(xcC$cardio.tmed,xcC$temp17,type="l",lwd=2.5,col="#20B2AA")
points(xcC$cardio.tmed,xcC$hitemp17,type="l",lty=2,lwd=0.5,col="#20B2AA")
points(xcC$cardio.tmed,xcC$lotemp17,type="l",lty=2,lwd=0.5,col="#20B2AA")

legend(21,-3, bty="n", lwd=2, col=c("#FF3E96","#832252","#20B2AA"), legend=c("Série temporal", "Case-crossover - dia da semana","Case-crossover - poluente"),ncol=1,cex=0.7)

dev.off()

par(mfrow=c(1,1))

#respira+mp10

pdf(file="temp_effect4.pdf",onefile=T)
plot(0, type="n", bty="n", main="", xlab=expression("Temperatura ("~degree~C~")"),ylim=c(-6,12), lwd=1.5, xlim=c(5,30),ylab="Mudança percentual no risco (%)")
#mtext("Função de alisamento para Temp. média",3,line=2.5,cex=1.0)
#mtext("móvel 2 dias",3,line=1.5,cex=1.0)
#mtext(expression("doenças respiratórias - modelo MP"[10]),3,line=0.1,cex=0.8)
abline(h=0,col="black",lwd=1.5,lty=2)

polygon(c(xrA$respira.tmedmov2,rev(xrA$respira.tmedmov2)),c(xrA$lotemp3,rev(xrA$hitemp3)),col="#FF8C0055"
,border=NA)
points(xrA$respira.tmedmov2,xrA$temp3,type="l",lwd=2.5,col="#FF8C00")
polygon(c(xrA$respira.tmedmov2,rev(xrA$respira.tmedmov2)),c(xrA$lotemp4,rev(xrA$hitemp4)),col="#8B450055"
,border=NA)
points(xrA$respira.tmedmov2,xrA$temp4,type="l",lwd=2.5,col="#8B4500")
points(xrA$respira.tmedmov2,xrA$temp14,type="l",lwd=2.5,col="#20B2AA")
points(xrA$respira.tmedmov2,xrA$hitemp14,type="l",lty=2,lwd=0.5,col="#20B2AA")
points(xrA$respira.tmedmov2,xrA$lotemp14,type="l",lty=2,lwd=0.5,col="#20B2AA")

legend(21,-3, bty="n", lwd=2, col=c("#FF8C00","#8B4500","#20B2AA"), legend=c("Série temporal", "Case-crossover - dia da semana","Case-crossover - poluente"),ncol=1,cex=0.7)

dev.off()

#respira+no2

pdf(file="temp_effect5.pdf",onefile=T)

plot(0, type="n", bty="n", main="", xlab=expression("Temperatura ("~degree~C~")"),ylim=c(-6,12), lwd=1.5, xlim=c(5,30),ylab="Mudança percentual no risco (%)")
#mtext("Função de alisamento para Temp. média",3,line=2.5,cex=1.0)
#mtext("móvel 2 dias",3,line=1.5,cex=1.0)
#mtext(expression("doenças respiratórias - modelo NO"[2]),3,line=0.1,cex=0.8)
abline(h=0,col="black",lwd=1.5,lty=2)

polygon(c(xrB$respira.tmedmov2,rev(xrB$respira.tmedmov2)),c(xrB$lotemp7,rev(xrB$hitemp7)),col="#BF3EFF55"
,border=NA)
points(xrB$respira.tmedmov2,xrB$temp7,type="l",lwd=2.5,col="#BF3EFF")
polygon(c(xrB$respira.tmedmov2,rev(xrB$respira.tmedmov2)),c(xrB$lotemp8,rev(xrB$hitemp8)),col="#68228B55"
,border=NA)
points(xrB$respira.tmedmov2,xrB$temp8,type="l",lwd=2.5,col="#68228B")
points(xrB$respira.tmedmov2,xrB$temp16,type="l",lwd=2.5,col="#20B2AA")
points(xrB$respira.tmedmov2,xrB$hitemp16,type="l",lty=2,lwd=0.5,col="#20B2AA")
points(xrB$respira.tmedmov2,xrB$lotemp16,type="l",lty=2,lwd=0.5,col="#20B2AA")

legend(21,-3, bty="n", lwd=2, col=c("#BF3EFF","#68228B","#20B2AA"), legend=c("Série temporal", "Case-crossover - dia da semana","Case-crossover - poluente"),ncol=1,cex=0.7)

dev.off()

#respira+o3

pdf(file="temp_effect6.pdf",onefile=T)

plot(0, type="n", bty="n", main="", xlab=expression("Temperatura ("~degree~C~")"),ylim=c(-6,12), lwd=1.5, xlim=c(5,30),ylab="Mudança percentual no risco (%)")
#mtext("Função de alisamento para Temp. média",3,line=2.5,cex=1.0)
#mtext("móvel 2 dias",3,line=1.5,cex=1.0)
#mtext(expression("doenças respiratórias - modelo O"[3]),3,line=0.1,cex=0.8)
abline(h=0,col="black",lwd=1.5,lty=2)

polygon(c(xrC$respira.tmedmov2,rev(xrC$respira.tmedmov2)),c(xrC$lotemp11,rev(xrC$hitemp11)),col="#FF3E9655"
,border=NA)
points(xrC$respira.tmedmov2,xrC$temp11,type="l",lwd=2.5,col="#FF3E96")
polygon(c(xrC$respira.tmedmov2,rev(xrC$respira.tmedmov2)),c(xrC$lotemp12,rev(xrC$hitemp12)),col="#83225255"
,border=NA)
points(xrC$respira.tmedmov2,xrC$temp12,type="l",lwd=2.5,col="#832252")
points(xrC$respira.tmedmov2,xrC$temp18,type="l",lwd=2.5,col="#20B2AA")
points(xrC$respira.tmedmov2,xrC$hitemp18,type="l",lty=2,lwd=0.5,col="#20B2AA")
points(xrC$respira.tmedmov2,xrC$lotemp18,type="l",lty=2,lwd=0.5,col="#20B2AA")

legend(21,-3, bty="n", lwd=2, col=c("#FF3E96","#832252","#20B2AA"), legend=c("Série temporal", "Case-crossover - dia da semana","Case-crossover - poluente"),ncol=1,cex=0.7)

dev.off()

##### interactions ####

#cardio
cint_mp10<- gam(obitos_total~s(tmed,mp10_av_lag0)+s(index,fx=T,k=45)+as.factor(dow)+feriado+Urmin,data=cardio,family=poisson,na.action=na.exclude)
cint_no2<-gam(obitos_total~s(tmed,no2_av_lag0)+s(index,fx=T,k=45)+as.factor(dow)+feriado+Urmin,data=cardio,family=poisson,na.action=na.exclude)
cint_o3<-gam(obitos_total~s(tmed,o3_av_lag0mov2)+s(index,fx=T,k=45)+as.factor(dow)+feriado+Urmin,data=cardio,family=poisson,na.action=na.exclude)

#respira
rint_mp10<- gam(obitos_total~s(tmedmov2,mp10_av_lag0)+s(index,fx=T,k=45)+as.factor(dow)+feriado+Urmin,data=respira,family=poisson,na.action=na.exclude)
rint_no2<-gam(obitos_total~s(tmedmov2,no2_av_lag0)+s(index,fx=T,k=45)+as.factor(dow)+feriado+Urmin,data=respira,family=poisson,na.action=na.exclude)
rint_o3<-gam(obitos_total~s(tmedmov2,o3_av_lag5)+s(index,fx=T,k=45)+as.factor(dow)+feriado+Urmin,data=respira,family=poisson,na.action=na.exclude)

####extract plots###


png(file="intera_cardio1a.png")

plot(cint_mp10,select=1,pers=T,theta=50,phi=05,col="#FF8C00AA",ticktype="detailed",xlab="Temperatura",ylab="MP10",zlab="log")
#mtext(expression("Interação poluente-temperatura"),3,line=1,cex=1.2)
#mtext(expression("doenças cardiovasculares - Temp. média x MP"[10]),3,line=0.1)

dev.off()

png(file="intera_cardio1b.png")

plot(cint_mp10,select=1,pers=F,xlab=expression("Temperatura ("~degree~C~")"),ylab="MP10")
#mtext(expression("Interação poluente-temperatura (curvas de nível)"),3,line=2.7,cex=1.2)
#mtext(expression("doenças cardiovasculares - Temp. média x MP"[10]),3,line=1.9,cex=0.8)
#abline(h=150,lty=2,lwd=2,col="#1C86EE")
dev.off()

png(file="intera_cardio2a.png")
plot(cint_no2,select=1,pers=T,theta=50,phi=05,col="#BF3EFFAA",ticktype="detailed")
#mtext(expression("Interação poluente-temperatura"),3,line=1,cex=1.2)
#mtext(expression("doenças cardiovasculares - Temp. média x NO"[2]),3,line=0.1)

dev.off()

png(file="intera_cardio2b.png")

plot(cint_no2,select=1,pers=F,xlab=expression("Temperatura ("~degree~C~")"),ylab="NO2")
#mtext(expression("Interação poluente-temperatura (curvas de nível)"),3,line=2.7,cex=1.2)
#mtext(expression("doenças cardiovasculares - Temp. média x NO"[2]),3,line=1.9,cex=0.8)
#abline(h=320,lty=2,lwd=2,col="#1C86EE")

dev.off()

png(file="intera_cardio3a.png")

plot(cint_o3,select=1,pers=T,theta=50,phi=05,col="#FF3E96AA",ticktype="detailed")
#mtext(expression("Interação poluente-temperatura"),3,line=1,cex=1.2)
#mtext(expression("doenças cardiovasculares - Temp. média x O"[3]),3,line=0.1)

dev.off()

png(file="intera_cardio3b.png")

plot(cint_o3,select=1,pers=F,xlab=expression("Temperatura ("~degree~C~")"),ylab="O3")
#mtext(expression("Interação poluente-temperatura (curvas de nível)"),3,line=2.7,cex=1.2)
#mtext(expression("doenças cardiovasculares - Temp. média x O"[3]),3,line=1.9,cex=0.8)
#abline(h=160,lty=2,lwd=2,col="#1C86EE")

dev.off()

png(file="intera_respira1a.png")

plot(rint_mp10,select=1,pers=T,theta=50,phi=05,col="#FF8C00AA",ticktype="detailed")
#mtext(expression("Interação poluente-temperatura"),3,line=1,cex=1.2)
#mtext(expression("doenças respiratórias - Temp. média (2 dias) x MP"[10]),3,line=0.1)

dev.off()

png(file="intera_respira1b.png")
plot(rint_mp10,select=1,pers=F,xlab=expression("Temperatura ("~degree~C~")"),ylab="MP10")
#mtext(expression("Interação poluente-temperatura (curvas de nível)"),3,line=2.7,cex=1.2)
#mtext(expression("doenças respiratórias - Temp. média (2 dias) x MP"[10]),3,line=1.9,cex=0.8)

dev.off()

png(file="intera_respira2a.png")

plot(rint_no2,select=1,pers=T,theta=50,phi=05,col="#BF3EFFAA",ticktype="detailed")
#mtext(expression("Interação poluente-temperatura"),3,line=1,cex=1.2)
#mtext(expression("doenças respiratórias - Temp. média (2 dias)x NO"[2]),3,line=0.1)

dev.off()

png(file="intera_respira2b.png")

plot(rint_no2,select=1,pers=F,xlab=expression("Temperatura ("~degree~C~")"),ylab="NO2")
#mtext(expression("Interação poluente-temperatura (curvas de nível)"),3,line=2.7,cex=1.2)
#mtext(expression("doenças respiratórias - Temp. média (2 dias)x NO"[2]),3,line=1.9,cex=0.8)

dev.off()

png(file="intera_respira3a.png")

plot(rint_o3,select=1,pers=T,theta=50,phi=05,col="#FF3E96AA",ticktype="detailed")
#mtext(expression("Interação poluente-temperatura"),3,line=1,cex=1.2)
#mtext(expression("doenças cardiovasculares - Temp. média (2 dias) x O"[3]),3,line=0.1)

dev.off()

png(file="intera_respira3b.png")

plot(rint_o3,select=1,pers=F,xlab=expression("Temperatura ("~degree~C~")"),ylab="O3")
#mtext(expression("Interação poluente-temperatura (curvas de nível)"),3,line=2.7,cex=1.2)
#mtext(expression("doenças cardiovasculares - Temp. média (2 dias)x O"[3]),3,line=1.9,cex=0.8)

dev.off()

p_int<-data.frame(summary(cint_mp10)$s.table[1,4],summary(cint_no2)$s.table[1,4],summary(cint_o3)$s.table[1,4],summary(rint_mp10)$s.table[1,4],summary(rint_no2)$s.table[1,4],summary(rint_o3)$s.table[1,4])

#extract p value
write.csv(p_mp10,file="p_mp10.csv")
write.csv(p_no2,file="p_no2.csv")
write.csv(p_o3,file="p_o3.csv")
write.csv(p_temp,file="p_temp.csv")
write.csv(p_temp_2,file="p_temp2.csv")
write.csv(p_int,file="p_int.csv")



predtemp19<-predict(cint_mp10,type='terms',se.fit=TRUE)
predtemp20<-predict(cint_no2,type='terms',se.fit=TRUE)
predtemp21<-predict(cint_o3,type='terms',se.fit=TRUE)
predtemp22<-predict(rint_mp10,type='terms',se.fit=TRUE)
predtemp23<-predict(rint_no2,type='terms',se.fit=TRUE)
predtemp24<-predict(rint_o3,type='terms',se.fit=TRUE)

temp19<-predtemp19$fit[,4]
temp20<-predtemp20$fit[,4]
temp21<-predtemp21$fit[,4]
temp22<-predtemp22$fit[,4]
temp23<-predtemp23$fit[,4]
temp24<-predtemp24$fit[,4]


lotemp19<-(exp(predtemp19$fit[,4]-1.96*predtemp19$se.fit[,4])-1)*100
lotemp20<-(exp(predtemp20$fit[,4]-1.96*predtemp20$se.fit[,4])-1)*100
lotemp21<-(exp(predtemp21$fit[,4]-1.96*predtemp21$se.fit[,4])-1)*100
lotemp22<-(exp(predtemp22$fit[,4]-1.96*predtemp19$se.fit[,4])-1)*100
lotemp23<-(exp(predtemp23$fit[,4]-1.96*predtemp19$se.fit[,4])-1)*100
lotemp24<-(exp(predtemp24$fit[,4]-1.96*predtemp19$se.fit[,4])-1)*100

hitemp19<-(exp(predtemp19$fit[,4]+1.96*predtemp19$se.fit[,4])-1)*100
hitemp20<-(exp(predtemp20$fit[,4]+1.96*predtemp20$se.fit[,4])-1)*100
hitemp21<-(exp(predtemp21$fit[,4]+1.96*predtemp21$se.fit[,4])-1)*100
hitemp22<-(exp(predtemp22$fit[,4]+1.96*predtemp19$se.fit[,4])-1)*100
hitemp23<-(exp(predtemp23$fit[,4]+1.96*predtemp19$se.fit[,4])-1)*100
hitemp24<-(exp(predtemp24$fit[,4]+1.96*predtemp19$se.fit[,4])-1)*100

oi<-data.frame(cardio$tmed,cardio$tmedmov2,cardio$mp10_av_lag0,cardio$no2_av_lag0,cardio$o3_av_lag0mov2,cardio$o3_av_lag5,temp19,temp20,temp21,temp22,temp23,temp24)
write.table(oi,file="samya3d.txt",sep=" ")
#row.names=F,na="")
oi<-oi[is.na(oi$temp19)==F,]
oi<-oi[order(oi[,1]),]
oi2<-oi[order(oi[,2]),]

xc<-xc[order(xc$cardio.tmed),]

la<-predict.matrix(cint_mp10,data=cardio)

persp(oi[,1],oi2[,2],oi[,3])
