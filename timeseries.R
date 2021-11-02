#*TIME SERIES*#

#### CARDIO ######

##Sazonalidade

time_1<-gam(obitos_total~s(index,fx=FALSE,bs="cr")+as.factor(dow)+feriado,data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")	#gcv/aic/ubre decide df=8.898

time_2<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado,data=cardio,family=poisson,na.action=na.exclude)	#df=4/year

time_3<-gam(obitos_total~s(index,fx=T,k=56)+as.factor(dow)+feriado,data=cardio,family=poisson,na.action=na.exclude)		#df=5/year

##Clima

#tmed
temptime_a1<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmed,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")		
temptime_a1beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmed,fx=T,k=5),data=cardio,family=poisson,na.action=na.exclude)		

temptime_a2<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov2,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")
temptime_a2beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov2,fx=T,k=5),data=cardio,family=poisson,na.action=na.exclude)

temptime_a3<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov3,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")

temptime_a4<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov4,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")

temptime_a5<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov5,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")

temptime_a6<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov6,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")

temptime_a7<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov7,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")
temptime_a7beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov7,fx=T,k=5),data=cardio,family=poisson,na.action=na.exclude)

#tmin
temptime_b1<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmin,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")		
temptime_b1beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmin,fx=T,k=5),data=cardio,family=poisson,na.action=na.exclude)		

temptime_b2<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tminmov2,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")
temptime_b2beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tminmov2,fx=T,k=5),data=cardio,family=poisson,na.action=na.exclude)

temptime_b3<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tminmov3,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")

temptime_b4<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tminmov4,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")

temptime_b5<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tminmov5,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")

temptime_b6<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tminmov6,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")

temptime_b7<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tminmov7,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")
temptime_b7beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tminmov7,fx=T,k=5),data=cardio,family=poisson,na.action=na.exclude)

#tmax
temptime_c1<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmax,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")		
temptime_c1beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmax,fx=T,k=5),data=cardio,family=poisson,na.action=na.exclude)		

temptime_c2<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmaxmov2,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")
temptime_c2beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmaxmov2,fx=T,k=5),data=cardio,family=poisson,na.action=na.exclude)

temptime_c3<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmaxmov3,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")

temptime_c4<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmaxmov4,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")

temptime_c5<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmaxmov5,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")

temptime_c6<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmaxmov6,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")

temptime_c7<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmaxmov7,fx=F,bs="cr"),data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")
temptime_c7beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmaxmov7,fx=T,k=5),data=cardio,family=poisson,na.action=na.exclude)

#Ur

#Urnoon
urtemptime_a1<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmed,fx=F,bs="cr")+Urnoon,data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")		
urtemptime_a1beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmed,fx=T,k=5)+Urnoon,data=cardio,family=poisson,na.action=na.exclude)		

urtemptime_a2<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov2,fx=F,bs="cr")+Urnoon,data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")
urtemptime_a2beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov2,fx=T,k=5)+Urnoon,data=cardio,family=poisson,na.action=na.exclude)

urtemptime_a7<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov7,fx=F,bs="cr")+Urnoon,data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")
urtemptime_a7beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov7,fx=T,k=5)+Urnoon,data=cardio,family=poisson,na.action=na.exclude)

urmtemptime_a1<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmed,fx=F,bs="cr")+Urnoonmov2,data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")		
urmtemptime_a1beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmed,fx=T,k=5)+Urnoonmov2,data=cardio,family=poisson,na.action=na.exclude)		

urmtemptime_a2<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov2,fx=F,bs="cr")+Urnoonmov2,data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")
urmtemptime_a2beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov2,fx=T,k=5)+Urnoonmov2,data=cardio,family=poisson,na.action=na.exclude)

urmtemptime_a7<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov7,fx=F,bs="cr")+Urnoonmov2,data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")
urmtemptime_a7beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov7,fx=T,k=5)+Urnoonmov2,data=cardio,family=poisson,na.action=na.exclude)

#Urmin

uritemptime_a1<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmed,fx=F,bs="cr")+Urmin,data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")		
uritemptime_a1beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmed,fx=T,k=5)+Urmin,data=cardio,family=poisson,na.action=na.exclude)		

uritemptime_a2<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov2,fx=F,bs="cr")+Urmin,data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")
uritemptime_a2beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov2,fx=T,k=5)+Urmin,data=cardio,family=poisson,na.action=na.exclude)

uritemptime_a7<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov7,fx=F,bs="cr")+Urmin,data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")
uritemptime_a7beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov7,fx=T,k=5)+Urmin,data=cardio,family=poisson,na.action=na.exclude)

urimtemptime_a1<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmed,fx=F,bs="cr")+Urminmov2,data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")		
urimtemptime_a1beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmed,fx=T,k=5)+Urminmov2,data=cardio,family=poisson,na.action=na.exclude)		

urimtemptime_a2<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov2,fx=F,bs="cr")+Urminmov2,data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")
urimtemptime_a2beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov2,fx=T,k=5)+Urminmov2,data=cardio,family=poisson,na.action=na.exclude)

urimtemptime_a7<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov7,fx=F,bs="cr")+Urminmov2,data=cardio,family=poisson,na.action=na.exclude,method="GACV.Cp")
urimtemptime_a7beta<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov7,fx=T,k=5)+Urminmov2,data=cardio,family=poisson,na.action=na.exclude)

#Modelos finais

a1<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmed,fx=T,k=5)+Urmin,data=cardio,family=poisson,na.action=na.exclude)
a2<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmin,fx=T,k=5)+Urmin,data=cardio,family=poisson,na.action=na.exclude)
a3<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmax,fx=T,k=5)+Urmin,data=cardio,family=poisson,na.action=na.exclude)

b1<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov2,fx=T,k=5)+Urminmov2,data=cardio,family=poisson,na.action=na.exclude)
b2<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tminmov2,fx=T,k=5)+Urminmov2,data=cardio,family=poisson,na.action=na.exclude)
b3<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmaxmov2,fx=T,k=5)+Urminmov2,data=cardio,family=poisson,na.action=na.exclude)

c1<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov7,fx=T,k=5)+Urminmov2,data=cardio,family=poisson,na.action=na.exclude)
c2<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tminmov7,fx=T,k=5)+Urminmov2,data=cardio,family=poisson,na.action=na.exclude)
c3<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmaxmov7,fx=T,k=5)+Urminmov2,data=cardio,family=poisson,na.action=na.exclude)


#Poluentes

a1_mp10<-gam(obitos_total~mp10_av_lag0+s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmed,fx=T,k=4)+Urmin,data=cardio,family=poisson,na.action=na.exclude)
summary(a1_mp10)
a1_mp10$aic

b1_mp10<-gam(obitos_total~mp10_av_lag0mov2+s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov2,fx=F,bs="cr")+Urminmov2,data=cardio,family=poisson,na.action=na.exclude)
summary(b1_mp10)
b1_mp10$aic

c1_mp10<-gam(obitos_total~mp10_av_lag0mov5+s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov7,fx=F,bs="cr")+Urminmov2,data=cardio,family=poisson,na.action=na.exclude)
summary(c1_mp10)
c1_mp10$aic




###### RESPIRA #####

a1<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmedmov2,fx=F, bs='cr')+Urmin,data=respira,family=poisson,na.action=na.exclude)
a2<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmin,fx=F, bs='cr')+Urmin,data=respira,family=poisson,na.action=na.exclude)
a3<-gam(obitos_total~s(index,fx=T,k=45)+as.factor(dow)+feriado+s(tmax,fx=F, bs='cr')+Urmin,data=respira,family=poisson,na.action=na.exclude)



