#Call data prep
source('Prepare_Data.R')
source('Refine_TowerData.R')
source('Calc_Sapflow_Full.R')


#surface temperatures
syv.temp<-rowMeans(syv.master[7:10])
wcr.temp<-Lag(rowMeans(wcr.master[7:9]),-2)

syv.temp.copy<-syv.temp; wcr.temp.copy<-wcr.temp
syv.temp<-syv.temp+273.15; wcr.temp<-wcr.temp+273.15

syv.twr.copy<-syv.twr;wcr.twr.copy<-wcr.twr
syv.twr$TA_1<-syv.twr$TA_1+273.15;wcr.twr$TA_1<-wcr.twr$TA_1+273.15

#Juang temp partitioning

#Are the assumptions of the same SW, temp fulfilled?

#MAE temperature
sum(abs(syv.twr$TA_1-wcr.twr$TA_1), na.rm=TRUE)/length(which(!is.na(syv.twr$TA_1-wcr.twr$TA_1)))

#MAE shortwave
sum(abs(syv.twr$SW_IN-wcr.twr$SW_IN), na.rm=TRUE)/length(which(!is.na(syv.twr$SW_IN-wcr.twr$SW_IN)))
sum(abs(syv.twr$SW_IN[gsind]-wcr.twr$SW_IN[gsind]), na.rm=TRUE)/length(which(!is.na(syv.twr$SW_IN[gsind]-wcr.twr$SW_IN[gsind])))
sum(abs(syv.twr$SW_IN[daygs]-wcr.twr$SW_IN[daygs]), na.rm=TRUE)/length(which(!is.na(syv.twr$SW_IN[daygs]-wcr.twr$SW_IN[daygs])))
#yikes I think we have to subset

cutoff<-50 #Greatest tolerable difference in SWin, arbitrary for now

sim<-which(abs(syv.twr$SW_IN-wcr.twr$SW_IN)<cutoff) 
sum(abs(syv.twr$SW_IN[sim]-wcr.twr$SW_IN[sim]), na.rm=TRUE)/length(which(!is.na(syv.twr$SW_IN[sim]-wcr.twr$SW_IN[sim])))# MAE of remaining values remaining
1-(length(sim)/nrow(syv.twr)) #Data loss

off<-which(abs(syv.twr$SW_IN-wcr.twr$SW_IN)>cutoff)
syv.twr$SW_IN[off]<-NA; wcr.twr$SW_IN[off]<-NA
syv.twr$NETRAD_1[off]<-NA; wcr.twr$NETRAD_1[off]<-NA

#now that we've satisfied similar TA, SW values...

#percent clipping formula for when ratios get weird
pct.clip<-function(toclip, clip=0.01){
  bound<-quantile(toclip, c(clip, 1-clip), na.rm=TRUE)
  toclip[toclip<bound[1]|toclip>bound[2]]<-NA
  return(toclip)
}

#Nu's
syv.nu1<-(syv.twr$H_1/(syv.temp-syv.twr$TA_1))/(1-(syv.twr$LE_1/syv.twr$NETRAD_1))
syv.nu2<-(syv.twr$H_1/(syv.temp-syv.twr$TA_1))/(1+((syv.twr$H_1-syv.twr$NETRAD_1)/syv.twr$NETRAD_1))
syv.nu3<-(syv.twr$NETRAD_1/(syv.temp-syv.twr$TA_1))

wcr.nu1<-(wcr.twr$H_1/(wcr.temp-wcr.twr$TA_1))/(1-(wcr.twr$LE_1/wcr.twr$NETRAD_1))
wcr.nu2<-(wcr.twr$H_1/(wcr.temp-wcr.twr$TA_1))/(1+((wcr.twr$H_1-wcr.twr$NETRAD_1)/wcr.twr$NETRAD_1))
wcr.nu3<-(wcr.twr$NETRAD_1/(wcr.temp-wcr.twr$TA_1))

dnu1<-pct.clip(syv.nu1-wcr.nu1); dnu2<-pct.clip(syv.nu2-wcr.nu2); dnu3<-pct.clip(syv.nu3-wcr.nu3)

#Transpiration based nu's
#####
syv.sap.all<-rowSums(syv.mega, na.rm=TRUE)
syv.sap.all[syv.sap.all==0]<-NA
LE.sap.syv<-(syv.sap.all*2260*1000*1.37)/(6400*1800) #2260: spec. heat water KJ/kg (1kg = 1L); 1000: KJ to Joules; 1.37: surveyed area to total area. 6400: plot (80*80) to m2;  1800: 3o min to s

wcr.sap.all<-rowSums(wcr.mega, na.rm=TRUE)
syv.sap.all[wcr.sap.all==0]<-NA
LE.sap.wcr<-(wcr.sap.all*2260*1000*1.37)/(6400*1800)

syv.nutr1<-(syv.twr$H_1/(syv.temp-syv.twr$TA_1))/(1-(LE.sap.syv/syv.twr$NETRAD_1))
syv.nutr2<-(syv.twr$H_1/(syv.temp-syv.twr$TA_1))/(1+((syv.twr$H_1-syv.twr$NETRAD_1)/syv.twr$NETRAD_1))
syv.nutr3<-(syv.twr$NETRAD_1/(syv.temp-syv.twr$TA_1))

wcr.nutr1<-(wcr.twr$H_1/(wcr.temp-wcr.twr$TA_1))/(1-(LE.sap.wcr/wcr.twr$NETRAD_1))
wcr.nutr2<-(wcr.twr$H_1/(wcr.temp-wcr.twr$TA_1))/(1+((wcr.twr$H_1-wcr.twr$NETRAD_1)/wcr.twr$NETRAD_1))
wcr.nutr3<-(wcr.twr$NETRAD_1/(wcr.temp-wcr.twr$TA_1))

dnutr1<-pct.clip(syv.nutr1-wcr.nutr1); dnutr2<-pct.clip(syv.nutr2-wcr.nutr2); dnutr3<-pct.clip(syv.nutr3-wcr.nutr3)


#transpiration nu's
termtr1a<-(syv.twr$TA_1-syv.temp)*dnutr1; termtr1b<-(syv.twr$TA_1-syv.temp)*dnutr2; termtr1c<-(syv.twr$TA_1-syv.temp)*dnutr3; 
termtr1<-(-1)*(scalar.a*termtr1a+scalar.b*termtr1b+scalar.c*termtr1c)/3

#####



#Albedo
syv.alb<-syv.twr$SW_OUT/syv.twr$SW_IN; wcr.alb<-wcr.twr$SW_OUT/wcr.twr$SW_IN
syv.alb[syv.alb>1 | syv.alb<0]<-NA; wcr.alb[wcr.alb>1 | wcr.alb<0]<-NA

dalb<-(syv.alb-wcr.alb)


#Ground heat
#we're assuming change in G is 0, for now.

#Emissivity
e.syv<-0.987; e.wcr<-0.983 #Used albedo paper values for now
de<-rep(e.syv-e.wcr, 17568) 


#Okay first attempt
sb<-(5.67e-8)


scalar.a<-1/((4*sb*e.syv*syv.temp^3)+syv.nu1); scalar.b<-1/((4*sb*e.syv*syv.temp^3)+syv.nu2); scalar.c<-1/((4*sb*e.syv*syv.temp^3)+syv.nu3)
term1a<-(syv.twr$TA_1-syv.temp)*dnu1; term1b<-(syv.twr$TA_1-syv.temp)*dnu2; term1c<-(syv.twr$TA_1-syv.temp)*dnu3; 
term2<-syv.twr$SW_IN*dalb
term3<-0
term4<-sb*syv.temp^4*de


par(mfrow=c(1,3))
plot(pct.clip(scalar.a*term1a), main="term 1, nu1")
plot(pct.clip(scalar.b*term1b), main="term 1, nu2")
plot(pct.clip(scalar.c*term1c), main="term 1, nu3")

term1<-(-1)*(scalar.a*term1a+scalar.b*term1b+scalar.c*term1c)/3
term2<-(-1)*(scalar.a*term2+scalar.b*term2+scalar.c*term2)/3
term4<-(-1)*(scalar.a*term4+scalar.b*term4+scalar.c*term4)/3

plot(pct.clip(term1), main='turbulent'); mean(pct.clip(term1), na.rm=TRUE)
#hist(pct.clip(scalar.a*term1a+scalar.b*term1b+scalar.c*term1c)/3)
plot(pct.clip(term2), main='albedo'); mean(pct.clip(term2), na.rm=TRUE)
plot(pct.clip(term4), main='emissivity'); mean(pct.clip(term4), na.rm=TRUE)

#Put it alll together
DT<-(term1-term2-term4); mean(pct.clip(DT), na.rm=TRUE)
#DT.transp<-(termtr1-term2-term4);mean(pct.clip(DT.transp), na.rm=TRUE)
mean(pct.clip(DT)[daygs], na.rm=TRUE)


par(mfrow=c(1,2))
col=c('blue','yellow','red', 'black')

all<-term1-term2-term4
t1all<-mean(pct.clip(term1), na.rm=TRUE)
t2all<-mean(pct.clip(term2), na.rm=TRUE)
t4all<-mean(pct.clip(term4), na.rm=TRUE)
predall<-t1all-t2all-t4all
barplot(c(t1all,-t2all,-t4all, predall), main='all',ylim=c(-0.3,0.1), names.arg=c('turbulent','albedo','emissivity', 'predicted DT'), col=col)
abline(h=0);abline(v=3.7)

obsall<-mean(wcr.temp[which(!is.na(all))]-syv.temp[which(!is.na(all))], na.rm=TRUE)
obsall.td<-mean(wcr.td[which(!is.na(all))]-syv.td[which(!is.na(all))], na.rm=TRUE)

t1day<-mean(pct.clip(term1[dayind]), na.rm=TRUE)
t2day<--mean(pct.clip(term2)[dayind], na.rm=TRUE)
t4day<-mean(pct.clip(term4)[dayind], na.rm=TRUE)
#barplot(c(t1gs,-t2gs,-t4gs),main='all days',ylim=c(-0.2,0.05))

allgs<-term1[daygs]-term2[daygs]-term4[daygs]
realgs<-which(!is.na(allgs))

t1gs<-mean(pct.clip(term1)[daygs], na.rm=TRUE)
t2gs<-mean(pct.clip(term2)[daygs], na.rm=TRUE)
t4gs<-mean(pct.clip(term4)[daygs], na.rm=TRUE)
predgs<- (t1gs-t2gs-t4gs)
barplot(c(t1gs,-t2gs,-t4gs, predgs),main='growing season days',ylim=c(-0.3,0.1), names.arg=c('turbulent','albedo','emissivity', 'predicted DT'), col=col)
abline(h=0);abline(v=3.7)
obsgs<-mean(wcr.temp[which(!is.na(allgs))]-syv.temp[which(!is.na(allgs))], na.rm=TRUE)
obsgs.td<-mean(wcr.td[which(!is.na(allgs))]-syv.td[which(!is.na(allgs))], na.rm=TRUE)

