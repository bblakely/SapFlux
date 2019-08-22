if(!exists('wcr.twr')){
library(bigleaf)
source('Prepare_Data.R')
source('Refine_TowerData.R')
}

site<-"wcr"
if(site=="syv"){dat<-syv.twr};if(site=='wcr'){dat<-wcr.twr}



#Copy tower data for modification
dat.leaf<-dat

#####Leaf area####

LAI<-read.csv('LAI_2016_2017.csv')
LAI.2016<-LAI[LAI$YEAR==2016,]

#Reshape LAI data to daily

lai.wcr<-approx(LAI.2016$DOY,LAI.2016$WCR,n=126)
lai.wcrm<-approx(LAI.2016$DOY,LAI.2016$WCRM,n=126)
lai.syv<-approx(LAI.2016$DOY,LAI.2016$SYV, n=135)
lai.und<-approx(LAI.2016$DOY,LAI.2016$UND, n=96)

doys<-seq(130,264)
syv.l<-data.frame(cbind(doys,lai.syv$y))

doys<-seq(139,264)
wcr.l<-data.frame(cbind(doys,lai.wcr$y))
wcrm.l<-data.frame(cbind(doys,lai.wcrm$y))

doys<-seq(169,264)
und.l<-data.frame(cbind(doys,lai.und$y))

lai.1<-merge(syv.l,wcr.l, by="doys",all.x=TRUE,all.y=TRUE)
lai.2<-merge(lai.1,wcrm.l, by="doys",all.x=TRUE,all.y=TRUE)
lai.3<-merge(lai.2,und.l, by="doys",all.x=TRUE,all.y=TRUE)

LAI.dat<-lai.3
colnames(LAI.dat)<-c('DOY','SYV',"WCR","WCRM","UND")

rm('lai.1','lai.2','lai.3','und.l','wcr.l','syv.l','wcrm.l','lai.wcr','lai.wcrm','lai.und','lai.syv')
#####

if(site=='syv'){LAI<-mean(LAI.dat$SYV[LAI.dat$DOY%in%c(150:250)], na.rm=TRUE)}
if(site=='wcr'){LAI<-mean(LAI.dat$WCR[LAI.dat$DOY%in%c(150:250)], na.rm=TRUE)}

#Name changing function
namerep<-function(dat, orig, new){
  vec<-colnames(dat)
  vec[vec==orig]<-new
  colnames(dat)<-vec
  return(dat)}

#Renamings for PT
dat.leaf<-namerep(dat.leaf, "TA_1", "Tair")
dat.leaf<-namerep(dat.leaf, "PA_1", 'pressure')
dat.leaf<-namerep(dat.leaf, "NETRAD_1", 'Rn')
dat.leaf<-namerep(dat.leaf, "VPD_PI_1", 'VPD'); dat.leaf$VPD<-dat.leaf$VPD*0.1
dat.leaf$S<-dat.leaf$SH_PI_F_1 + dat.leaf$SLE_PI_F_1

ET<-potential.ET(dat.leaf, approach="Priestley-Taylor")
plot(ET$LE_pot)
points(dat.leaf$LE_1, col='blue')

#Penman-Monteith

#Roughness params

dat.leaf<-namerep(dat.leaf, "WS_1", 'wind')
dat.leaf<-namerep(dat.leaf, "USTAR_1", 'ustar')
dat.leaf<-namerep(dat.leaf, "H_1", 'H')
dat.leaf<-namerep(dat.leaf, "LE_1", 'LE')

if(site=='syv'){
zh<-26 #27m high canopy; Desai 2004 reports 20 - 27
zr<-36 #36m instrument mounting height, as reported in Desai 2004
hs<-0.026 #roughness of soil. Chose "Level terrain, low shrubs" from Hansen 1998. Makes no difference; I think the LAI is too high for soil to matter
}

if(site=='wcr'){
zh<-24 #24m high canopy; Desai 2004
zr<-30 #36m instrument mounting height, as reported in Desai 2004
hs<-0.026 
}


rough.ht<-roughness.parameters(method="canopy_height", zh=zh,LAI=LAI, zr=zr)
rough.lai<-roughness.parameters(method="canopy_height&LAI", zh=zh,LAI=LAI, zr=zr, hs=hs)

#Aero conductance

d<-rough.lai$d
z0m<-rough.lai$z0m

rh<-aerodynamic.conductance(data=dat.leaf, Rb_model="Thom_1972")$Ga_h
rh.wp<-aerodynamic.conductance(data=dat.leaf, zr=zr,zh=zh,d=d,z0m=z0m, wind_profile = TRUE, Rb_model="Thom_1972")$Ga_h
rh.c<-aerodynamic.conductance(data=dat.leaf, LAI=LAI, zr=zr,zh=zh,d=d,z0m=z0m,Dl=0.1, Rb_model="Choudhury_1988")$Ga_h
rh.c.wp<-aerodynamic.conductance(data=dat.leaf, LAI=LAI, zr=zr,zh=zh,d=d,z0m=z0m,Dl=0.1,wind_profile = TRUE,Rb_model="Choudhury_1988")$Ga_h

#(surface conductance)
rw<-surface.conductance(data=dat.leaf,S=dat.leaf$S, Ga=rh)$Gs_mol

#assigned by site at this point

plot(syv.rw[syv.rw>0], ylim=c(0,5));points(wcr.rw[wcr.rw>0], col='blue')


syv.rw.cl<-syv.rw[which(syv.rw>quantile(syv.rw, 0.01, na.rm=TRUE) & syv.rw<quantile(syv.rw, 0.99, na.rm=TRUE))]; syv.rw.cl[syv.rw.cl<0]<-NA
wcr.rw.cl<-wcr.rw[which(wcr.rw>quantile(wcr.rw, 0.01, na.rm=TRUE) &wcr.rw<quantile(wcr.rw, 0.99, na.rm=TRUE))]; wcr.rw.cl[wcr.rw.cl<0]<-NA

hist(syv.rw.cl, xlim=c(-2,2)); hist(wcr.rw.cl, xlim=c(-2,2));
median(syv.rw.cl); median(wcr.rw.cl, na.rm=TRUE)
mean(syv.rw.cl, na.rm=TRUE); mean(wcr.rw.cl, na.rm=TRUE)

#Penman-monteith potential
Gs<-quantile(rw, 0.9, na.rm=TRUE)

PM<-potential.ET(data=dat.leaf,S=dat.leaf$S,Ga=rh,Gs_pot = Gs, approach="Penman-Monteith")
plot(PM$LE_pot);points(dat$LE_1, col='blue')
#plot(PM$LE_pot~dat$LE_1);abline(0,1, col='red')

if(site=='syv'){PM.syv<-PM$LE_pot;LE.syv<-dat$LE_1}
if(site=='wcr'){PM.wcr<-PM$LE_pot;LE.wcr<-dat$LE_1}


plot(PM.syv[daygs])
points(PM.wcr[daygs], col='blue')

plot(PM.syv[daygs]~PM.wcr[daygs])
abline(0,1,col='red')

plot(LE.syv[daygs]~LE.wcr[daygs])
abline(0,1,col='red')

plot(PM.syv~syv.twr$DOY, xlab='DOY', ylab='syv');points(LE.syv~syv.twr$DOY, col='blue')
plot(PM.wcr~wcr.twr$DOY, xlab='DOY', ylab='wcr');points(LE.wcr~wcr.twr$DOY, col='blue')



miss.syv<-PM.syv[daygs]-LE.syv[daygs];miss.wcr<-PM.wcr[daygs]-LE.wcr[daygs]
plot(miss.syv~miss.wcr)
abline(0,1,col='red')
#Sylvania is further from its potential ET than WCR


#Gs from sap flux
source('Calc_Sapflow_Full.R')
syv.totals<-rowMeans(syv.gap)

y<-psychrometric.constant(Tair=syv.twr$TA_1,pressure=syv.twr$PA_1) 
lam<-latent.heat.vaporization(Tair=syv.twr$TA_1)
rho<-air.density(Tair=syv.twr$TA_1, pressure=syv.twr$PA_1, constants = bigleaf.constants())
cp<-1004.834
D<-syv.twr$VPD_PI_1*0.1
Leaf<-mean(LAI.dat$SYV[LAI.dat$DOY%in%c(150:250)])*6400; sa.p<-(sum(syv.forest$SWA/10000)*1.27)

#Equation from pataki
Gs<-(y*lam*syv.totals)/(rho*cp*D*(Leaf/sa.p))
Gs[Gs==Inf]<-NA
Gs[which(Gs>quantile(Gs, 0.99, na.rm=TRUE)| Gs<quantile(Gs, 0.01, na.rm=TRUE))]<-NA
hist(Gs[daygs])

#EWUE
#ewue.wcr<-wcr.twr$GPP_PI_F[daygs]/LE.to.ET(wcr.twr$LE_1[daygs], wcr.twr$TA_1[daygs]); plot(ewue.wcr, ylim=quantile(ewue.wcr, c(0.01,0.99), na.rm=TRUE))
#ewue.syv<-syv.twr$GPP_PI_F[daygs]/LE.to.ET(syv.twr$LE_1[daygs], syv.twr$TA_1[daygs]); plot(ewue.syv, ylim=quantile(ewue.wcr, c(0.01,0.99), na.rm=TRUE))

#Rc SW

bigts<-read.csv('WCR_Tower_2015_2017.csv')
carb<-read.csv('WCR_Tower_ex.csv')$CO2_PI_F_1_1_1[bigts$YEAR=="2016"]
g1<-2
rs.sw.syv<-(1.6*(1+(g1/sqrt(syv.twr$VPD_PI_1*0.1)))*(syv.twr$GPP_PI_F/carb))^-1;rs.sw.syv[rs.sw.syv==Inf]<-NA
rs.sw.wcr<-(1.6*(1+(g1/sqrt(wcr.twr$VPD_PI_1*0.1)))*(wcr.twr$GPP_PI_F/carb))^-1;rs.sw.wcr[rs.sw.wcr==Inf]<-NA

smoothScatter(rs.sw.wcr[daygs], ylim=c(0,30))
smoothScatter(rs.sw.syv[daygs])

median(rs.sw.wcr[daygs]-rs.sw.syv[daygs], na.rm=TRUE)

#uWUE
library(quantreg)

syv.uwu<-syv.twr$GPP_PI_F*((syv.twr$VPD_PI_1*0.1)^0.5)
plot(syv.uwu~LE.to.ET(syv.twr$LE_1, Tair = syv.twr$TA_1))
quant.syv<-rq(syv.uwu~LE.to.ET(syv.twr$LE_1, Tair = syv.twr$TA_1), tau=0.95);abline(coef(quant.syv)[1],coef(quant.syv)[2], col='red')


wcr.uwu<-wcr.twr$GPP_PI_F*((syv.twr$VPD_PI_1*0.1)^0.5)
plot(wcr.uwu~LE.to.ET(wcr.twr$LE_1, Tair = wcr.twr$TA_1))
quant.wcr<-rq(wcr.uwu~LE.to.ET(wcr.twr$LE_1, Tair = wcr.twr$TA_1), tau=0.95); abline(coef(quant.wcr)[1],coef(quant.wcr)[2], col='red')


daytest<-which(wcr.twr$DOY%in%c(200, 208))

plot(wcr.uwu[daytest]~LE.to.ET(wcr.twr$LE_1, Tair = wcr.twr$TA_1)[daytest])
dayfit<-(lm(wcr.uwu[daytest]~LE.to.ET(wcr.twr$LE_1, Tair = wcr.twr$TA_1)[daytest]))
coef(dayfit)[2]/coef(quant.wcr)[2]

plot(syv.uwu[daytest]~LE.to.ET(syv.twr$LE_1, Tair = syv.twr$TA_1)[daytest])
dayfit<-(lm(syv.uwu[daytest]~LE.to.ET(syv.twr$LE_1, Tair = syv.twr$TA_1)[daytest]))
coef(dayfit)[2]/coef(quant.syv)[2]

