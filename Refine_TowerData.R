#These are the same tower data refining steps from TS_analyses
#I want them in a separate script so I can call it for other purposes.

library(Hmisc) #sometimes you must run this manually (???)
library(zoo)

#source('Calc_Sapflow_Full.R')
if(exists("wcr.master")==FALSE){source('Prepare_Data.R')}


gs<-c(150:250) #growing season days

#Set dataframes
wcr.twr<-wcr.twr.2016
syv.twr<-syv.twr.2016

syv.twr.gs<-syv.twr[syv.twr$DOY%in%gs,]
wcr.twr.gs<-wcr.twr[wcr.twr$DOY%in%gs,]

#syv.sap.all.1<-syv.sap.all
#wcr.sap.all.1<-wcr.sap.all

#syv.sap.all[syv.sap.all==0]<-NA
#wcr.sap.all[wcr.sap.all==0]<-NA

#wcr.td[wcr.td>8]<-NA
#syv.td[syv.td>8]<-NA


#Indices used throughout 
dayhr<-c(8:20)
midday<-c(12:15)
dayind<-which(syv.twr$HOUR%in%dayhr) #made earlier; daytime hours
daymid<-which(syv.twr$HOUR%in%midday)

gsind<-which(syv.twr$DOY%in%gs)

daygs<-which(syv.twr$HOUR%in%dayhr & syv.twr$DOY%in%gs)
midgs<-which(syv.twr$HOUR%in%midday & syv.twr$DOY%in%gs)

meas.col<-c(10,11,14:23,25, 27:30) #Columns with non-filled data.

####DF QC#####

#Function to check out missing data
histmiss<-function(dat){
  par(mfrow=c(1,2))
  for(i in meas.col){
    daylim<-length(which(dat$HOUR==12))
    if(length(which(is.na(dat[,i])))/nrow(dat)>0.1){
      hist(dat$HOUR[which(is.na(dat[,i]))], main=colnames(dat)[i], breaks=seq(from=0,to=23,length.out=25), ylim=c(0,1.1*daylim), xlab='hour')
      abline(h=daylim)
      hist(dat$HOUR[which(is.na(dat[,i]) & !is.na(rowMeans(dat[meas.col[3:17]], na.rm=TRUE)))], breaks=seq(from=0,to=23,length.out=25), main=colnames(dat)[i], add=T, col='purple4', ylim=c(0,1.1*daylim), xlab=n)
      abline(h=daylim)
      hist(dat$DOY[which(is.na(dat[,i]))], main=colnames(dat)[i], breaks=seq(from=1,to=380, by=14), ylim=c(0,672*1.1), xlab="DOY")
      abline(h=672)
      hist(dat$DOY[which(is.na(dat[,i]) & !is.na(rowMeans(dat[meas.col[3:17]], na.rm=TRUE)))], breaks=seq(from=1,to=380, by=14), main=colnames(dat)[i], add=T, col='black', ylim=c(0,672*1.1), xlab=n)
      abline(h=672)
    } else {print(paste((colnames(dat)[i]), "is missing less than 10% of data"))}
    
  }
}

#First pass; raw tower data
histmiss(wcr.twr)
histmiss(syv.twr)


####Basic flux filtering####
#Ustar filtering
syv.twr$LE_1[syv.twr$USTAR_1<0.325]<-NA #.325 is from Ankur's paper on this 2005
wcr.twr$LE_1[wcr.twr$USTAR_1<0.3]<-NA  #Same for wcr

syv.twr$H_1[syv.twr$USTAR_1<0.325]<-NA #.325 is from Ankur's paper on this 2005
wcr.twr$H_1[wcr.twr$USTAR_1<0.3]<-NA  #Same for wcr

#Directional filtering SYV

syv.twr$LE_1[syv.twr$WD>30 & syv.twr$WD<90]<-NA
syv.twr$H_1[syv.twr$WD>30 & syv.twr$WD<90]<-NA

histmiss(wcr.twr)
histmiss(syv.twr)
#####

####gapfill WCR LE in winter####
#Create gapfill for WCR LE gaps in winter/spring
wcr.hole<-which(is.na(wcr.twr$LE_1[1:6000]))
regfill<-coef(lm(syv.twr$LE_1[1:6000]~0+wcr.twr$LE_1[1:6000]))
wcr.le<-wcr.twr$LE_1
wcr.le[wcr.hole]<-syv.twr$LE_1[wcr.hole]*regfill[1] #+ regfill[1]
plot(wcr.twr$LE_1)
points(wcr.le, col='light blue', pch=18)

#Set LE to gapfilled LE
wcr.twr$LE_1<-wcr.le #Put in filled LE for WCR

#Reset WCR nighttime SW_in NA's to 0 as per consultation with Ankur Desai

wcr.twr$SW_IN[is.na(wcr.twr$SW_IN) & syv.twr$SW_IN<=0]<-0 #Crude, but makes sure only night data are zeroed. 157 day points (1.5%) also get zeroed

#mini-gapfill (1 hour limit)
wcr.twr<-data.frame(na.approx(wcr.twr, maxgap=2))
syv.twr<-data.frame(na.approx(syv.twr, maxgap=2))


histmiss(wcr.twr)
histmiss(syv.twr)


#####EB mis-balance####
syv.gap.eb<-syv.twr$NETRAD_1-(syv.twr$H_1+syv.twr$LE_1)
wcr.gap.eb<-wcr.twr$NETRAD_1-(wcr.twr$H_1+wcr.twr$LE_1)

calcdev<-function(gap, co=3){
  gap.sd<-sd(gap, na.rm=TRUE)
  gap.err<-which(abs(gap)>(gap.sd*co))
  return(gap.err)
}
syv.err<-calcdev(syv.gap.eb)
wcr.err<-calcdev(wcr.gap.eb)

wcr.res<-residuals(lm((wcr.twr$H_1+wcr.twr$LE_1)~wcr.twr$NETRAD_1, na.action=na.exclude))
wcr.res.sd<-sd(wcr.res, na.rm=TRUE)
wcr.err2<-which(abs(wcr.res)>wcr.res.sd*3)
syv.res<-residuals(lm((syv.twr$H_1+syv.twr$LE_1)~syv.twr$NETRAD_1, na.action=na.exclude))
syv.res.sd<-sd(syv.res, na.rm=TRUE)
syv.err2<-which(abs(syv.res)>syv.res.sd*3)

wcr.twr<-data.frame(na.approx(wcr.twr, maxgap=2))
syv.twr<-data.frame(na.approx(syv.twr, maxgap=2))

#####
plot(wcr.twr$NETRAD_1,(wcr.twr$H_1+wcr.twr$LE_1))
points(wcr.twr$NETRAD_1[wcr.err2],(wcr.twr$H_1+wcr.twr$LE_1)[wcr.err2], col='blue', pch=3, cex=0.7)
points(wcr.twr$NETRAD_1[wcr.err],(wcr.twr$H_1+wcr.twr$LE_1)[wcr.err], col='red', pch=20, cex=0.5)
abline(0,1, col='red')
abline(lm((wcr.twr$H_1+wcr.twr$LE_1)~wcr.twr$NETRAD_1), col='blue')

plot(syv.twr$NETRAD_1,(syv.twr$H_1+syv.twr$LE_1))
abline(0,1, col='red')
abline(lm((syv.twr$H_1+wcr.twr$LE_1)~syv.twr$NETRAD_1), col='blue')
points(syv.twr$NETRAD_1[syv.err2],(syv.twr$H_1+syv.twr$LE_1)[syv.err2], col='blue', pch=3, cex=0.7)
points(syv.twr$NETRAD_1[syv.err],(syv.twr$H_1+syv.twr$LE_1)[syv.err], col='red', pch=20, cex=0.5)
#####
syv.twr[syv.err2, c(10:11)]<-NA
wcr.twr[wcr.err2, c(10:11)]<-NA


histmiss(wcr.twr)
histmiss(syv.twr)


####NAN all non-shared values (costly; check data loss)####
syv.nans<-(sapply(syv.twr, function(y) sum(length(which(is.na(y))))))/nrow(syv.twr)
wcr.nans<-(sapply(wcr.twr, function(y) sum(length(which(is.na(y))))))/nrow(wcr.twr)

syv.twr[is.na(wcr.twr)]<-NA
wcr.twr[is.na(syv.twr)]<-NA

syv.nans.new<-(sapply(syv.twr, function(y) sum(length(which(is.na(y))))))/nrow(syv.twr)
wcr.nans.new<-(sapply(wcr.twr, function(y) sum(length(which(is.na(y))))))/nrow(wcr.twr)

syv.exloss<-syv.nans.new-syv.nans
barplot(sort(syv.exloss[syv.exloss>0.01]), ylim=c(0,0.5), las=2, main='SYV (wcr missing)')
wcr.exloss<-wcr.nans.new-wcr.nans
barplot(sort(wcr.exloss[wcr.exloss>0.01]), ylim=c(0,0.5), las=2, main='WCR (syv missing)')

#Mini-fill
wcr.twr<-data.frame(na.approx(wcr.twr, maxgap=2))
syv.twr<-data.frame(na.approx(syv.twr, maxgap=2))

histmiss(wcr.twr) #Both towers will have the same missing data now

#Clear out mid-season low LE at WCR####
#Latent

wcr.twr.gs<-wcr.twr[gsind,]
syv.twr.gs<-syv.twr[gsind,]

smoothScatter(wcr.twr.gs$LE_1~wcr.twr.gs$HOUR, ylim=c(-100,650))
smoothScatter(syv.twr.gs$LE_1~syv.twr.gs$HOUR, ylim=c(-100,650))

clipLE<-function(dat, cut=10, quant=0.05, hr=c(9:16), gs=c(150:250)){
  
  smoothScatter(dat$LE_1~dat$HOUR, ylim=c(-100,650), main='orig')
  dat$LE_1[abs(dat$LE_1)<cut & dat$HOUR%in%hr]<-NA
  qs<-rep(NA, 24)
  for(i in 1:24){
    qs[i]<-(quantile(dat$LE_1[wcr.twr.gs$HOUR==(i-1)], quant, na.rm=TRUE))
  }
  smoothScatter(dat$LE_1~dat$HOUR, ylim=c(-100,650), main='flat clip')
  lines(qs)
  
  
  qmin<-rep(qs,length(gs), each=2)
  print(paste(length(which(dat$LE_1<qmin)), 'points (', length(which(dat$LE_1<qmin))/length(dat$LE_1)*100, 'percent ) removed'))
  
  dat$LE_1[dat$LE_1<qmin]<-NA
  
  return(dat)
}

wcr.twr.clip<-clipLE(wcr.twr.gs, cut=25, hr=c(8:17), quant=0.05)
syv.twr.clip<-clipLE(syv.twr.gs, cut=25, hr=c(9:17), quant=0.05)

smoothScatter(wcr.twr.clip$LE_1~wcr.twr.clip$HOUR, ylim=c(-100,650))
smoothScatter(syv.twr.clip$LE_1~syv.twr.clip$HOUR, ylim=c(-100,650))

wcr.twr[gsind,]<-wcr.twr.clip
syv.twr[gsind,]<-syv.twr.clip


#mini-gapfill (1 hour limit)
wcr.twr<-data.frame(na.approx(wcr.twr, maxgap=2))
syv.twr<-data.frame(na.approx(syv.twr, maxgap=2))

histmiss(wcr.twr)
