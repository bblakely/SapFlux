
#Preliminaries...
syv.ap.dense<-read.csv('SYV_AP.csv')

syv.ap.30<-syv.ap.dense[syv.ap.dense$M==0 | syv.ap.dense$M==30,]
rm(syv.ap.dense)

syv.ap.30$FLAG<-NA
syv.ap.30$FLAG[diff(syv.ap.30$M)!=30 & diff(syv.ap.30$M)!=(-30)]<-2
syv.ap.30$FLAG[diff(syv.ap.30$H)!=1 & diff(syv.ap.30$H)!=0 & diff(syv.ap.30$H)!=(-23)]<-3
#write.csv(syv.ap.30, 'SYV_AP_FLAG.csv')

#actually it's in great shape... Did not manually gapfill
syv.ap.2016<-syv.ap.30[4684:22251,]

syv.ap.2016.dat<-syv.ap.2016[7:10]
syv.ap.2016.dat[syv.ap.2016.dat>100 | syv.ap.2016.dat<(-100)]<-NA

syv.ap<-cbind(syv.ap.2016[1:6],syv.ap.2016.dat)

wcr.ap.dense<-read.csv('WCR_AP.csv')
wcr.ap.30<-wcr.ap.dense[wcr.ap.dense$M==0 | wcr.ap.dense$M==30,]
rm(wcr.ap.dense)

wcr.ap.30$FLAG<-NA
wcr.ap.30$FLAG[diff(wcr.ap.30$M)!=30 & diff(wcr.ap.30$M)!=(-30)]<-2
wcr.ap.30$FLAG[diff(wcr.ap.30$H)!=1 & diff(wcr.ap.30$H)!=0 & diff(wcr.ap.30$H)!=(-23)]<-3

#write.csv(wcr.ap.30, 'WCR_AP_FLAG.csv')

wcr.clean<-read.csv('WCR_AP_CLEAN1.csv')

wcr.ap.2016<-wcr.clean[4864:22431,]
wcr.ap.2016$DOY[11607]<-242  #One missed doy

wcr.ap.2016.dat<-wcr.ap.2016[7:10]
wcr.ap.2016.dat[wcr.ap.2016.dat>50 | wcr.ap.2016.dat<(-50)]<-NA


wcr.ap<-cbind(wcr.ap.2016[1:6],wcr.ap.2016.dat)
wcr.twr<-WCR.twr.2016 #From analyse hysteresis

wcr.temp.raw<-WCR.twr.2016$tair
wcr.temp<-wcr.temp.raw
wcr.temp[wcr.temp > 50 | wcr.temp < (-50)]<-NA
wcr.temp.day<-aggregate(wcr.temp, by=list(wcr.ts$DOY),FUN=mean)$x

wcr.surf<-as.numeric(rowMeans(wcr.ap[7:10]), na.rm=TRUE)
wcr.surf.day<-aggregate(wcr.surf,by=list(wcr.ts$DOY), FUN=mean)$x

wcr.fl.day<-rowSums(alltree.wcr.pd)

wcr.wind<-WCR.twr.2016$ws
wcr.wind[wcr.wind<0]<-NA
wcr.wind.day<-aggregate(wcr.wind, by=list(WCR.twr.2016$day), FUN=mean)$x

wcr.light<-WCR.twr.2016$par
wcr.light[wcr.light<0]<-NA
wcr.light.day<-aggregate(wcr.light, by=list(WCR.twr.2016$day), FUN=mean)$x

wcr.precip<-WCR.twr.2016$precipitation
wcr.precip[wcr.precip<0]<-NA
wcr.precip.day<-aggregate(wcr.precip, by=list(WCR.twr.2016$day), FUN=mean)$x


wcr.diff<-wcr.surf-wcr.temp

wcr.diff.profile<-aggregate(wcr.diff, by=list(WCR.twr.2016$hour), FUN=mean, na.rm=TRUE)
plot(wcr.diff.profile$x~wcr.diff.profile$Group.1, pch='*', col='gray', cex=2, xlab='Hour', ylab="TS - TA", font=2, font.lab=2, main= 'Surface Cooling')
abline(h=0, col='dark red', lty=2)
legend(0,1.4,legend="(Surface warmer)", cex=0.8,bty='n')
legend(13,-0.5, legend="(Air warmer)", cex=0.8, bty='n')

wcr.diff.day<-aggregate(wcr.diff, by=list(WCR.twr.2016$day), FUN=mean, na.rm=TRUE)$x
plot(wcr.diff.day, type='l')

gs<-c(130:250)

datset.fl<-ts(wcr.fl.day[gs])
datset.temp<-ts(wcr.diff.day[gs])
datset.wind<-ts(wcr.wind.day[gs])
datset.airt<-ts(wcr.temp.day[gs])
datset.light<-ts(wcr.light.day[gs])
datset.precip<-ts(wcr.precip.day[gs])


tmod<-lm(datset.temp~datset.fl+datset.wind+datset.light+datset.precip+datset.airt)
summary(tmod)

nov<-anova(tmod)
