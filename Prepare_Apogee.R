#Processes apogee data. Currently just from 2016.

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

rm("syv.ap.2016","syv.ap.2016.dat","syv.ap.30")

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



