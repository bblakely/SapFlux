SYVraw<-read.csv('SYV_AP_clean.csv')
WCRraw<-read.csv('WCR_AP_clean.csv')

start<-189
end<-260

#Correct for WCR lag
wcr.lag<-WCRraw
wcr.meta<-wcr.lag[,1:5]
wcr.data<-wcr.lag[,6:14]

#Take first hour of timeseries out 
wcr.meta<-wcr.meta[13:nrow(wcr.meta),]
#Take last hour of data out
wcr.data<-wcr.data[1:(nrow(wcr.data)-12),]
#Stick 'em back together
WCR.lag<-cbind(wcr.meta,wcr.data)


SYVcore<-SYVraw[SYVraw$DOY>=start &SYVraw$DOY<end,c(2:4, 7:10)]
WCRcore<-WCR.lag[WCR.lag$DOY>=start &WCR.lag$DOY<end,c(2:4,7:10)]


SYVcore$Dectime=(SYVcore$H)+((SYVcore$M)/60)
SYVcore$DecDay=(SYVcore$DOY)+(SYVcore$Dectime/24)
WCRcore$Dectime=(WCRcore$H)+((WCRcore$M)/60)
WCRcore$DecDay=(WCRcore$DOY)+(WCRcore$Dectime/24)


syv.temp<-SYVcore[SYVcore$M==0 | SYVcore$M==30,]
wcr.temp<-WCRcore[WCRcore$M==0 | WCRcore$M==30,]

#Some analyses
syv.tempavg<-rowMeans(syv.temp[,4:7], na.rm=TRUE)
wcr.tempavg<-rowMeans(wcr.temp[,4:7], na.rm=TRUE)

syv.rp.temp<-aggregate(syv.tempavg, by=list(syv.temp$Dectime), FUN=mean, na.rm=TRUE)
wcr.rp.temp<-aggregate(wcr.tempavg, by=list(wcr.temp$Dectime), FUN=mean, na.rm=TRUE)


wcr.rp.temp

syv.resid<-syv.tempavg-tower.match.syv$TA
wcr.resid<-wcr.tempavg-tower.match.wcr$TA

syv.rp<-aggregate(syv.resid, by=list(syv.temp$Dectime), FUN=mean, na.rm=TRUE)
wcr.rp<-aggregate(wcr.resid, by=list(wcr.temp$Dectime), FUN=mean, na.rm=TRUE)

par(mfrow=c(1,1), xpd=FALSE)
plot(syv.rp, ylim=c(-1.8,1.2), type='l', col='black', main='Surface T - Atmospheric T', lwd=3, xlab='Hour', ylab='Ts - Ta')
lines(wcr.rp, col='dark gray', lwd=3)
abline(h=0)
abline(v=c(5.5,13,20.5), lty=2)
legend(0,1,legend=c('SYV', 'WCR'), lwd=2,col=c('black', 'dark gray'), cex=0.9)


#smoothed
library(zoo)
syv.rp.sm<-rollapply(syv.rp,4, FUN=mean)
wcr.rp.sm<-rollapply(wcr.rp,4, FUN=mean)

par(mfrow=c(1,1), xpd=FALSE)
plot(syv.rp.sm, ylim=c(-1.8,1.2), type='l', col='black', main='Surface - Air', 
     lwd=5, xlab='Hour', ylab='TS - TA', font.lab=2,cex.lab=1.2, font.axis=2)
lines(wcr.rp.sm, col='dark gray', lwd=5)
abline(h=0)
#abline(v=c(5.5,13,20.5), lty=2)
legend(0.5,1.2,legend=c('Old', 'Second'), lwd=4,col=c('black', 'dark gray'), cex=0.8, text.font=2, x.intersp=0.5, bty='n')


#TA and TS plots for ESA extra slides
wcr.rp.ta<-aggregate(tower.match.wcr$TA, by=list(wcr.temp$Dectime), FUN=mean)
syv.rp.ta<-aggregate(tower.match.syv$TA, by=list(syv.temp$Dectime), FUN=mean)


#par(mfrow=c(2,1))
plot(rollapply(wcr.rp.ta,2,FUN=mean), type='l', lwd=3, col='dark gray', ylim=c(13.5,23), main='Air', 
     ylab='Air T (C)', xlab='Hour', font=2,font.lab=2,font.axis=2,cex.lab=1.2)
lines(rollapply(syv.rp.ta,2,FUN=mean), lwd=3, col='black')
legend(0,22.5,legend=c('Old', 'Second'), lwd=3,col=c('black', 'dark gray'), cex=0.8, text.font=2, x.intersp=0.5, bty='n')

plot(rollapply(wcr.rp.temp,2,FUN=mean), type='l', lwd=3, col='dark gray', ylim=c(13.5,23), main='Surface',
     ylab='Surface T (C)', xlab='Hour', font=2,font.lab=2,font.axis=2,cex.lab=1.2)
lines(rollapply(syv.rp.temp,2,FUN=mean), lwd=3, col='black')
legend(0,22.5,legend=c('Old', 'Second'), lwd=3,col=c('black', 'dark gray'), cex=0.8, text.font=2, x.intersp=0.5, bty='n')



# #Temporary
# for (i in 1:20){
#   plot(syv.flow[syv.flow$Dectime>6&syv.flow$Dectime<20,i+5], syv.resid[syv.flow$Dectime>6&syv.flow$Dectime<20], main=i, xlab='Flux', ylab='Ts - Ta')
#   print(summary(lm(syv.resid[syv.flow$Dectime>6&syv.flow$Dectime<20]~syv.flow[syv.flow$Dectime>6&syv.flow$Dectime<20,i+5])))
# }
# for (i in 1:14){
#   plot(wcr.flow[,i+5], wcr.resid, main=i, xlab='Flux', ylab='Ts - Ta')
#   print(summary(lm(wcr.resid~wcr.flow[,i+5])))
# }
