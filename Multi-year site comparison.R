#Multi-year site comparison
#Probablt depends on PM_can; was originally part of that script
library(bigleaf)


wcr.flux<-ET.to.LE(rowSums(wcr.mega)/(5024*1800), Tair=wcr.twr$TA_1)
plot(wcr.flux[wcr.twr$DOY%in%c(200:207)], type='l', ylim=c(0,500))
lines(wcr.twr$LE_1[wcr.twr$DOY%in%c(200:207)], col='blue')
lines(PM.wcr[wcr.twr$DOY%in%c(200:207)], type='l', col='red')

syv.flux<-ET.to.LE(rowSums(syv.mega)/(5024*1800), Tair=syv.twr$TA_1)
plot(syv.flux[syv.twr$DOY%in%c(200:207)], type='l', ylim=c(0,500))
lines(syv.twr$LE_1[syv.twr$DOY%in%c(200:207)], col='blue')
lines(PM.syv[syv.twr$DOY%in%c(200:207)], type='l', col='red')

gpp.syv.c<-umolCO2.to.gC(syv.twr$GPP_PI_F)/48
gpp.wcr.c<-umolCO2.to.gC(wcr.twr$GPP_PI_F)/48

syv.gpp.month<-aggregate(gpp.syv.c, by=list(syv.twr$MONTH), FUN='sum')
wcr.gpp.month<-aggregate(gpp.wcr.c, by=list(wcr.twr$MONTH), FUN='sum')
#plot(syv.gpp.month,pch=2); points(wcr.gpp.month, pch=1)

#Interannual variability?
wcr.twr.max<-read.csv('WCR_Tower_2015_2017.csv')
syv.twr.max<-read.csv('SYV_Tower_2015_2017.csv')


syv.twr.2015<-syv.twr.max[syv.twr.max$YEAR==2015,]; syv.twr.2015[syv.twr.2015==-9999]<-NA
wcr.twr.2015<-wcr.twr.max[wcr.twr.max$YEAR==2015,]; wcr.twr.2015[wcr.twr.2015==-9999]<-NA

gpp.syv.c<-umolCO2.to.gC(syv.twr.2015$GPP_PI_F)/48
gpp.wcr.c<-umolCO2.to.gC(wcr.twr.2015$GPP_PI_F)/48

syv.gpp.month.15<-aggregate(gpp.syv.c, by=list(syv.twr.2015$MONTH), FUN='sum')
wcr.gpp.month.15<-aggregate(gpp.wcr.c, by=list(wcr.twr.2015$MONTH), FUN='sum')

syv.twr.2017<-syv.twr.max[syv.twr.max$YEAR==2017,];syv.twr.2017[syv.twr.2017==-9999]<-NA
wcr.twr.2017<-wcr.twr.max[wcr.twr.max$YEAR==2017,];wcr.twr.2017[wcr.twr.2017==-9999]<-NA
gpp.syv.c<-umolCO2.to.gC(syv.twr.2017$GPP_PI_F)/48; 
gpp.wcr.c<-umolCO2.to.gC(wcr.twr.2017$GPP_PI_F)/48

syv.gpp.month.17<-aggregate(gpp.syv.c, by=list(syv.twr.2017$MONTH), FUN='sum', na.rm=TRUE)
wcr.gpp.month.17<-aggregate(gpp.wcr.c, by=list(wcr.twr.2017$MONTH), FUN='sum', na.rm=TRUE)


syv.LE.15<-aggregate(syv.twr.2015$LE_1_1_1, by=list(syv.twr.2015$MONTH), FUN=mean, na.rm=TRUE)
wcr.LE.15<-aggregate(wcr.twr.2015$LE_1_1_1, by=list(wcr.twr.2015$MONTH), FUN=mean, na.rm=TRUE)

wcr.LE.16<-aggregate(wcr.twr$LE_1, by=list(wcr.twr$MONTH), FUN=mean, na.rm=TRUE)
syv.LE.16<-aggregate(syv.twr$LE_1, by=list(syv.twr$MONTH), FUN=mean, na.rm=TRUE)
wcr.ET.16<-aggregate(LE.to.ET(wcr.twr$LE_1, Tair=wcr.twr$TA_1)*1800,by=list(wcr.twr$MONTH), FUN=sum, na.rm=TRUE)
syv.ET.16<-aggregate(LE.to.ET(syv.twr$LE_1, Tair=syv.twr$TA_1)*1800,by=list(syv.twr$MONTH), FUN=sum, na.rm=TRUE)

syv.LE.17<-aggregate(syv.twr.2017$LE_1_1_1, by=list(syv.twr.2017$MONTH), FUN=mean, na.rm=TRUE)
wcr.LE.17<-aggregate(wcr.twr.2017$LE_1_1_1, by=list(wcr.twr.2017$MONTH), FUN=mean, na.rm=TRUE)
wcr.ET.17<-aggregate(LE.to.ET(wcr.twr.2017$LE_1_1_1, Tair=wcr.twr.2017$TA_1_1_1)*1800,by=list(wcr.twr.2017$MONTH), FUN=sum, na.rm=TRUE)
syv.ET.17<-aggregate(LE.to.ET(syv.twr.2017$LE_1_1_1, Tair=syv.twr.2017$TA_1_1_1)*1800,by=list(syv.twr.2017$MONTH), FUN=sum, na.rm=TRUE)

par(mfrow=c(2,1), mar=c(4,4,1,1))

plot(syv.gpp.month,pch=17, ylab='GPP', xlab='month'); points(wcr.gpp.month, pch=16)
points(syv.gpp.month.15,pch=2, col='red'); points(wcr.gpp.month.15, pch=1, col='red')
points(syv.gpp.month.17, pch=24, col='blue'); points(wcr.gpp.month.17, pch=21, col='blue')

legend(1,350, legend=c("SYV", "WCR"), pch=c(2,1), cex=0.5, bty='n')
legend (0.95,275, legend=c('2015','2016','2017'), fill=c('red','black','blue'), cex=0.5, bty='n')

leafadjust<0

plot(syv.LE.16, pch=17, ylim=c(10,200), ylab='LE', xlab='month');points(wcr.LE.16$x/0.7, pch=16) #IMPORTANT: /0.7 adjusts for leaf area from harvest
points(syv.LE.15, pch=2, col='red');points(wcr.LE.15$x/0.8, pch=1, col='red')
points(syv.LE.17, pch=2, col='blue');points(wcr.LE.17$x/0.8, pch=1, col='blue')



#days

syv.gpp.day<-aggregate(syv.twr$GPP_PI_F*1800/1000000, by=list(syv.twr$DOY), FUN='sum')
plot(syv.gpp.day)
#plot(syv.gpp.day$x[syv.gpp.day$Group.1%in%LAI.dat$DOY]~LAI.dat$SYV)

wcr.gpp.day<-aggregate(wcr.twr$GPP_PI_F*1800/1000000, by=list(wcr.twr$DOY), FUN='sum')
plot(wcr.gpp.day)
#plot(wcr.gpp.day$x[wcr.gpp.day$Group.1%in%LAI.dat$DOY]~LAI.dat$WCR)

syv.le.day<-aggregate(syv.twr$LE_1, by=list(syv.twr$DOY), FUN='mean', na.rm=TRUE)
plot(syv.le.day)
#plot(syv.le.day$x[syv.le.day$Group.1%in%LAI.dat$DOY]~LAI.dat$SYV)

wcr.le.day<-aggregate(wcr.twr$LE_1, by=list(wcr.twr$DOY), FUN='mean', na.rm=TRUE)
plot(wcr.le.day)
#plot(wcr.le.day$x[wcr.le.day$Group.1%in%LAI.dat$DOY]~LAI.dat$WCR)


plot(syv.gpp.day$x[syv.gpp.day$Group.1%in%LAI.dat$DOY]~LAI.dat$SYV, pch=2, cex=0.5)
points(wcr.gpp.day$x[wcr.gpp.day$Group.1%in%LAI.dat$DOY]~LAI.dat$WCR, cex=0.5, col='blue')

plot(syv.le.day$x[syv.le.day$Group.1%in%LAI.dat$DOY]~LAI.dat$SYV, pch=2, cex=0.5)
points(wcr.le.day$x[wcr.le.day$Group.1%in%LAI.dat$DOY]~LAI.dat$WCR, cex=0.5, col='blue')

par(mfrow=c(2,1))
hist(syv.gpp.day$x[syv.gpp.day$Group.1%in%LAI.dat$DOY]/LAI.dat$SYV, breaks=seq(from=0, to=0.3, length.out=20), col=rgb(0,0,1,0.5))
hist(wcr.gpp.day$x[wcr.gpp.day$Group.1%in%LAI.dat$DOY]/LAI.dat$WCR,add=TRUE, breaks=seq(from=0, to=0.3, length.out=20), col=rgb(1,0,0,0.5))

abline(v=mean(syv.gpp.day$x[syv.gpp.day$Group.1%in%LAI.dat$DOY]/LAI.dat$SYV), col='blue')
abline(v=mean(wcr.gpp.day$x[wcr.gpp.day$Group.1%in%LAI.dat$DOY]/LAI.dat$WCR, na.rm=TRUE), col='red')

hist(syv.le.day$x[syv.le.day$Group.1%in%LAI.dat$DOY]/LAI.dat$SYV, breaks=seq(from=-20, to=80, length.out=20), col=rgb(0,0,1,0.5))
hist(wcr.le.day$x[wcr.le.day$Group.1%in%LAI.dat$DOY]/LAI.dat$WCR,add=TRUE, breaks=seq(from=-20, to=80, length.out=20), col=rgb(1,0,0,0.5))

abline(v=mean(syv.le.day$x[syv.le.day$Group.1%in%LAI.dat$DOY]/LAI.dat$SYV,na.rm=TRUE), col='blue')
abline(v=mean(wcr.le.day$x[wcr.le.day$Group.1%in%LAI.dat$DOY]/LAI.dat$WCR, na.rm=TRUE), col='red')

#WUE
#2016
par(mfrow=c(1,2))
plot(syv.gpp.month$x/syv.ET$x, ylim=c(0,7)); points(wcr.gpp.month$x/wcr.ET$x, col='blue')
abline(v=c(5, 9))
mean((syv.gpp.month$x/syv.ET$x)[5:9]);mean((wcr.gpp.month$x/wcr.ET$x)[5:9])

#2017
plot(syv.gpp.month.17$x/syv.ET.17$x, ylim=c(0,7)); points(wcr.gpp.month.17$x/wcr.ET.17$x, col='blue')
abline(v=c(5, 9))
mean((syv.gpp.month.17$x/syv.ET.17$x)[5:9]);mean((wcr.gpp.month.17$x/wcr.ET$x)[5:9])

#WCR has lower WUE

####Forest things####
par(mfrow=c(1,1))
hist(wcr.forest$dbh, breaks=seq(from=5,to=90, length.out=20),col=rgb(1,0,0,0.5), ylim=c(0,100))
hist(syv.forest$dbh, add=TRUE, breaks=seq(from=5,to=90, length.out=20),col=rgb(0,0,1,0.5),ylim=c(0,100))

#maps<-which(wcr.tree$SPP=="ACSA");lwd<-rep(1, 14);lwd[maps]<-2
#abline(v=wcr.tree$DBH, col='red', lwd=lwd);abline(v=syv.tree$DBH, col='blue')

barplot(table(syv.forest$cc, exclude=c("S ", "I ")), ylim=c(0, 200), col=rgb(0,0,1,0.5))
barplot(table(wcr.forest$cc, exclude=c("S ", "I ")), add=TRUE, col=rgb(1,0,0,0.5))
#####
#light
par(mfrow=c(1,1))
syv.li<-aggregate(syv.twr$PPFD_IN_PI_F_1, by=list(syv.twr$DOY), FUN='mean')
wcr.li<-aggregate(wcr.twr$PPFD_IN_PI_F_1, by=list(wcr.twr$DOY), FUN='mean')

median(wcr.li$x[gs]-syv.li$x[gs],na.rm=TRUE)
mean(wcr.li$x[gs]-syv.li$x[gs],na.rm=TRUE)

#LUE
plot(syv.gpp.day$x[gs]~syv.li$x[gs], pch=2, ylim=c(0,1.2)); points(wcr.gpp.day$x[gs]~wcr.li$x[gs])

#wcr has less light and lower LUE



