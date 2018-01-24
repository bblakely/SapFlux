###Plots for Ankur. Making new scripts to preserve old code. 
###Should only be used after running SapProcess_simple for both sites, Apogee_SimpleProcess, and DailyTempPatterns.

#--------
###Sapflux
#--------

par(mfrow=c(2,2))
par(xpd=FALSE)
#--------
#Sylvania
#--------
#par(mfrow=c(1,1))
plot(SYV.VoltPivot$dectime,SYV.VoltPivot$s1flux,type='l',ylim=c(0,7e-05),col='white', xlab="Hour", ylab="Flux", main="SYV Sap Flux")
#Hemlock
##Dominant
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s1flux, type='l',col='forest green',lwd=2, lty=2)
##Codominant
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s2flux, type='l',col='forest green',lwd=2, lty=2)
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s20flux, type='l',col='forest green',lwd=2, lty=2)
##Intermediate
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s8flux, type='l',col='forest green',lwd=2, lty=2)
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s10flux, type='l',col='forest green',lwd=2, lty=2)
#lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s11flux, type='l',col='forest green',lwd=2)
#lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s19flux, type='l',col='forest green',lwd=2) #this one is weird; bad data quailty, probably unfixable.
##Suppressed
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s3flux, type='l',col='forest green',lwd=2, lty=2)

#Birch
##Dominant
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s7flux, type='l',col='blue',lwd=2, lty=2) #A little high at night
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s18flux, type='l',col='blue',lwd=2, lty=2) #slightly early

#Hophornbeam
##Codiminant
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s6flux, type='l',col='dark red',lwd=2, lty=2) #this one is weird but I fixed it
##Intermediate
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s5flux, type='l',col='dark red',lwd=2, lty=2) #A little high at night

#Sugar maple
##Dominant
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s9flux, type='l',col=' dark orange',lwd=2, lty=2)
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s13flux, type='l',col='dark orange',lwd=2, lty=2)
##Codominant
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s4flux, type='l',col='dark orange',lwd=2, lty=2)
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s14flux, type='l',col='dark orange',lwd=2, lty=2)
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s15flux, type='l',col='dark orange',lwd=2, lty=2)
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s17flux, type='l',col='dark orange',lwd=2, lty=2)
##Intermediate
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s16flux, type='l',col='dark orange',lwd=2, lty=2)
#Suppressed
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s12flux, type='l',col='dark orange',lwd=2, lty=2)

#Combine all to get mean flux
Flux.avg.syv<-rowMeans(SYV.VoltPivot[,2:21], na.rm=TRUE)
Flux.avg.syv.med<-apply(SYV.VoltPivot[,2:21], 1, median)
lines(SYV.VoltPivot$dectime, Flux.avg.syv, lwd=4)
#plot(SYV.VoltPivot$dectime, Flux.avg.syv, type='l',lwd=3)

#When does site-level flux peak?
SYV.peaktime<-SYV.VoltPivot$dectime[which(Flux.avg.syv==max(Flux.avg.syv, na.rm=TRUE))]
SYV.peaktime
abline(v=SYV.peaktime, lty=2)


legend(0,6e-05,legend=c("Hemlock","Maple","Yellow Birch","Hophornbeam"), 
       col=c("forest green","dark orange","blue","dark red"), lwd=c(2,2,2,2),lty=c(2,2,2,2),
       cex=0.7,y.intersp=0.3, x.intersp=0.3, text.font=2,bty='n')

#--------
#Willow Creek
#--------

#par(mfrow=c(1,1))
plot(WCR.VoltPivot$dectime,WCR.VoltPivot$s1flux,type='l',ylim=c(0,7e-05),col='white', xlab="Hour", ylab="Flux", main="WCR Sap Flux")

#THE FOLLOWING PLOT COMMANDS ARE FOR WILLOW CREEK#
#Basswood#
#plot(WCR.VoltPivot$dectime,WCR.VoltPivot$s1flux,type='l',ylim=c(0,7e-05),col='white', xlab="Hour", ylab="Flux", main='Basswood')
#legend(1,4e-05, legend=c('codominant','suppressed'), lwd=c(2,2), col=c('yellow','gold'))
##Codominant
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s1flux, type='l',col='gold',lwd=2, lty=2)
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s2flux, type='l',col='gold',lwd=2, lty=2)
##Suppressed
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s8flux, type='l',col='gold',lwd=2, lty=2)
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s11flux, type='l',col='gold',lwd=2, lty=2)

#Hophornbeam
#plot(WCR.VoltPivot$dectime,WCR.VoltPivot$s1flux,type='l',ylim=c(0,7e-05),col='white', xlab="Hour", ylab="Flux", main='Hophornbeam')
#legend(1,4e-05, legend=c('intermediate','suppressed'), lwd=c(2,2), col=c('red','dark red'))
##Intermediate
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s7flux, type='l',col='dark red',lwd=2, lty=2)
##Suppressed
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s3flux, type='l',col='dark red',lwd=2, lty=2)
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s14flux, type='l',col='dark red',lwd=2, lty=2)

#Sugar Maple
#plot(WCR.VoltPivot$dectime,WCR.VoltPivot$s1flux,type='l',ylim=c(0,7e-05),col='white', xlab="Hour", ylab="Flux", main='Sugar Maple')
#legend(1,4e-05, legend=c('codominant','intermediate', 'suppressed'), lwd=c(2,2,2), col=c('orange','dark orange', 'brown'))

##Codominant
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s5flux, type='l',col='dark orange',lwd=2, lty=2)
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s6flux, type='l',col='dark orange',lwd=2, lty=2)
##Intermediate
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s12flux, type='l',col='dark orange',lwd=2, lty=2)
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s13flux, type='l',col='dark orange',lwd=2, lty=2)
##Suppressed
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s4flux, type='l',col='dark orange',lwd=2, lty=2)

#Green Ash
#plot(WCR.VoltPivot$dectime,WCR.VoltPivot$s1flux,type='l',ylim=c(0,7e-05),col='white', xlab="Hour", ylab="Flux", main='Green Ash')
#legend(1,4e-05, legend='codominant', lwd=2, lty=2, col='forest green')

lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s9flux, type='l',col='purple 4',lwd=2, lty=2)
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s10flux, type='l',col='purple 4',lwd=2, lty=2)


legend(0,6e-05,legend=c("Basswood","Maple","Green Ash","Hophornbeam"), 
       col=c("gold","dark orange","purple 4","dark red"), lwd=c(2,2,2,2),lty=c(2,2,2,2),
       cex=0.7, y.intersp=0.3,x.intersp=0.3, text.font=2, bty='n')

#Combine all to get mean flux
Flux.avg.wcr<-rowMeans(WCR.VoltPivot[,2:15], na.rm=TRUE)
Flux.avg.wcr.med<-apply(WCR.VoltPivot[,2:15],1,median)
lines(WCR.VoltPivot$dectime, Flux.avg.wcr, lwd=4)
#plot(WCR.VoltPivot$dectime, Flux.avg.WCR, type='l',lwd=3)

#When does site-level flux peak?
WCR.peaktime<-WCR.VoltPivot$dectime[which(Flux.avg.wcr==max(Flux.avg.wcr, na.rm=TRUE))]
WCR.peaktime
abline(v=WCR.peaktime, lty=2)


#--------
###Apogee
#--------

#------
##Sylvania
#------
plot(SYV.Pivot$dectime, SYV.Pivot$t1, type='l', main="SYV Diel Temperature", 
     xlab="Hour", ylab="Temperature (°C)", lwd=2,lty=2, ylim=c(13,23), col='forest green')
#axis(1, labels=c(1:24), at=seq(from=1, to=nrow(SYV.Pivot), length.out= 24))
lines(SYV.Pivot$dectime,SYV.Pivot$t2, col='red', lwd=2,lty=2)
lines(SYV.Pivot$dectime,SYV.Pivot$t3, col='blue', lwd=2,lty=2)
lines(SYV.Pivot$dectime,SYV.Pivot$t4, col='orange', lwd=2,lty=2)

legend(x=0, y=22,legend=c("North", "East","South","West"), lty=c(2,2,2,2), lwd=c(2,2,2,2), 
       col=c('forest green','red','blue','orange'), cex=0.7,y.intersp=0.3,x.intersp=0.3,text.font=2,
       bty='n')

#Average of all, peak of mean
SYV.temp.avg<-rowMeans(SYV.Pivot[2:5])
lines(SYV.Pivot$dectime,SYV.temp.avg, lwd=4)
#plot(SYV.Pivot$dectime,SYV.temp.avg, type='l')
SYV.peaktemp<-SYV.Pivot$dectime[which(SYV.temp.avg==max(SYV.temp.avg))]
SYV.peaktemp
abline(v=SYV.peaktemp, lty=2)

#Mean of peaks
Peak.temp.SYV<-rep(NaN,4)
for (i in 1:4){
  peak.5min<-which(SYV.Pivot[i+1]==max(SYV.Pivot[i+1]))
  Peak.temp.SYV[i]<-SYV.Pivot$dectime[peak.5min]
}
mean(Peak.temp.SYV)+1

WCR.peaktemp+1
mean(Peak.temp.WCR)+1
WCR.peaktime

SYV.peaktemp
mean(Peak.temp.SYV)
SYV.peaktime


#--------
#Willow Creek
#--------
plot(WCR.Pivot$dectime, WCR.Pivot$t2, type='l', main="WCR Diel Temperature", 
    xlab="Hour", ylab="Temperature (°C)", lwd=2, ylim=c(13,23), lty=2, col='forest green')
#axis(1, labels=c(1:24), at=seq(from=1, to=nrow(WCR.Pivot), length.out= 24), lty=2)
lines(WCR.Pivot$dectime,WCR.Pivot$t1, col='red', lwd=2, lty=2)
lines(WCR.Pivot$dectime,WCR.Pivot$t3, col='blue', lwd=2, lty=2)
lines(WCR.Pivot$dectime,WCR.Pivot$t4, col='orange', lwd=2, lty=2)

legend(x=0, y=22,legend=c("North", "East","South","West"), lty=c(2,2,2,2), lwd=c(2,2,2,2), 
       col=c('forest green','red','blue','orange'), cex=0.7,y.intersp=0.3,
       x.intersp=0.3,text.font=2, bty='n')

#Average of all, peak of mean
WCR.temp.avg<-rowMeans(WCR.Pivot[2:5])
lines(WCR.Pivot$dectime,WCR.temp.avg, type='l', lwd=4)
#plot(WCR.Pivot$dectime,WCR.temp.avg, type='l')
WCR.peaktemp<-WCR.Pivot$dectime[which(WCR.temp.avg==max(WCR.temp.avg))]
WCR.peaktemp
abline(v=WCR.peaktemp, lty=2)


#Mean of peaks
Peak.temp.WCR<-rep(NaN,4)
for (i in 1:4){
  peak.5min<-which(WCR.Pivot[i+1]==max(WCR.Pivot[i+1]))
  Peak.temp.WCR[i]<-WCR.Pivot$dectime[peak.5min]
}

mean(Peak.temp.WCR)

#abline(v=mean(Peak.temp.WCR))


WCR.peaktemp
WCR.peaktime

SYV.peaktemp
SYV.peaktime

#numbers


