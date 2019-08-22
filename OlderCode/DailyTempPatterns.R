
WCR.piv=WCRcore

names(WCR.piv)<-tolower(names(WCR.piv))
library(reshape2)
WCR.PrePiv=melt(WCR.piv,id='dectime',measure.vars=c('t1','t2', 't3','t4'),na.rm=TRUE)

WCR.Pivot=dcast(WCR.PrePiv,dectime ~ variable, mean)

# #Correct for datalogger lag
# WCR.volt.correct<-as.data.frame(matrix(nrow=nrow(WCR.Pivot.lag),ncol=ncol(WCR.Pivot.lag)))
# colnames(WCR.volt.correct)<-colnames(WCR.Pivot.lag)
# WCR.volt.correct[,1]<-WCR.Pivot.lag$dectime
# WCR.volt.correct[13:288,2:5]<-WCR.Pivot.lag[1:276,2:5]
# WCR.volt.correct[1:12,2:5]<-WCR.Pivot.lag[277:288,2:5]
# 
# WCR.Pivot<-WCR.volt.correct

plot(WCR.Pivot$t2, type='l', main="WCR Average Diel Temperature", 
     xlab="Hour", ylab="Temperature (°C)", lwd=3, xaxt='n', ylim=c(13,23))
axis(1, labels=c(1:24), at=seq(from=1, to=nrow(WCR.Pivot), length.out= 24))
lines(WCR.Pivot$t1, col='red', lwd=3)
lines(WCR.Pivot$t3, col='blue', lwd=3)
lines(WCR.Pivot$t4, col='orange', lwd=3)
legend(x=0, y=22,legend=c("N", "E","S","W"), lty=c(1,1,1,1), lwd=c(3,3,3,3), col=c('black','red','blue','orange'))

#Average of all, peak of mean
WCR.temp.avg<-rowMeans(WCR.Pivot[2:5])
plot(WCR.Pivot$dectime,WCR.temp.avg, type='l')
WCR.peaktemp<-WCR.Pivot$dectime[which(WCR.temp.avg==max(WCR.temp.avg))]
WCR.peaktemp
abline(v=WCR.peaktemp)

#Mean of peaks
Peak.temp.WCR<-rep(NaN,4)
for (i in 1:4){
peak.5min<-which(WCR.Pivot[i+1]==max(WCR.Pivot[i+1]))
Peak.temp.WCR[i]<-WCR.Pivot$dectime[peak.5min]
}

mean(Peak.temp.WCR)


SYV.piv=SYVcore
#[13500:nrow(SYVcore),4:8]
#DataPiv=DATcore
names(SYV.piv)<-tolower(names(SYV.piv))
library(reshape2)
SYV.PrePiv=melt(SYV.piv,id='dectime',measure.vars=c('t1','t2', 't3','t4'),na.rm=TRUE)

SYV.Pivot=dcast(SYV.PrePiv,dectime ~ variable, mean)

plot(SYV.Pivot$t1, type='l', main="SYV Average Diel Temperature", 
     xlab="Hour", ylab="Temperature (°C)", lwd=3, xaxt='n', ylim=c(13,23))
axis(1, labels=c(1:24), at=seq(from=1, to=nrow(SYV.Pivot), length.out= 24))
lines(SYV.Pivot$t2, col='red', lwd=3)
lines(SYV.Pivot$t3, col='blue', lwd=3)
lines(SYV.Pivot$t4, col='orange', lwd=3)
legend(x=0, y=22,legend=c("N", "E","S","W"), lty=c(1,1,1,1), lwd=c(3,3,3,3), col=c('black','red','blue','orange'))

#Average of all, peak of mean
SYV.temp.avg<-rowMeans(SYV.Pivot[2:5])
plot(SYV.Pivot$dectime,SYV.temp.avg, type='l')
SYV.peaktemp<-SYV.Pivot$dectime[which(SYV.temp.avg==max(SYV.temp.avg))]
SYV.peaktemp
abline(v=SYV.peaktemp)

#Mean of peaks
Peak.temp.SYV<-rep(NaN,4)
for (i in 1:4){
  peak.5min<-which(SYV.Pivot[i+1]==max(SYV.Pivot[i+1]))
  Peak.temp.SYV[i]<-SYV.Pivot$dectime[peak.5min]
}
mean(Peak.temp.SYV)


#Min and max temps

SYV.min<-apply(SYV.Pivot,FUN=min, MARGIN=2)
WCR.min<-apply(WCR.Pivot,FUN=min, MARGIN=2)

SYV.avg.min<-mean(SYV.min[2:5])
WCR.avg.min<-mean(WCR.min[2:5])


barplot(c(WCR.avg.min, SYV.avg.min), col=c('orange', 'forest green'), 
        names.arg=c('WCR','SYV'), main='Min Temp')


SYV.max<-apply(SYV.Pivot,FUN=max, MARGIN=2)
WCR.max<-apply(WCR.Pivot,FUN=max, MARGIN=2)

SYV.avg.max<-mean(SYV.max[2:5])
WCR.avg.max<-mean(WCR.max[2:5])

barplot(c(WCR.avg.max, SYV.avg.max), col=c('orange', 'forest green'), 
        names.arg=c('WCR','SYV'), main='Max Temp')



for(i in 1:4){
  plot(WCR.Pivot$dectime, (WCR.Pivot[,i+1]-SYV.Pivot[,i+1]))
  abline(h=0)
  abline(v=12, lty=2)
}


#SYV .12 deg cooler surface than air, WCR .67 deg cooler surface than air