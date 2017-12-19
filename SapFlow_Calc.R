source('Prepare_treedata.R')
library(vioplot)
syv.raw<-read.csv('SYV_manual.csv')
wcr.raw<-read.csv('WCR_manual.csv')

nDOY<-length(unique(syv.raw$DOY))

#Add H and M becasue baseliner doesn't preserve TOA5 time

syv.raw$H<-rep(c(0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,
               13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20,21,21,22,
               22,23,23),nDOY)

syv.raw$M<-rep(c(0,30),(24*nDOY))

wcr.raw$H<-rep(c(0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,
                 13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20,21,21,22,
                 22,23,23),nDOY)

wcr.raw$M<-rep(c(0,30),(24*nDOY))


time=14

#Choose a site. Eventually I'll streamline this. But for now do sequentially
DAT<-syv.raw
DAT$Dectime=(DAT$H)+((DAT$M)/60)
DAT$DecDay<-(DAT$DOY)+(DAT$Dectime/24)
fc<-6
lc<-25

DAT.s<-DAT[fc:lc]
Flux<-as.data.frame(0.000119*(DAT.s^1.231))
flux.mean<-colMeans(Flux, na.rm=TRUE)
flux.syv<-Flux

treemult<-(syv.tree$Multiplier*(syv.tree$SWA/10000))
treemult[3]<-(syv.tree$SWA[3])/10000

Flow<-(sweep(Flux,2,treemult,'*'))
syv.flow<-cbind(DAT[,c(2,4:5,28:29)],Flow)
Flow.pd<-Flow*1800  #1800 seconds in 30 min


flow.time<-colMeans(Flow.pd[syv.flow$Dectime==time,], na.rm=TRUE)

Daily.m3.s<-(aggregate(Flow.pd,by=list(DAT$DOY),FUN=sum))
Daily<-Daily.m3.s[2:21]*1000  #1000 L per m3

flow.mean<-colMeans(Daily, na.rm=TRUE)

nSensors<-ncol(Daily)

colvec<-rep('black', nSensors)
colvec[syv.tree$SPP=="ACSA"]<-'orange'
colvec[syv.tree$SPP=="ACRU"]<-'red'
colvec[syv.tree$SPP=="POTR"]<-'light blue'
colvec[syv.tree$SPP=="TIAM"]<-'yellow'
colvec[syv.tree$SPP=="BEPA"]<-'gray'
colvec[syv.tree$SPP=="BEAL"]<-'blue'
colvec[syv.tree$SPP=="TSCA"]<-'forest green'
colvec[syv.tree$SPP=="OSVI"]<-'dark red'
colvec[syv.tree$SPP=="FRPE"]<-'white'



syv.plot.info<-cbind(syv.tree,flux.mean,flow.mean, flow.time, colvec)
syv.plot.info$ba.norm<-(syv.plot.info$flow.mean)/syv.plot.info$BA
syv.plot.sort<-syv.plot.info[order(syv.plot.info$SPP, syv.plot.info$DBH),]
barplot(syv.plot.sort$flow.mean,col=as.character(syv.plot.sort$colvec))


#WCR
DAT<-wcr.raw
DAT$Dectime=(DAT$H)+((DAT$M)/60)
DAT$DecDay<-(DAT$DOY)+(DAT$Dectime/24)
fc<-6
lc<-19

DAT.s<-DAT[fc:lc]
Flux<-as.data.frame(0.000119*(DAT.s^1.231))
flux.wcr<-Flux
flux.mean<-colMeans(Flux, na.rm=TRUE)

treemult<-(wcr.tree$Multiplier*(wcr.tree$SWA/10000))

Flow<-(sweep(Flux,2,treemult,'*'))
wcr.flow<-cbind(DAT[,c(2,4:5,22:23)],Flow)
Flow.pd<-Flow*1800  #1800 seconds in 30 min


flow.time<-colMeans(Flow.pd[syv.flow$Dectime==time,], na.rm=TRUE)

Daily.m3.s<-(aggregate(Flow.pd,by=list(DAT$DOY),FUN=sum))
Daily<-Daily.m3.s[2:15]*1000  #1000 L per m3

flow.mean<-colMeans(Daily, na.rm=TRUE)

nSensors<-ncol(Daily)

colvec<-rep('black', nSensors)
colvec[wcr.tree$SPP=="ACSA"]<-'orange'
colvec[wcr.tree$SPP=="ACRU"]<-'red'
colvec[wcr.tree$SPP=="POTR"]<-'light blue'
colvec[wcr.tree$SPP=="TIAM"]<-'yellow'
colvec[wcr.tree$SPP=="BEPA"]<-'gray'
colvec[wcr.tree$SPP=="BEAL"]<-'blue'
colvec[wcr.tree$SPP=="TSCA"]<-'forest green'
colvec[wcr.tree$SPP=="OSVI"]<-'dark red'
colvec[wcr.tree$SPP=="FRPE"]<-'darkolivegreen3'

wcr.plot.info<-cbind(wcr.tree,flux.mean, flow.mean,flow.time, colvec)
wcr.plot.info$ba.norm<-(wcr.plot.info$flow.mean)/wcr.plot.info$BA
wcr.plot.sort<-wcr.plot.info[order(wcr.plot.info$SPP, wcr.plot.info$DBH),]
barplot(wcr.plot.sort$flow.mean,col=as.character(wcr.plot.sort$colvec))


barplot(wcr.plot.sort$ba.norm,col=as.character(wcr.plot.sort$colvec), main='BA', ylim=c(0,0.06))
barplot(syv.plot.sort$ba.norm,col=as.character(syv.plot.sort$colvec), main='BA', ylim=c(0,0.06))

barplot(wcr.plot.sort$flux.mean,col=as.character(wcr.plot.sort$colvec), main=' WCR Flux Rate')
barplot(syv.plot.sort$flux.mean,col=as.character(syv.plot.sort$colvec), main='SYV Flux Rate')

write.csv(syv.plot.sort,'syv.summary.csv')
write.csv(wcr.plot.sort,'wcr.summary.csv')

#summary(lm(syv.plot.sort$DBH[c(12,13,15:20)]~syv.plot.sort$flux.mean[c(12,13,15:20)]))
#t.test(wcr.plot.sort$flux.mean[wcr.plot.sort$SPP=='OSVI'], syv.plot.sort$flux.mean[syv.plot.sort$SPP=='OSVI'])

syv.flux.sp<-aggregate(syv.plot.sort$flux.mean,list(syv.plot.sort$SPP),FUN=mean)
wcr.flux.sp<-aggregate(wcr.plot.sort$flux.mean,list(wcr.plot.sort$SPP),FUN=mean)

#Play with data

# for(i in 1:14){
# plot(wcr.flow[,i+5],wcr.flow$VPD, main=i)
# #print(summary(lm(wcr.flow[,i+5]~wcr.flow$VPD))$r.squared)
# #print(summary(lm(wcr.flow[,i+5]~wcr.flow$VPD)))
# }
# 
# for(i in 1:20){
#   plot(syv.flow[,i+5],syv.flow$VPD, main=i)
#   #print(summary(lm(wcr.flow[,i+5]~wcr.flow$VPD))$r.squared)
#   #print(summary(lm(wcr.flow[,i+5]~wcr.flow$VPD)))
# }


#Violin plots of flux
no.nan<-lapply(wcr.flow[wcr.flow$Dectime==13.0,6:19],na.omit)
no.nan.flux<-lapply(flux.wcr[wcr.flow$Dectime==14.0,],na.omit)
# vioplot(unlist(no.nan[1]),unlist(no.nan[2]),unlist(no.nan[3]),unlist(no.nan[4]),unlist(no.nan[5]),unlist(no.nan[6]),unlist(no.nan[7]),unlist(no.nan[8]),unlist(no.nan[9]),
#         unlist(no.nan[10]),unlist(no.nan[11]),unlist(no.nan[12]),unlist(no.nan[13]),unlist(no.nan[14]), col='white')

plot(0:20, ylim=c(0,1.5e-04), axes=FALSE, ylab="Flux", xlab="Species")
for (i in 1:14) { 
  vioplot(unlist(no.nan.flux[i]), at = i, add = T, col = as.character(wcr.plot.info$colvec[i]))
}
axis(side=1,at=1:14,labels=wcr.plot.info$SPP)
axis(side=2,at=seq(from=0,to=2e-04,length.out=5),labels=seq(from=0,to=2e-04,length.out=5))



#Now SYV!
no.nan.flux<-lapply(flux.syv[syv.flow$Dectime==14.0,],na.omit)
# vioplot(unlist(no.nan[1]),unlist(no.nan[2]),unlist(no.nan[3]),unlist(no.nan[4]),unlist(no.nan[5]),unlist(no.nan[6]),unlist(no.nan[7]),unlist(no.nan[8]),unlist(no.nan[9]),
#         unlist(no.nan[10]),unlist(no.nan[11]),unlist(no.nan[12]),unlist(no.nan[13]),unlist(no.nan[14]), col='white')

plot(0:20, ylim=c(0,1.5e-04), axes=FALSE, ylab="Flux", xlab="Species")
for (i in 1:20) { 
  vioplot(unlist(no.nan.flux[i]), at = i, add = T, col = as.character(syv.plot.info$colvec[i]))
}
axis(side=1,at=1:20,labels=syv.plot.info$SPP)
axis(side=2,at=seq(from=0,to=2e-04,length.out=5),labels=seq(from=0,to=2e-04,length.out=5))


####Diel profliles, woo
diels.syv<-aggregate(syv.flow, by=list(syv.flow$Dectime), FUN=mean, na.rm=TRUE)
diels.wcr<-aggregate(wcr.flow, by=list(wcr.flow$Dectime), FUN=mean, na.rm=TRUE)


# plot(diels.syv$S1, type='l', ylim=c(0,2.5e-06), col='white')
# for(i in c(1:18,20)){
#   lines(diels.syv[,i+6], col=as.character(syv.plot.info$colvec[i]), lwd=3)
# }

#separate by spp
par(mfrow=c(2,2), mar=c(4,4.3,2,0.5))


plot(diels.syv$S1~diels.syv$Dectime, type='l', ylim=c(0,2.5e-06), col='white', xlab='', ylab="Flow (m3 s-1)", main="ACSA",
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
for(i in which(syv.plot.info$SPP=='ACSA')){
  lines(diels.syv[,i+6]~diels.syv$Dectime, col=as.character(syv.plot.info$colvec[i]), lwd=4)
}

plot(diels.syv$S1~diels.syv$Dectime, type='l', ylim=c(0,2.5e-06), col='white', xlab='', ylab="", main="BEAL",
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
for(i in which(syv.plot.info$SPP=='BEAL')){
  lines(diels.syv[,i+6]~diels.syv$Dectime, col=as.character(syv.plot.info$colvec[i]), lwd=4)
}

plot(diels.syv$S1~diels.syv$Dectime, type='l', ylim=c(0,2.5e-06), col='white', xlab='Hour', ylab="Flow (m3 s-1)", main="OSVI",
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
for(i in which(syv.plot.info$SPP=='OSVI')){
  lines(diels.syv[,i+6]~diels.syv$Dectime, col=as.character(syv.plot.info$colvec[i]), lwd=4)
}

plot(diels.syv$S1~diels.syv$Dectime, type='l', ylim=c(0,2.5e-06), col='white', xlab='Hour', ylab="", main='TSCA',
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
for(i in which(syv.plot.info$SPP=='TSCA')){
  lines(diels.syv[,i+6]~diels.syv$Dectime, col=as.character(syv.plot.info$colvec[i]), lwd=4)
}

#Now WCR

par(mfrow=c(2,2), mar=c(4,4.3,2,0.5))


plot(diels.wcr$S1~diels.wcr$Dectime, type='l', ylim=c(0,2.5e-06), col='white', xlab='', ylab="Flow (m3 s-1)", main="ACSA",
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
for(i in which(wcr.plot.info$SPP=='ACSA')){
  lines(diels.wcr[,i+6]~diels.wcr$Dectime, col=as.character(wcr.plot.info$colvec[i]), lwd=4)
}

plot(diels.wcr$S1~diels.wcr$Dectime, type='l', ylim=c(0,2.5e-06), col='white', xlab='', ylab="", main="FRPE",
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
for(i in which(wcr.plot.info$SPP=='FRPE')){
  lines(diels.wcr[,i+6]~diels.wcr$Dectime, col=as.character(wcr.plot.info$colvec[i]), lwd=4)
}

plot(diels.wcr$S1~diels.wcr$Dectime, type='l', ylim=c(0,2.5e-06), col='white', xlab='Hour', ylab="Flow (m3 s-1)", main="OSVI",
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
for(i in which(wcr.plot.info$SPP=='OSVI')){
  lines(diels.wcr[,i+6]~diels.wcr$Dectime, col=as.character(wcr.plot.info$colvec[i]), lwd=4)
}

plot(diels.wcr$S1~diels.wcr$Dectime, type='l', ylim=c(0,2.5e-06), col='white', xlab='Hour', ylab="", main="TIAM",
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
for(i in which(wcr.plot.info$SPP=='TIAM')){
  lines(diels.wcr[,i+6]~diels.wcr$Dectime, col=as.character(wcr.plot.info$colvec[i]), lwd=4)
}



#####Nooooow for fluuuuxxx
diels.syv<-aggregate(flux.syv, by=list(syv.flow$Dectime), FUN=mean, na.rm=TRUE)
diels.wcr<-aggregate(flux.wcr, by=list(wcr.flow$Dectime), FUN=mean, na.rm=TRUE)



par(mfrow=c(2,2), mar=c(4,4.3,2,0.5))


plot(diels.syv$S1~diels.syv$Group.1, type='l', col='white', xlab='',ylim=c(0,7e-05), ylab="Flux (ms-1)", main="ACSA",
cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
for(i in which(syv.plot.info$SPP=='ACSA')){
  lines(diels.syv[,i+1]~diels.syv$Group.1, col=as.character(syv.plot.info$colvec[i]), lwd=4)
}

plot(diels.syv$S1~diels.syv$Group.1, type='l', ylim=c(0,7e-05), col='white', xlab='', ylab="", main="BEAL",
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
for(i in which(syv.plot.info$SPP=='BEAL')){
  lines(diels.syv[,i+1]~diels.syv$Group.1, col=as.character(syv.plot.info$colvec[i]), lwd=4)
}

plot(diels.syv$S1~diels.syv$Group.1, type='l', ylim=c(0,7e-05), col='white', xlab='Hour', ylab="Flux (ms-1)", main="OSVI",
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
for(i in which(syv.plot.info$SPP=='OSVI')){
  lines(diels.syv[,i+1]~diels.syv$Group.1, col=as.character(syv.plot.info$colvec[i]), lwd=4)
}

plot(diels.syv$S1~diels.syv$Group.1, type='l', ylim=c(0,7e-05), col='white', xlab='Hour', ylab="", main='TSCA',
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
for(i in which(syv.plot.info$SPP=='TSCA')){
  lines(diels.syv[,i+1]~diels.syv$Group.1, col=as.character(syv.plot.info$colvec[i]), lwd=4)
}

#Now WCR

par(mfrow=c(2,2), mar=c(4,4.3,2,0.5))


plot(diels.wcr$S1~diels.wcr$Group.1, type='l', ylim=c(0,7e-05), col='white', xlab='', ylab="Flux (ms-1)", main="ACSA",
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
for(i in which(wcr.plot.info$SPP=='ACSA')){
  lines(diels.wcr[,i+1]~diels.wcr$Group.1, col=as.character(wcr.plot.info$colvec[i]), lwd=4)
}

plot(diels.wcr$S1~diels.wcr$Group.1, type='l', ylim=c(0,7e-05), col='white', xlab='', ylab="", main="FRPE",
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
for(i in which(wcr.plot.info$SPP=='FRPE')){
  lines(diels.wcr[,i+1]~diels.wcr$Group.1, col=as.character(wcr.plot.info$colvec[i]), lwd=4)
}

plot(diels.wcr$S1~diels.wcr$Group.1, type='l', ylim=c(0,7e-05), col='white', xlab='Hour', ylab="Flux (ms-1)", main="OSVI",
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
for(i in which(wcr.plot.info$SPP=='OSVI')){
  lines(diels.wcr[,i+1]~diels.wcr$Group.1, col=as.character(wcr.plot.info$colvec[i]), lwd=4)
}

plot(diels.wcr$S1~diels.wcr$Group.1, type='l', ylim=c(0,7e-05), col='white', xlab='Hour', ylab="", main="TIAM",
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
for(i in which(wcr.plot.info$SPP=='TIAM')){
  lines(diels.wcr[,i+1]~diels.wcr$Group.1, col=as.character(wcr.plot.info$colvec[i]), lwd=4)
}


all.plot.info<-rbind(syv.plot.info,wcr.plot.info)
all.plot.info$CC[c(1,5,11,14,17)]<-c("D","S","I","I","C")

simple<-lm(flow.time~Site+SPP+DBH+CC, all.plot.info)
summary(simple)
af <- anova(simple)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))

# anova<-aov(flow.time~Site+SPP+CC, all.plot.info)
# summary(anova)
# tuk<-TukeyHSD(anova)
