start=189
end=260

longplot=FALSE

tower.raw.syv<-read.csv('Syv_TowerData_2015.csv',skip=1, header=TRUE)
tower.names.syv<-colnames(read.csv('Syv_TowerData_2015.csv'))
names(tower.raw.syv)<-tower.names.syv

tower.match.syv<-tower.raw.syv[tower.raw.syv$DTIME>=start & tower.raw.syv$DTIME<end,]
#Ustar filter
#tower.match.syv[tower.match.syv$UST<0.2,6:45]<-(-9999)
#tower.match.syv[tower.match.syv$WD>330 | tower.match.syv$WD<90, 6:45]<-(-9999)
tower.match.syv[tower.match.syv==-9999]<-NA

#WCR Tower
tower.raw.wcr<-read.csv('WCR_TowerData_2015.csv',skip=1, header=TRUE)
tower.names.wcr<-colnames(read.csv('WCR_TowerData_2015.csv'))
names(tower.raw.wcr)<-tower.names.wcr

tower.match.wcr<-tower.raw.wcr[tower.raw.wcr$DTIME>=start & tower.raw.wcr$DTIME<end,]
#tower.match.wcr<-tower.match.wcr[1:(nrow(tower.match.wcr)-1),]
tower.match.wcr[tower.match.wcr==-9999]<-NA

source('Sapflow_plotscale.R')



#rm(list=setdiff(ls(), c("tower.match.syv", "syv.flow","flux.syv", 'syv.tree',
                       # "tower.match.wcr", "wcr.flow", 'flux.wcr','wcr.tree',"start","end")))

timecol<-rep('black', 3408)
timecol[tower.match.syv$HRMIN>600 & tower.match.syv$HRMIN<=1300]<-'orange'
timecol[tower.match.syv$HRMIN>1300 & tower.match.syv$HRMIN<=2200]<-'blue'

#These take a good bit of time
#par(mfrow=c(2,2))
if (longplot==TRUE){
for (i in 1:20){
  plot(tower.match.syv$VPD[timecol!='black'], flux.syv[timecol!='black',i],  main=paste('syv',syv.tree$CC[i], syv.tree$SPP[i], i), col=timecol[timecol!='black'], pch=".", xlab='VPD', ylab='Flux', cex=6)
  print(paste(round(i*100/34), '%', sep=''))
}
for (i in 1:14){
  plot((tower.match.wcr$VPD[timecol!='black']), (flux.wcr[timecol!='black',i]),  main=paste('wcr',wcr.tree$CC[i], wcr.tree$SPP[i], i), col=timecol[timecol!='black'], pch=".", xlab='VPD', ylab='Flux', cex=6)
  print(paste(round((i+20)*100/34), '%', sep=''))
}
}

#For PAR; will normally comment out
par(mfrow=c(2,2))
if (longplot==TRUE){
  for (i in 1:20){
    plot(tower.match.syv$PAR[timecol!='black'], flux.syv[timecol!='black',i],  main=paste('syv',syv.tree$CC[i], syv.tree$SPP[i], i), col=timecol[timecol!='black'], pch=".", cex=6)
    print(paste(round(i*100/34), '%', sep=''))
  }
  for (i in 1:14){
    plot((tower.match.wcr$PAR[timecol!='black']), (flux.wcr[timecol!='black',i]),  main=paste('wcr',wcr.tree$CC[i], wcr.tree$SPP[i], i), col=timecol[timecol!='black'], pch=".", cex=6)
    print(paste(round((i+20)*100/34), '%', sep=''))
  }
}


if (prepMEEC=TRUE){
  par(mfrow=c(2,2), mar=c(4.1,4.5,2,2))
  timecol<-rep('black', 3408)
  timecol[tower.match.syv$HRMIN>600 & tower.match.syv$HRMIN<=1300]<-'light blue'
  timecol[tower.match.syv$HRMIN>1300 & tower.match.syv$HRMIN<=2200]<-'blue'
  for (i in 12){
    plot(tower.match.syv$VPD[timecol!='black'], flux.syv[timecol!='black',i],  
         main="Vapor Pressure Deficit", col=timecol[timecol!='black'], pch="." 
         ,cex=6, ylab="VPD", xaxt='n', xlab='', cex.axis=1.5,cex.main=1.5,cex.lab=1.5, font.axis=2,font.main=2,font.lab=2)
    print(paste(round(i*100/34), '%', sep=''))
  }
  for (i in 12){
    plot(tower.match.syv$PAR[timecol!='black'], flux.syv[timecol!='black',i],  main="Light", col=timecol[timecol!='black'], 
         pch=".", cex=6, ylab="PAR", xlab='', xaxt='n', cex.axis=1.5,cex.main=1.5,cex.lab=1.5, 
         font.axis=2,font.main=2,font.lab=2)
    print(paste(round(i*100/34), '%', sep=''))
  }
  
  for (i in 12){
    plot(tower.match.syv$TA[timecol!='black'], flux.syv[timecol!='black',i],  main="Air Temperature", 
         col=timecol[timecol!='black'], pch=".", cex=6, ylab="TA",xlab="Flux", cex.axis=1.5,cex.main=1.5,
         cex.lab=1.5, font.axis=2,font.main=2,font.lab=2, xaxt='n')
    print(paste(round(i*100/34), '%', sep=''))
  }
  
  for (i in 12){
    plot(syv.tempavg[timecol!='black'], flux.syv[timecol!='black',i],  main="Surface Temperature", 
         col=timecol[timecol!='black'], pch=".", cex=6, ylab="TS",xlab="Flux", 
         cex.axis=1.5,cex.main=1.5,cex.lab=1.5, font.axis=2,font.main=2,font.lab=2, xaxt='n')
    print(paste(round(i*100/34), '%', sep=''))
  }
}

#Hysterisis
par(mfrow=c(2,2))

ylim.syv<-c(2e-05,4.2e-05,1.5e-05,3.5e-05)
ylim.wcr<-c(5e-05,3e-05,3.8e-05,6e-06)

for (j in  dry[1:9]){ #dry[1:9]
day<-j
ylim.count<-1
for (i in c(2,6,7,9)){
  plot(tower.match.wcr$VPD[tower.match.wcr$DOY==day], flux.wcr[tower.match.wcr$DOY==day,i],  main=paste('wcr',wcr.tree$CC[i], wcr.tree$SPP[i], i, 'DOY',j), 
       col=timecol[tower.match.wcr$DOY==day], pch="*", xlim=c(0,3), ylim=c(0,ylim.wcr[ylim.count]), cex=2)
  #print(paste(round(i*100/34), '%', sep=''))
  ylim.count<-ylim.count+1
}
ylim.count<-1
for (i in c(2,5,9,12)){ # c(2,5,9,12)
  plot(tower.match.syv$VPD[tower.match.syv$DOY==day], flux.syv[tower.match.syv$DOY==day,i],  main=paste('syv',syv.tree$CC[i], syv.tree$SPP[i], i, 'DOY',j), 
       col=timecol[tower.match.syv$DOY==day], pch="*", xlim=c(0,3),ylim=c(0,ylim.syv[ylim.count]), cex=2, xlab='VPD', ylab='Flux')
  #print(paste(round(i*100/34), '%', sep=''))
  ylim.count<-ylim.count+1
}
}


#Now with light as covariate
par(mfrow=c(2,2))
for (j in dry[1:9]){
  day<-j
  for (i in c(2,6,7,9)){
    plot(tower.match.wcr$PAR[tower.match.wcr$DOY==day], flux.wcr[tower.match.wcr$DOY==day,i],  main=paste('wcr',wcr.tree$CC[i], wcr.tree$SPP[i], i, 'DOY',j), 
         col=timecol[tower.match.wcr$DOY==day], pch="*", cex=2)
    #print(paste(round(i*100/34), '%', sep=''))
    ylim.count<-ylim.count+1
  }
  for (i in c(2,5,9,12)){
    plot(tower.match.syv$PAR[tower.match.syv$DOY==day], flux.syv[tower.match.syv$DOY==day,i],  main=paste('syv',syv.tree$CC[i], syv.tree$SPP[i], i, 'DOY',j), 
         col=timecol[tower.match.syv$DOY==day], pch="*", cex=2)
    #print(paste(round(i*100/34), '%', sep=''))
  }
}




#Fuck around with some plots and stuff

#nigttime LE, paired
t.test(tower.match.syv$LE[tower.match.syv$HRMIN<600 | tower.match.syv$HRMIN>2100], tower.match.wcr$LE[tower.match.syv$HRMIN<600 | tower.match.syv$HRMIN>2100],paired=TRUE)
#nighttime VPD, paired
t.test(tower.match.syv$VPD[tower.match.syv$HRMIN<600 | tower.match.syv$HRMIN>2100], tower.match.wcr$VPD[tower.match.syv$HRMIN<600 | tower.match.syv$HRMIN>2100],paired=TRUE)

#daytime LE, paired
t.test(tower.match.syv$LE[tower.match.syv$HRMIN>600 & tower.match.syv$HRMIN<2100], tower.match.wcr$LE[tower.match.syv$HRMIN>600 & tower.match.syv$HRMIN<2100],paired=TRUE)
#daytime VPD, paired
t.test(tower.match.syv$VPD[tower.match.syv$HRMIN>600 & tower.match.syv$HRMIN<2100], tower.match.wcr$VPD[tower.match.syv$HRMIN>600 & tower.match.syv$HRMIN<2100],paired=TRUE)

# #LE diel profile
# plot(aggregate(tower.match.wcr$LE, by=list(tower.match.wcr$HRMIN), FUN=mean, na.rm=TRUE), ylim=c(-20,250), col='dark gray',main='LE', pch='*', cex=2, xlab='HH:MM')
# points(aggregate(tower.match.syv$LE, by=list(tower.match.syv$HRMIN), FUN=mean, na.rm=TRUE), ylim=c(-20,250), col='black', pch='*', cex=2)
# legend(0,225,legend=c('SYV', 'WCR'), col=c('black', 'dark gray'), pch='*')

par(mfrow=c(2,2), mar=c(4,4.5,2,1.5))
#VPD diel profile
plot(aggregate(tower.match.wcr$VPD, by=list(tower.match.wcr$HRMIN), FUN=mean, na.rm=TRUE), ylim=c(0,1.3), main='Vapor Pressure Deficit', 
     col='dark gray', pch='*', cex=2, xlab='HH:MM', ylab='VPD',
     cex.lab=1.5,cex.axis=1.5,cex.main=1.5,font.main=2,font.axis=2,font.lab=2)
points(aggregate(tower.match.syv$VPD, by=list(tower.match.syv$HRMIN), FUN=mean, na.rm=TRUE), ylim=c(-0,1.3), col='black', pch='*', cex=2)
legend(0,1.18,legend=c('SYV', 'WCR'), col=c('black', 'dark gray'), pch='*')

#PAR diel profile
plot(aggregate(tower.match.wcr$PAR, by=list(tower.match.wcr$HRMIN), FUN=mean, na.rm=TRUE), ylim=c(0,1700), 
     col='dark gray',main='Light', pch='*', cex=2, xlab='HH:MM', ylab='PAR',
     cex.lab=1.5,cex.axis=1.5,cex.main=1.5,font.main=2,font.axis=2,font.lab=2)

points(aggregate(tower.match.syv$PAR, by=list(tower.match.syv$HRMIN), FUN=mean, na.rm=TRUE), ylim=c(0,1700), col='black', pch='*', cex=2)
legend(0,1600,legend=c('SYV', 'WCR'), col=c('black', 'dark gray'), pch='*')

#TA diel profile
plot(aggregate(tower.match.wcr$TA, by=list(tower.match.wcr$HRMIN), FUN=mean, na.rm=TRUE), ylim=c(14,24), main='Air Temperature', 
     col='dark gray', pch='*', cex=2, xlab='HH:MM', ylab="TA",
     cex.lab=1.5,cex.axis=1.5,cex.main=1.5,font.main=2,font.axis=2,font.lab=2)

points(aggregate(tower.match.syv$TA, by=list(tower.match.syv$HRMIN), FUN=mean, na.rm=TRUE), ylim=c(14,24), col='black', pch='*', cex=2)
legend(0,23.4,legend=c('SYV', 'WCR'), col=c('black', 'dark gray'), pch='*')

#TS diel profile (must have run apogee simple process)
plot(wcr.rp.temp$x, ylim=c(14,24), main='Surface Temperature', 
     col='dark gray', pch='*', cex=2, xlab='HH:MM', xaxt='n', ylab="TS",
     cex.lab=1.5,cex.axis=1.5,cex.main=1.5,font.main=2,font.axis=2,font.lab=2)

points(syv.rp.temp$x, ylim=c(14,24), col='black', pch='*', cex=2)
legend(0,23.4,legend=c('SYV', 'WCR'), col=c('black', 'dark gray'), pch='*')
axis(side=1, at=c(0,10,20,30,40), labels=c(0,500,1000,1500,2000), cex.axis=1.5,font=2)


