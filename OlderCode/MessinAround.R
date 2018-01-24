wcr.2016<-read.csv('WCR_2016_SAPFLUX.csv')
wcr.2015<-read.csv('WCR_2015_SAPFLUX.csv')

wcr.mid<-wcr.2015[wcr.2015$HOUR==13&wcr.2015$MIN==00,]

syv.2016<-read.csv('SYV_2016_SAPFLUX.csv')
syv.2015<-read.csv('SYV_2015_SAPFLUX.csv')

#####
syv.mid<-syv.2015[syv.2015$HOUR==13&syv.2015$MIN==00,]


for(i in 1:14){
plot(wcr.mid[,i+4]~wcr.mid$DOY, type='l', main=i)
}



xcd
for(i in 1:14){
  dat<-wcr.mid[i+4]
  sm<-rollapply(dat,7,FUN=mean, na.rm=TRUE)
  if(i==1){
    plot(sm,type='l', ylim=c(0,100), main='WCR')
  }else{
    lines(sm, col=as.character(wcr.plot.info$colvec[i]))
  }
}



for(i in 1:20){
  dat<-syv.mid[i+4]
  sm<-rollapply(dat,7,FUN=mean, na.rm=TRUE)
  if(i==1){
    plot(sm,type='l', ylim=c(0,100), main='SYV')
  }else{
    lines(sm, col=as.character(syv.plot.info$colvec[i]))
  }
}

######
wcr.tower.raw<-read.csv('WCR_Tower_clean.csv', skip=2)
wcr.names<-names(read.csv('WCR_Tower_clean.csv'))
colnames(wcr.tower.raw)<-wcr.names


wcr.tower<-wcr.tower.raw

wcr.tower.agg<-aggregate(wcr.tower, by=list(wcr.tower$DOY), FUN=mean, na.rm=TRUE)

wcr.sap.agg<-aggregate(wcr.2015, by=list(wcr.2015$DOY), FUN=mean, na.rm=TRUE)

for(i in 1:14){
  plot(wcr.sap.agg[wcr.sap.agg$DOY>120 & wcr.sap.agg$DOY<300,i+5]~wcr.tower.agg$VPD[wcr.tower.agg$DOY>120 & wcr.tower.agg$DOY<300], main=i, xlim=c(0,1.5))
}


wcr.sap.gs<-wcr.sap.agg[wcr.sap.agg$DOY>120 & wcr.sap.agg$DOY<300,6:19]
wcr.twr.gs<-wcr.tower.agg[wcr.tower.agg$DOY>120 & wcr.tower.agg$DOY<300,]


ngood.gs<-aggregate(wcr.2015, by=list(wcr.2015$DOY), FUN=function(wcr.2015)sum(is.na(wcr.2015)))[121:299,6:19]
plot(rowSums(wcr.sap.gs[unname(which(rowSums(ngood.gs)<168)),], na.rm=TRUE)~wcr.twr.gs$VPD[unname(which(rowSums(ngood.gs)<168))], main='sum', xlim=c(0,1.5))
#####
###### data gaps

wcr.2015.day<-aggregate(wcr.2015, by=list(wcr.2015$DOY), FUN=mean, na.rm=TRUE)
wcr.2016.day<-aggregate(wcr.2016, by=list(wcr.2016$DOY), FUN=mean, na.rm=TRUE)

syv.2015.day<-aggregate(syv.2015, by=list(syv.2015$DOY), FUN=mean, na.rm=TRUE)
syv.2016.day<-aggregate(syv.2016, by=list(syv.2016$DOY), FUN=mean, na.rm=TRUE)

wcr.2015.rg<-which(!is.na(rowMeans(wcr.2015.day[,6:19], na.rm=TRUE)))
wcr.2015.line<-rep(NA, 366)
wcr.2015.line[wcr.2015.rg]<-1

syv.2015.rg<-which(!is.na(rowMeans(syv.2015.day[,6:19], na.rm=TRUE)))
syv.2015.line<-rep(NA, 366)
syv.2015.line[syv.2015.rg]<-2

wcr.2016.rg<-which(!is.na(rowMeans(wcr.2016.day[,6:19], na.rm=TRUE)))
wcr.2016.line<-rep(NA, 366)
wcr.2016.line[wcr.2016.rg]<-3

syv.2016.rg<-which(!is.na(rowMeans(syv.2016.day[,6:19], na.rm=TRUE)))
syv.2016.line<-rep(NA, 366)
syv.2016.line[syv.2016.rg]<-4

plot(wcr.2015.line, type='l', ylim=c(0,5), col='orange4')
lines(syv.2015.line, col='forest green')
lines(wcr.2016.line, col='orange')
lines(syv.2016.line, col='green')

abline(v=105)
abline(v=135)
abline(v=166, lty=2)
abline(v=227, lty=2)
abline(v=274)
abline(v=304)




