#Figures! Yay!

#Subset to GS

wcr.sapfig<-wcr.gap[wcr.ts$DOY%in%gs,]
syv.sapfig<-syv.gap[syv.ts$DOY%in%gs,]

ts.gs<-wcr.ts[wcr.ts$DOY%in%gs,]


#Create daily average flux rates
wcr.sapday<-aggregate(wcr.gap[wcr.ts$DOY%in%gs,], by=list(wcr.ts$DOY[wcr.ts$DOY%in%gs]), FUN='mean')
syv.sapday<-aggregate(syv.gap[syv.ts$DOY%in%gs,], by=list(syv.ts$DOY[syv.ts$DOY%in%gs]), FUN='mean')

#Boxplots each tree. Informative, but crowded
#boxplot((wcr.sapday[2:15])[order(wcr.tree$SPP)], col=wcr.tree$col[order(wcr.tree$SPP)])
#boxplot((syv.sapday[2:21])[order(syv.tree$SPP)], col=syv.tree$col[order(syv.tree$SPP)])


#Maple barplots!
wcr.maple<-wcr.sapday[,1+which(wcr.tree$SPP=="ACSA")]
wcr.maple$mean<-rowMeans(wcr.maple)

syv.maple<-syv.sapday[,1+which(syv.tree$SPP=="ACSA")]
syv.maple$mean<-rowMeans(syv.maple)

par(mfrow=c(1,2))

boxplot(wcr.maple[,1:5], ylim=c(0,50), xlim=c(0,9),col=c(rep(NA,5), "orange"), main="WCR maples")
boxplot(wcr.maple[6],at=8, add=TRUE, col='orange', names="Mean", width=1.2)

boxplot(syv.maple, ylim=c(0,50), xlim=c(0,9),col=c(rep(NA,7), "orange"), main="SYV maples")

#Measured trees, flux
wcr.tree$flux<-colMeans(wcr.sapday[2:15])
syv.tree$flux<-colMeans(syv.sapday[2:21], na.rm=TRUE)

fluxord.wcr<-c(1,4,2,3)
  #order(aggregate(wcr.tree$flux, by=list(wcr.tree$SPP), FUN='mean', na.rm=TRUE)$x)
boxplot(wcr.tree$flux~wcr.tree$SPP, at=fluxord.wcr, col=c('orange', ' yellow green','dark red','yellow'), ylim=c(0,30))

fluxord.syv<-c(3,1,2,4)
#(aggregate(syv.tree$flux, by=list(syv.tree$SPP), FUN='mean', na.rm=TRUE)$x)
boxplot(syv.tree$flux~syv.tree$SPP, at=fluxord.syv, col=c('orange',' blue','dark red','forest green'), ylim=c(0,30))


#Profiles
#Option 1, multipanel

par(mfrow=c(2,2))
for(p in 1:length(unique(syv.tree$SPP))){
name<-as.character(unique(syv.tree$SPP)[p])
plot(syv.sapfig[,1]~ts.gs$HOUR, col='white', ylim=c(0,70), main=name)
for(i in which(syv.tree$SPP==name)){
 lines((aggregate(syv.sapfig[,i], by=list(ts.gs$HOUR), FUN='mean', na.rm=TRUE))$x, col=syv.tree$col[syv.tree$SPP==name],lwd=2)
}
}


for(p in 1:length(unique(wcr.tree$SPP))){
  name<-as.character(unique(wcr.tree$SPP)[p])
  plot(wcr.sapfig[,1]~ts.gs$HOUR, col='white', ylim=c(0,70), main=name)
  for(i in which(wcr.tree$SPP==name)){
    lines((aggregate(wcr.sapfig[,i], by=list(ts.gs$HOUR), FUN='mean', na.rm=TRUE))$x, col=wcr.tree$col[wcr.tree$SPP==name],lwd=2)
  }
}

#option 2, all on one

par(mfrow=c(1,2))
plot(syv.sapfig[,1]~ts.gs$HOUR, col='white', ylim=c(0,70))

for(p in 1:length(unique(syv.tree$SPP))){
  name<-as.character(unique(syv.tree$SPP)[p])
  for(i in which(syv.tree$SPP==name)){
    lines((aggregate(syv.sapfig[,i], by=list(ts.gs$HOUR), FUN='mean', na.rm=TRUE))$x, col=syv.tree$col[syv.tree$SPP==name],lwd=0.5, lty=3)
  }
  sp<-rowMeans(syv.sapfig[,syv.tree$SPP==name], na.rm=TRUE)
  lines(aggregate(sp, by=list(ts.gs$HOUR), FUN='mean', na.rm=TRUE)$x, col=syv.tree$col[syv.tree$SPP==name], lwd=3)
  
}


plot(wcr.sapfig[,1]~ts.gs$HOUR, col='white', ylim=c(0,70))

for(p in 1:length(unique(wcr.tree$SPP))){
  name<-as.character(unique(wcr.tree$SPP)[p])
  for(i in which(wcr.tree$SPP==name)){
    lines((aggregate(wcr.sapfig[,i], by=list(ts.gs$HOUR), FUN='mean', na.rm=TRUE))$x, col=wcr.tree$col[wcr.tree$SPP==name],lwd=0.5, lty=3)
  }
  sp<-rowMeans(wcr.sapfig[,wcr.tree$SPP==name], na.rm=TRUE)
  lines(aggregate(sp, by=list(ts.gs$HOUR), FUN='mean', na.rm=TRUE)$x, col=wcr.tree$col[wcr.tree$SPP==name], lwd=3)
  
}

#Option 3, maybe with error bars?
plot(syv.sapfig[,1]~ts.gs$HOUR, col='white', ylim=c(0,70))

for(p in 1:length(unique(syv.tree$SPP))){
  name<-as.character(unique(syv.tree$SPP)[p])

  sp<-rowMeans(syv.sapfig[,syv.tree$SPP==name], na.rm=TRUE)
  sphr<-aggregate(sp, by=list(ts.gs$HOUR), FUN='mean', na.rm=TRUE)$x
  lines(sphr, col=syv.tree$col[syv.tree$SPP==name], lwd=3)
  
  spsd<-aggregate(sp, by=list(ts.gs$HOUR), FUN='sd', na.rm=TRUE)$x
  
  arrows(unique(ts.gs$HOUR)+1, sphr+spsd,unique(ts.gs$HOUR)+1, sphr-spsd, length=0.01,angle=90, code=3)
}

plot(wcr.sapfig[,1]~ts.gs$HOUR, col='white', ylim=c(0,70))

for(p in 1:length(unique(wcr.tree$SPP))){
  name<-as.character(unique(wcr.tree$SPP)[p])
  
  sp<-rowMeans(wcr.sapfig[,wcr.tree$SPP==name], na.rm=TRUE)
  sphr<-aggregate(sp, by=list(ts.gs$HOUR), FUN='mean', na.rm=TRUE)$x
  lines(sphr, col=wcr.tree$col[wcr.tree$SPP==name], lwd=3)
  
  spsd<-aggregate(sp, by=list(ts.gs$HOUR), FUN='sd', na.rm=TRUE)$x
  
  arrows(unique(ts.gs$HOUR)+1, sphr+spsd,unique(ts.gs$HOUR)+1, sphr-spsd, length=0.01,angle=90, code=3)
}

###OKAY NOW FLOW

#Measured trees, flow
wcr.site<-sweep(wcr.gap, 2,((wcr.tree$SWA_calc/10000)*wcr.tree$MULT_calc), FUN='*')*1800/1000
syv.site<-sweep(syv.gap, 2,((syv.tree$SWA_calc/10000)*syv.tree$MULT_calc), FUN='*')*1800/1000

#
wcr.flow.gs<-wcr.site[wcr.ts$DOY%in%gs,]
wcr.flow.day<-aggregate(wcr.flow.gs,by=list(ts.gs$DOY), FUN='mean')

syv.flow.gs<-syv.site[syv.ts$DOY%in%gs,]
syv.flow.day<-aggregate(syv.flow.gs,by=list(ts.gs$DOY), FUN='mean')

wcr.flow.tr<-colMeans(wcr.flow.day)[2:15]
syv.flow.tr<-colMeans(syv.flow.day, na.rm=TRUE)[2:21]

wcr.tree$flow<-wcr.flow.tr
syv.tree$flow<-syv.flow.tr

boxplot(wcr.tree$flow~wcr.tree$SPP)
boxplot(syv.tree$flow~syv.tree$SPP)

#temporary
plot(aggregate(syv.site[,1], by=list(syv.master$H), FUN='mean', na.rm=TRUE), col='white', ylim=c(0,3))
for(i in 1:ncol(syv.site)){
  lines(aggregate(syv.site[,i], by=list(syv.master$H), FUN='mean', na.rm=TRUE), col=syv.tree$col[i])
}

plot(aggregate(wcr.site[,1], by=list(wcr.master$H), FUN='mean', na.rm=TRUE), col='white', ylim=c(0,3))
for(i in 1:ncol(wcr.site)){
  lines(aggregate(wcr.site[,i], by=list(wcr.master$H), FUN='mean', na.rm=TRUE), col=wcr.tree$col[i])
}