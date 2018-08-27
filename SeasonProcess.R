#source("Gapfill2") #Need to do twice, once for each site, or read 7/17 version from file (WCR_gapfill; SYV_gapfill)
source("Prepare_treedata.R")

#(((This edit for test push)))#


#Organize data
wcr.gapfill<- read.csv('WCR_gapfill.csv')[2:15]
wcr.dat<-data.frame(wcr.gapfill)
wcr.dat[wcr.dat<0]<-0
wcr.names<-colnames(read.csv('WCR_2016_SAPFLUX.csv'))
wcr.ts<-read.csv('WCR_2016_SAPFLUX.csv')[1:4]
colnames(wcr.dat)<-wcr.names[5:18]

syv.gapfill<- read.csv('SYV_gapfill.csv')[2:21]
syv.dat<-data.frame(syv.gapfill)
syv.dat[syv.dat<0]<-0
syv.names<-colnames(read.csv('SYV_2016_SAPFLUX.csv'))
syv.ts<-(read.csv('SYV_2016_SAPFLUX.csv'))[1:4]
colnames(syv.dat)<-syv.names[5:24]


##Scale to sap flow
#SYV
treemult.syv<-(syv.tree$MULT_meas*(syv.tree$SWA_meas/10000))
treemult.syv[3]<-(syv.tree$SWA_meas[3])/10000

Flow<-(sweep(syv.dat,2,treemult.syv,'*'))
Flow.pd<-Flow*1800  #1800 seconds in 30 min

syv.flow<-cbind(syv.ts,Flow)
syv.flow.pd<-cbind(syv.ts,Flow.pd)

rm('treemult.syv','Flow','Flow.pd')

#WCR

treemult.wcr<-(wcr.tree$MULT_meas*(wcr.tree$SWA_meas/10000))
treemult.wcr[3]<-(wcr.tree$SWA_meas[3])/10000

Flow<-(sweep(wcr.dat,2,treemult.wcr,'*'))
Flow.pd<-Flow*1800  #1800 seconds in 30 min

wcr.flow<-cbind(wcr.ts,Flow)
wcr.flow.pd<-cbind(wcr.ts,Flow.pd)

rm('treemult.wcr','Flow','Flow.pd')

##Now for some analysis

par(mfrow=c(1,2))

#Site level timeseries WCR
site.wcr<-(aggregate(wcr.flow.pd, by=list(wcr.flow.pd$DOY), FUN=sum))
agg.site.wcr<-rowSums(site.wcr[,6:19])
plot(agg.site.wcr/14, ylim=c(0,30000), main='WCR all trees', ylab='Sapflow (g/day)', xlab='DOY')

#Site level timeseries SYV
site.syv<-(aggregate(syv.flow.pd, by=list(syv.flow.pd$DOY), FUN=sum))
site.syv$S11<-NA
#site.syv$S7<-NA
agg.site.syv<-rowSums(site.syv[,6:25],na.rm=TRUE )
plot(agg.site.syv/18,ylim=c(0,30000), main='SYV all trees', ylab='Sapflow (g/day)', xlab='DOY')

par(mfrow=c(1,2))
agg.site.syv[agg.site.syv==0]<-NA
#agg.site.syv[187:195]<-NA
agg.site.wcr[agg.site.wcr==0]<-NA
diff<-(agg.site.wcr/14)-(agg.site.syv/18)
plot(diff, type='l', main='wcr-syv daily', xlab='DOY')
abline(h=0, col='red', lwd=1)
abline(v=c(152,274), col=c('green', 'orange3'), lwd=2, lty=2)

library(zoo)
sm<-rollapply(diff,30,FUN=mean, na.rm=TRUE)
plot(sm, type='l', lwd=2, main='wcr-syv smoothed', xlab='DOY')
abline(h=0, col='red', lwd=1)
abline(v=c(152,274), col=c('green', 'orange3'), lwd=2, lty=2)


#How about species level trends
#####
HL<-c(1:4,8,10,19:20)  #11 is left out because bad data quality
SM<-c(9,13:18)
HB<-c(5,6)
YB<-c(7,12)

site.syv.dat<-site.syv[,6:25]

HL.tr<-rowMeans(site.syv.dat[,HL])
SM.tr<-rowMeans(site.syv.dat[,SM], na.rm<-TRUE) #Few trees dead in winter; this gives better timeseries
HB.tr<-rowMeans(site.syv.dat[,HB])
YB.tr<-rowMeans(site.syv.dat[,YB], na.rm=TRUE)

HLmax<-max(rowMeans(site.syv.dat[,HL]), na.rm=TRUE)
SMmax<-max(rowMeans(site.syv.dat[,SM]), na.rm=TRUE)

par(mfrow=c(2,2))

#Straight
plot(HL.tr, type='l', main='Hemlock', ylab='HL', col='forest green', ylim=c(0,25000))
plot(SM.tr, type='l', main='Sugar Maple', ylab='SM', col='orange', ylim=c(0,25000))
plot(HB.tr, type='l', main='Hophornbeam', ylab='HB',col='dark red', ylim=c(0,25000))
plot(YB.tr, type='l', main='Yellow Birch', ylab='YB', col='blue', ylim=c(0,80000))

#Normalized by max
HLnorm<-(HL.tr/HLmax)
SMnorm<-(SM.tr/SMmax)


par(mfrow=c(1,2))
plot(HLnorm-SMnorm, type='l', main='SYV HL-SM, day', ylab='normSF')
plot(rollapply(HLnorm-SMnorm, 7, FUN=mean), type='l', lwd=2, main='SYV HL-SM, sm', ylab='normSF')
abline(h=0, col='red',lwd='2')


#Species level for WCR

AB<-c(1:2,8,11)  
SM<-c(4:6,12:13)
HB<-c(3,7,14)
GA<-c(9:10)

site.wcr.dat<-site.wcr[,6:19]

AB.tr<-rowMeans(site.wcr.dat[,AB])
SM.tr<-rowMeans(site.wcr.dat[,SM], na.rm<-TRUE) #Few trees dead in winter; this gives better timeseries
HB.tr<-rowMeans(site.wcr.dat[,HB])
GA.tr<-rowMeans(site.wcr.dat[,GA], na.rm=TRUE)


par(mfrow=c(2,2))
#Straight
plot(AB.tr, type='l', main='Basswood', ylab='AB', col='yellow', ylim=c(0,25000))
plot(SM.tr, type='l', main='Sugar Maple', ylab='SM', col='orange', ylim=c(0,50000))
plot(HB.tr, type='l', main='Hophornbeam', ylab='HB',col='dark red', ylim=c(0,25000))
plot(GA.tr, type='l', main='Green Ash', ylab='GA', col='yellow green', ylim=c(0,25000))
#####
##Bring in leaf area!
LAI<-read.csv('LAI_2016_2017.csv')
LAI.2016<-LAI[LAI$YEAR==2016,]

#####
#Reshape LAI data to daily
#####
lai.wcr<-approx(LAI.2016$DOY,LAI.2016$WCR,n=126)
lai.wcrm<-approx(LAI.2016$DOY,LAI.2016$WCRM,n=126)
lai.syv<-approx(LAI.2016$DOY,LAI.2016$SYV, n=135)
lai.und<-approx(LAI.2016$DOY,LAI.2016$UND, n=96)

doys<-seq(130,264)
syv.l<-data.frame(cbind(doys,lai.syv$y))

doys<-seq(139,264)
wcr.l<-data.frame(cbind(doys,lai.wcr$y))
wcrm.l<-data.frame(cbind(doys,lai.wcrm$y))

doys<-seq(169,264)
und.l<-data.frame(cbind(doys,lai.und$y))

lai.1<-merge(syv.l,wcr.l, by="doys",all.x=TRUE,all.y=TRUE)
lai.2<-merge(lai.1,wcrm.l, by="doys",all.x=TRUE,all.y=TRUE)
lai.3<-merge(lai.2,und.l, by="doys",all.x=TRUE,all.y=TRUE)

LAI.dat<-lai.3
colnames(LAI.dat)<-c('DOY','SYV',"WCR","WCRM","UND")

rm('lai.1','lai.2','lai.3','und.l','wcr.l','syv.l','wcrm.l','lai.wcr','lai.wcrm','lai.und','lai.syv')
#####
#LAI plot
#####
par(mfrow=c(1,1))
plot(LAI.dat$SYV~LAI.dat$DOY, type='l', ylim=c(0.2,6.7), col='gray2', main='Leaf Area Index', ylab='LAI',xlab='DOY',lwd=2, font=2, font.lab=2)

points(LAI.2016$SYV~LAI.2016$DOY, col='gray2', pch=2, lwd=2)
lines(LAI.dat$WCR~LAI.dat$DOY, col='blue', lwd=2)
points(LAI.2016$WCR~LAI.2016$DOY, col='blue', pch=1, lwd=2)

grid(NULL,col='dark gray')

#lines(LAI.dat$WCRM~LAI.dat$DOY, col='gray', lty=2, lwd=2)
#points(LAI.2016$WCRM~LAI.2016$DOY, col='gray', pch=1)

#lines(LAI.dat$UND~LAI.dat$DOY, col='light blue')
#points(LAI.2016$UND~LAI.2016$DOY, col='light blue', pch=4)

legend(170,3,legend=c('SYV',"WCR"), #,"WCRM","UND"), 
       col=c("gray2","gray"), #,"orange","light blue"), 
       lwd=2, ncol=1, cex=0.8)

#####
#Plotting flux values
#####
syv.flux.dat<-cbind(syv.ts,syv.dat)
syv.flux.daily<-(aggregate(syv.flux.dat, by=list(syv.flux.dat$DOY), FUN=mean))
syv.flux.daily<-syv.flux.daily[,c(3,6:25)]
#Fix the damn canopy classes
syv.tree.orig<-syv.tree
syv.tree$CC[c(1,5,11,14,17)]<-c('D','S','I','I','C')

#Define for WCR
wcr.flux.dat<-cbind(wcr.ts,wcr.dat)
wcr.flux.daily<-(aggregate(wcr.flux.dat, by=list(wcr.flux.dat$DOY), FUN=mean))
wcr.flux.daily<-wcr.flux.daily[,c(3,6:19)]


#Symbolize by species and CC, SYV

col.class<-rep('forest green',20)
col.class[syv.tree$SPP=='ACSA']<-'orange'
col.class[syv.tree$SPP=='OSVI']<-'dark red'
col.class[syv.tree$SPP=='BEAL']<-'blue'
col.class->col.class.syv

pch.class<-rep('S',20)
pch.class[syv.tree$CC=='D']<-'D'
pch.class[syv.tree$CC=='C']<-'C'
pch.class[syv.tree$CC=='I']<-'N'
pch.class->pch.class.syv

meanflux.lai<-colMeans(syv.flux.daily[130:264,], na.rm=TRUE)
syv.tree.vis<-cbind(syv.tree,col.class,pch.class, meanflux.lai[2:21])
sort<-syv.tree.vis[order(syv.tree.vis$DBH), ]
plot(sort$`meanflux.lai[2:21]`,col=as.character(sort$col.class), pch=as.character(sort$pch.class))

par(mfrow=c(2,3))
doystart.var<-c(130,160,190,220,250,280)
for(i in 1:5){
  meanflux.lai<-colMeans(syv.flux.daily[doystart.var[i]:doystart.var[i+1],], na.rm=TRUE)
  syv.tree.vis<-cbind(syv.tree,col.class,pch.class, meanflux.lai[2:21])
  sort<-syv.tree.vis[order(syv.tree.vis$DBH), ]
  plot(sort$`meanflux.lai[2:21]`~sort$DBH,col=as.character(sort$col.class), 
       pch=as.character(sort$pch.class), main=paste("u flux DOY",doystart.var[i],'to',doystart.var[i+1]), 
       ylim=c(0,30), ylab='Flux (g/m2 s)', xlab='DBH', xlim=c(0,90))
}

#Symbolize by species and CC, WCR

col.class<-rep('yellow',14)
col.class[wcr.tree$SPP=='ACSA']<-'orange'
col.class[wcr.tree$SPP=='OSVI']<-'dark red'
col.class[wcr.tree$SPP=='FRPE']<-'yellow green'
col.class->col.class.wcr

pch.class<-rep('S',14)
pch.class[wcr.tree$CC=='D']<-'D'
pch.class[wcr.tree$CC=='C']<-'C'
pch.class[wcr.tree$CC=='I']<-'N'
pch.class->pch.class.wcr

meanflux.lai<-colMeans(wcr.flux.daily[130:264,], na.rm=TRUE)
wcr.tree.vis<-cbind(wcr.tree,col.class,pch.class, meanflux.lai[2:15])
sort<-wcr.tree.vis[order(wcr.tree.vis$DBH), ]
plot(sort$`meanflux.lai[2:15]`,col=as.character(sort$col.class), pch=as.character(sort$pch.class))

par(mfrow=c(2,3))
doystart.var<-c(130,160,190,220,250,280)
for(i in 1:5){
  meanflux.lai<-colMeans(wcr.flux.daily[doystart.var[i]:doystart.var[i+1],], na.rm=TRUE)
  wcr.tree.vis<-cbind(wcr.tree,col.class,pch.class, meanflux.lai[2:15])
  sort<-wcr.tree.vis[order(wcr.tree.vis$DBH), ]
  plot(sort$`meanflux.lai[2:15]`~sort$DBH,col=as.character(sort$col.class), 
       pch=as.character(sort$pch.class), main=paste("u flux DOY",doystart.var[i],'to',doystart.var[i+1]), 
       ylim=c(0,30), ylab='Flux (g/m2 s)', xlab='DBH', xlim=c(0,90))
}

#overall...
par(mfrow=c(2,1), mar=c(4,4.2,4,2))
plot(colMeans(wcr.flow.pd[5:18], na.rm=TRUE)~wcr.tree$DBH, col=col.class.wcr, pch=pch.class.wcr, ylim=c(0,1000), xlim=c(5,90))
#abline(a=-20,b=5)
plot(colMeans(syv.flow.pd[5:24], na.rm=TRUE)~syv.tree$DBH, col=col.class.syv, pch=pch.class.syv, ylim=c(0,1000), xlim=c(5,90))
#abline(a=-20,b=5)

#Without letters
plot(colMeans(wcr.flow.pd[5:18], na.rm=TRUE)~wcr.tree$DBH, col=col.class.wcr, pch='*', ylim=c(0,1000), xlim=c(5,90), cex=3.5, xlab='DBH', ylab="Avg. Daily Flow (L)", 
     main='Second Growth', font=2,font.lab=2, cex.lab=1.2, cex.main=1.5)
legend(2,1200,legend=c('Sugar Maple','Basswood','Green Ash','Hophornbeam'), 
       col=c('orange','yellow','yellow green','dark red'), cex=0.9,
       pch='*',pt.cex=3, x.intersp=0.6,y.intersp=0.4, bty='n', text.font=2)

#30 min in g - correct units

#abline(a=-20,b=5)
plot(colMeans(syv.flow.pd[5:24], na.rm=TRUE)~syv.tree$DBH, col=col.class.syv, pch='*', ylim=c(0,1000), xlim=c(5,90), cex=3.5, xlab='DBH', ylab="Avg. Daily Flow (L)", 
     main='Old Growth', font=2,font.lab=2, cex.lab=1.2, cex.main=1.5)
legend(2,1200,legend=c('Sugar Maple','Yellow Birch','Eastern Hemlock','Hophornbeam'), 
       col=c('orange','blue','forest green','dark red'), cex=0.9,
       pch='*',pt.cex=3, x.intersp=0.6,y.intersp=0.4, bty='n', text.font=2)





#####
#Scaling to stand
#####

#Making means, SYV

HL<-c(1:4,8,10,19:20)  #11 is left out because bad data quality
SM<-c(9,13:18)
HB<-c(5,6)
YB<-c(7,12)

HL.tr.f<-rowMeans(syv.flux.daily[,HL+1])
SM.tr.f<-rowMeans(syv.flux.daily[,SM+1], na.rm<-TRUE) #Few trees dead in winter; this gives better timeseries
HB.tr.f<-rowMeans(syv.flux.daily[,HB+1])
YB.tr.f<-rowMeans(syv.flux.daily[,YB+1], na.rm=TRUE)

syv.lut<-data.frame(cbind(HL.tr.f,SM.tr.f,HB.tr.f,YB.tr.f))
colnames(syv.lut)<-c('TSCA','ACSA','OSVI','BEAL')


#Making means, WCR

AB<-c(1:2,8,11)  
SM<-c(4:6,12:13)
HB<-c(3,7,14)
GA<-c(9:10)


AB.tr.f<-rowMeans(wcr.flux.daily[,AB+1])
SM.tr.f<-rowMeans(wcr.flux.daily[,SM+1], na.rm<-TRUE) #Few trees dead in winter; this gives better timeseries
HB.tr.f<-rowMeans(wcr.flux.daily[,HB+1])
GA.tr.f<-rowMeans(wcr.flux.daily[,GA+1], na.rm=TRUE)

wcr.lut<-data.frame(cbind(SM.tr.f,AB.tr.f,HB.tr.f,GA.tr.f))
colnames(wcr.lut)<-c('ACSA','TIAM','OSVI','FRPE')


#Actual scaling
syv.forest.rad<-read.csv('SYV_FOREST_RAD.csv')
wcr.forest.rad<-read.csv('WCR_FOREST_RAD.csv')
#Fucking factors
syv.forest.rad$Multiplier<-as.numeric(levels(syv.forest.rad$Multiplier)[syv.forest.rad$Multiplier])
#wcr.forest.rad$Multiplier<-as.numeric(levels(wcr.forest.rad$Multiplier)[wcr.forest.rad$Multiplier])

#SYV
DAT<-syv.forest.rad

treemult.syv<-(DAT$Multiplier*(DAT$SWA/10000))  #convert cm2 to m2
treemult.syv[297]<-(DAT$SWA[297])/10000

#SYV giant ass loop

syv.scale<-function(lut,treemult){

syv.lut<-lut
treemult.syv<-treemult

alltree.syv<-matrix(nrow=nrow(syv.lut), ncol=length(treemult.syv))

for (t in 1:length(treemult.syv)){
  if(syv.forest.rad$SPP[t]=='acsa'){
    alltree.syv[,t]<-treemult.syv[t]*syv.lut$ACSA
  }else if(syv.forest.rad$SPP[t]=='tsca'){
    alltree.syv[,t]<-treemult.syv[t]*syv.lut$TSCA
  }else if(syv.forest.rad$SPP[t]=='osvi'){
    alltree.syv[,t]<-treemult.syv[t]*syv.lut$OSVI
  }else if(syv.forest.rad$SPP[t]=='beal'){
    alltree.syv[,t]<-treemult.syv[t]*syv.lut$BEAL
  }else{
    alltree.syv[,t]<-treemult.syv[t]*rowMeans(syv.lut)
  }
}
alltree.syv<-data.frame(alltree.syv)  #units: g/s  (m2 * g/m2-s)
alltree.syv.pd<-alltree.syv*86400/1000
alltree.syv.pd.new<<-alltree.syv.pd  #Convert to kg/day which is the same as L/day
site.total.syv<<-rowSums(alltree.syv.pd)*1.27  #1.27 accounts for areas not surveyed
}

syv.scale(syv.lut,treemult.syv)
alltree.syv.pd.new->alltree.syv.pd

#WCR
DAT<-wcr.forest.rad

treemult.wcr<-(DAT$Multiplier*(DAT$SWA/10000))  #convert cm2 to m2
treemult.wcr[297]<-(DAT$SWA[297])/10000

#wcr giant ass loop
wcr.scale<-function(lut,treemult){
  
wcr.lut<-lut
treemult.wcr<-treemult
  
alltree.wcr<-matrix(nrow=nrow(wcr.lut), ncol=length(treemult.wcr))

for (t in 1:length(treemult.wcr)){
  if(wcr.forest.rad$SPP[t]=='acsa'){
    alltree.wcr[,t]<-treemult.wcr[t]*wcr.lut$ACSA
  }else if(wcr.forest.rad$SPP[t]=='tiam'){
    alltree.wcr[,t]<-treemult.wcr[t]*wcr.lut$TIAM
  }else if(wcr.forest.rad$SPP[t]=='osvi'){
    alltree.wcr[,t]<-treemult.wcr[t]*wcr.lut$OSVI
  }else if(wcr.forest.rad$SPP[t]=='frpe'){
    alltree.wcr[,t]<-treemult.wcr[t]*wcr.lut$FRPE
  }else{
    alltree.wcr[,t]<-treemult.wcr[t]*rowMeans(wcr.lut)
  }
}
alltree.wcr<-data.frame(alltree.wcr)  #units: g/s  (m2 * g/m2-s)
alltree.wcr.pd<-alltree.wcr*86400/1000
alltree.wcr.pd.new<<-alltree.wcr.pd  #Convert to kg/day which is the same as L/day
site.total.wcr<<-rowSums(alltree.wcr.pd)*1.27  #1.27 accounts for areas not surveyed
}

wcr.scale(wcr.lut,treemult.wcr)
alltree.wcr.pd.new->alltree.wcr.pd

#####
par(mfrow=c(1,2))
plot(site.total.wcr)
plot(site.total.syv)
par(mfrow=c(1,1))
plot(site.total.wcr-site.total.syv, type='l')
abline(h=0, col='red')


######
#Time for flux per leaf area

#Setting DOY bounds for LAI data
lai.min<-130
lai.max<-264
doys<-(lai.min:lai.max)

wcr.laidat<-alltree.wcr.pd[lai.min:lai.max,]
syv.laidat<-alltree.syv.pd[lai.min:lai.max,]

wcr.laiscale<-rowSums(wcr.laidat)/(LAI.dat$WCR*6400)
syv.laiscale<-rowSums(syv.laidat)/(LAI.dat$SYV*6400)

par(mfrow=c(1,2))  
plot(wcr.laiscale~doys, ylim=c(0,0.52), main='WCR per LA', ylab='Flow (kg) per m2 leaf area per day')
plot(rowSums(wcr.laidat)~doys, ylim=c(0,10000), main='WCR bulk', ylab='Total flow (kg/day)')

plot(syv.laiscale~doys, ylim=c(0,0.52), main='SYV per LA',ylab='Flow (kg) per m2 leaf area per day')
plot(rowSums(syv.laidat)~doys, ylim=c(0,10000), main='SYV bulk', ylab='Total flow (kg/day)')
#####
#Rhemtulla style plots of contributions

#SYV
syv.totals<-rowSums(alltree.syv.pd)
syv.hl.pct<-rollapply(rowSums(alltree.syv.pd[,syv.forest.rad$SPP=='tsca'])/syv.totals, 14, FUN='mean', na.rm=TRUE)
syv.sm.pct<-rollapply(rowSums(alltree.syv.pd[,syv.forest.rad$SPP=='acsa'])/syv.totals, 14, FUN='mean', na.rm=TRUE)
syv.hb.pct<-rollapply(rowSums(alltree.syv.pd[,syv.forest.rad$SPP=='osvi'])/syv.totals, 14, FUN='mean', na.rm=TRUE)
syv.yb.pct<-rollapply(rowSums(alltree.syv.pd[,syv.forest.rad$SPP=='beal'])/syv.totals, 14, FUN='mean', na.rm=TRUE)

pcts<-data.frame(cbind(syv.hl.pct,syv.sm.pct,syv.hb.pct,syv.yb.pct))

syv.sm.pct[is.na(syv.sm.pct)]<-0
syv.yb.pct[is.na(syv.yb.pct)]<-0
syv.hb.pct[is.na(syv.hb.pct)]<-0
syv.hl.pct[is.na(syv.hl.pct)]<-0

#gs<-c(125:304)
gs<-c(10:350)
#now with prct?
plot(syv.sm.pct[gs], col='white', ylim=c(0,1), xlim=c(min(gs), max(gs)), main='SYV', ylab="% sap flow contributed", xlab="DOY", font=2,font.lab=2)
polygon(y=c(0,1,1,0),x=c(min(gs),min(gs),max(gs),max(gs)), col='gray')
polygon(y=c(syv.sm.pct[gs]+syv.hl.pct[gs]+syv.yb.pct[gs]+syv.hb.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)),col='dark red')
polygon(y=c(syv.sm.pct[gs]+syv.hl.pct[gs]+syv.yb.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)),col='blue')
polygon(y=c(syv.sm.pct[gs]+syv.hl.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)),col='forest green')
polygon(y=c(syv.sm.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)), col='orange')
polygon(x=c(155,155,175,175),y=c(0,1.01,1.01,0), col='white', border=NA)
polygon(x=c(75,75,110,110),y=c(0,1.01,1.01,0), col='white', border=NA)


# plot(syv.sm.pct[gs], ylim=c(0,1), col='orange',type='l', main='stackline')
# lines(syv.hl.pct[gs]+syv.sm.pct[gs], col='forest green')
# lines(syv.hl.pct[gs]+syv.sm.pct[gs]+syv.hb.pct[gs], col='dark red')
# lines(syv.hl.pct[gs]+syv.sm.pct[gs]+syv.hb.pct[gs]+syv.yb.pct[gs], col='blue')
# 
# plot(syv.sm.pct[gs], ylim=c(0,1), col='orange',type='l', main='sep')
# lines(syv.hl.pct[gs],col='forest green')
# lines(syv.hb.pct[gs], col='dark red')
# lines(syv.yb.pct[gs], col='blue')


syv.hl.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest.rad$SPP=='tsca']), 14, FUN='mean', na.rm=TRUE)
syv.sm.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest.rad$SPP=='acsa']), 14, FUN='mean', na.rm=TRUE)
syv.hb.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest.rad$SPP=='osvi']), 14, FUN='mean', na.rm=TRUE)
syv.yb.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest.rad$SPP=='beal']), 14, FUN='mean', na.rm=TRUE)

tots<-data.frame(cbind(syv.hl.tot,syv.sm.tot,syv.hb.tot,syv.yb.tot))

# plot(syv.sm.tot[gs], ylim=c(0,5000), col='orange',type='l', main='stackline')
# lines(syv.hl.tot[gs]+syv.sm.tot[gs], col='forest green')
# lines(syv.hl.tot[gs]+syv.sm.tot[gs]+syv.hb.tot[gs], col='dark red')
# lines(syv.hl.tot[gs]+syv.sm.tot[gs]+syv.hb.tot[gs]+syv.yb.tot[gs], col='blue')

syv.sm.tot[is.na(syv.sm.tot)]<-0
syv.hl.tot[is.na(syv.hl.tot)]<-0
syv.yb.tot[is.na(syv.yb.tot)]<-0
syv.hb.tot[is.na(syv.hb.tot)]<-0

plot(syv.sm.tot[gs], col='white', ylim=c(0,6000), xlim=c(min(gs),max(gs)), main='SYV', ylab="Sap flow (L day-1)", xlab='DOY', font=2, font.lab=2)
polygon(y=c(syv.sm.tot[gs]+syv.hl.tot[gs]+syv.yb.tot[gs]+syv.hb.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='dark red')
polygon(y=c(syv.sm.tot[gs]+syv.hl.tot[gs]+syv.yb.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='blue')
polygon(y=c(syv.sm.tot[gs]+syv.hl.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='forest green')
polygon(y=c(syv.sm.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)), col='orange')
#mask out weird smoothing
polygon(x=c(156,156,177,177),y=c(0,5000,5000,0), col='white', border=NA)
polygon(x=c(75,75,110,110),y=c(0,5000,5000,0), col='white', border=NA)


#WCR
wcr.totals<-rowSums(alltree.wcr.pd)
wcr.ab.pct<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.rad$SPP=='tiam'])/wcr.totals, 14, FUN='mean', na.rm=TRUE)
wcr.sm.pct<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.rad$SPP=='acsa'])/wcr.totals, 14, FUN='mean', na.rm=TRUE)
wcr.hb.pct<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.rad$SPP=='osvi'])/wcr.totals, 14, FUN='mean', na.rm=TRUE)
wcr.ga.pct<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.rad$SPP=='frpe'])/wcr.totals, 14, FUN='mean', na.rm=TRUE)

pcts<-data.frame(cbind(wcr.ab.pct,wcr.sm.pct,wcr.hb.pct,wcr.ga.pct))

wcr.sm.pct[is.na(wcr.sm.pct)]<-0
wcr.ga.pct[is.na(wcr.ga.pct)]<-0
wcr.hb.pct[is.na(wcr.hb.pct)]<-0
wcr.ab.pct[is.na(wcr.ab.pct)]<-0

gs<-c(125:304)

plot(wcr.sm.pct[gs], col='white', ylim=c(0,1), xlim=c(min(gs), max(gs)), main='WCR', ylab="% sap flow", xlab="DOY", font=2,font.lab=2, cex.lab=1.2)
polygon(y=c(0,1,1,0),x=c(min(gs),min(gs),max(gs),max(gs)), col='gray')
polygon(y=c(wcr.sm.pct[gs]+wcr.ab.pct[gs]+wcr.ga.pct[gs]+wcr.hb.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)),col='dark red')
polygon(y=c(wcr.sm.pct[gs]+wcr.ab.pct[gs]+wcr.ga.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)),col='yellow green')
polygon(y=c(wcr.sm.pct[gs]+wcr.ab.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)),col='yellow')
polygon(y=c(wcr.sm.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)), col='orange')
polygon(y=c(0,1.01,1.01,0), x=c(247,247,252,252), col='white',border=NA)
polygon(y=c(0,1.01,1.01,0), x=c(39,39,46,46), col='white',border=NA)
polygon(y=c(0,1.01,1.01,0), x=c(330,330,365,365), col='white',border=NA)


wcr.ab.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.rad$SPP=='tiam']), 14, FUN='mean', na.rm=TRUE)
wcr.sm.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.rad$SPP=='acsa']), 14, FUN='mean', na.rm=TRUE)
wcr.hb.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.rad$SPP=='osvi']), 14, FUN='mean', na.rm=TRUE)
wcr.ga.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.rad$SPP=='frpe']), 14, FUN='mean', na.rm=TRUE)

tots<-data.frame(cbind(wcr.ab.tot,wcr.sm.tot,wcr.hb.tot,wcr.ga.tot))

wcr.sm.tot[is.na(wcr.sm.tot)]<-0
wcr.ab.tot[is.na(wcr.ab.tot)]<-0
wcr.ga.tot[is.na(wcr.ga.tot)]<-0
wcr.hb.tot[is.na(wcr.hb.tot)]<-0

plot(wcr.sm.tot[gs], col='white', ylim=c(0,6000), xlim=c(min(gs),max(gs)),main='WCR', ylab="Sap flow (L day-1)", xlab='DOY', font=2, font.lab=2)
polygon(y=c(wcr.sm.tot[gs]+wcr.ab.tot[gs]+wcr.ga.tot[gs]+wcr.hb.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='dark red')
polygon(y=c(wcr.sm.tot[gs]+wcr.ab.tot[gs]+wcr.ga.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='yellow green')
polygon(y=c(wcr.sm.tot[gs]+wcr.ab.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='yellow')
polygon(y=c(wcr.sm.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)), col='orange')
#mask out weird smoothing
polygon(y=c(0,6000,6000,0), x=c(247,247,252,252), col='white',border=NA)
polygon(y=c(0,6000,6000,0), x=c(39,39,46,46), col='white',border=NA)
polygon(y=c(0,6000,6000,0), x=c(330,330,365,365), col='white',border=NA)

#Universal legend
plot(c(1:10),c(1:10), col='white')
legend(1,10,legend=c('Sugar Maple','Eastern Hemlock','Yellow Birch','Basswood','Hophornbeam','Other'),
       fill=c('orange','forest green','blue','yellow','dark red','gray'), text.font=2)


#Intersite comparison

#Overlain silhouettes
plot(wcr.sm.tot[gs], col='white', ylim=c(0,6000), xlim=c(min(gs),max(gs)), ylab="Sap flow (L day-1)", xlab='DOY', font=2, font.lab=2)
polygon(y=c(wcr.sm.tot[gs]+wcr.ab.tot[gs]+wcr.ga.tot[gs]+wcr.hb.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='dark graY')
polygon(y=c(syv.sm.tot[gs]+syv.hl.tot[gs]+syv.yb.tot[gs]+syv.hb.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='black')

polygon(x=c(156,156,177,177),y=c(0,3000,3000,0), col='dark gray', border=NA)
polygon(x=c(170,170,177,177),y=c(3000,4500,4500,3000), col='dark gray', border=NA)


movwin<-function(subject,length){
  sm<-rollapply(subject, length, FUN='mean',na.rm=TRUE)
  sm.new<<-sm
}

wcr.sm<-movwin(wcr.totals,14)
syv.sm<-movwin(syv.totals,14)
diffs<-wcr.sm-syv.sm
allbad<-c(75:110,156:177,247:252,330:365)
diffs[allbad]<-NA

diffs.gf<-diffs

diffs.gf[75:111]<-seq(from=diffs[74], to=diffs[111], length.out=111-74)
diffs.gf[246:253]<-seq(from=diffs[245], to=diffs[253], length.out=253-245)
diffs.gf[155:178]<-seq(from=diffs[155], to=diffs[178], length.out=178-154)

#Line (w/color code)
par(mfrow=c(1,1))
plot(diffs.gf, type='l', main='WCR-SYV', ylab='Transp. difference (L day-1)', lwd=3, lty=2, font=2, font.lab=2, xlab='Day of Year', col='white')
lines(diffs.gf, col='dark red', lwd=2,lty=2);lines(diffs, lwd=3, col='dark red')
clip(0, length(diffs.gf),0, max(diffs.gf, na.rm=TRUE))
lines(diffs.gf, col='forest green', lwd=2,lty=2);lines(diffs, lwd=3, col='forest green')
abline(h=0, lwd=2)

#Blue-black style (for use with TS_analyses plots)
syv.sm[156:176]<-NA
plot(wcr.sm, col='blue', type='l', lwd=3, ylab='Transpiration (L Day-1)', xlab='Day of Year')
lines(syv.sm, type='l',lwd=3)


#####
###Normalizing by BA
#####
WCR.ba.sm<-sum(wcr.forest.rad$BA[wcr.forest.rad$SPP=='acsa'])
WCR.ba.ab<-sum(wcr.forest.rad$BA[wcr.forest.rad$SPP=='tiam'])
WCR.ba.hb<-sum(wcr.forest.rad$BA[wcr.forest.rad$SPP=='osvi'])
WCR.ba.ga<-sum(wcr.forest.rad$BA[wcr.forest.rad$SPP=='frpe'])
WCR.ba.uk<-sum(wcr.forest.rad$BA[wcr.forest.rad$SPP=='uk'])

WCR.ba<-c(WCR.ba.sm,WCR.ba.ab,WCR.ba.hb,WCR.ba.ga,WCR.ba.uk)
names(WCR.ba)<-c('acsa','tiam','osvi','frpe', 'unknown')

wcr.ba.norm<-WCR.ba/sum(wcr.forest.rad$BA)

#flux per basal area
wcr.acsanorm<-wcr.sm.pct/wcr.ba.norm[1]
wcr.tiamnorm<-wcr.ab.pct/wcr.ba.norm[2]
wcr.osvinorm<-wcr.hb.pct/wcr.ba.norm[3]
wcr.frpenorm<-wcr.ga.pct/wcr.ba.norm[4]
#wcr.uknorm<-wcr.uk.pct/wcr.ba.norm[5]

wcr.perba<-cbind(wcr.acsanorm,wcr.tiamnorm,wcr.osvinorm,wcr.frpenorm)

plot(wcr.perba[,1], col='white', type='l', ylim=c(0,5.0), xlim=c(min(gs),max(gs)), main='WCR sapflow per basal area', 
     xlab='DOY', ylab=" % Sapflow / % BA", font=2, font.lab=2, cex.lab=1.2)
cols<-c('orange','yellow','dark red','yellow green')
for(i in 1:4){
  lines(wcr.perba[,i], col=cols[i], lwd=5)
}
abline(h=1,lty=2)
polygon(y=c(-0.1,6,6,-0.1), x=c(247,247,254,254), col='white',border=NA)
#legend(x=155,y=4.6,legend=c("ACSA","OSVI","TIAM","FRPE"), col=c('orange','dark red','yellow','yellow green'), ncol=2, lwd=3, cex=0.8)


SYV.ba.sm<-sum(syv.forest.rad$BA[syv.forest.rad$SPP=='acsa'])
SYV.ba.hl<-sum(syv.forest.rad$BA[syv.forest.rad$SPP=='tsca'])
SYV.ba.hb<-sum(syv.forest.rad$BA[syv.forest.rad$SPP=='osvi'])
SYV.ba.yb<-sum(syv.forest.rad$BA[syv.forest.rad$SPP=='beal'])
SYV.ba.uk<-sum(syv.forest.rad$BA[syv.forest.rad$SPP=='uk'])

SYV.ba<-c(SYV.ba.sm,SYV.ba.hl,SYV.ba.hb,SYV.ba.yb, SYV.ba.uk)
names(SYV.ba)<-c('acsa','tsca','osvi','beal', 'unknown')

syv.ba.norm<-SYV.ba/sum(syv.forest.rad$BA)

#flux per basal area
syv.acsanorm<-syv.sm.pct/syv.ba.norm[1]
syv.tscanorm<-syv.hl.pct/syv.ba.norm[2]
syv.osvinorm<-syv.hb.pct/syv.ba.norm[3]
syv.bealnorm<-syv.yb.pct/syv.ba.norm[4]
#syv.uknorm<-syv.uk.pct/syv.ba.norm[5]

syv.perba<-cbind(syv.acsanorm,syv.tscanorm,syv.osvinorm,syv.bealnorm)


plot(syv.perba[,1], col='white', type='l', ylim=c(0,5.0), xlim=c(min(gs),max(gs)), main='SYV sapflow per basal area', 
     xlab='DOY', ylab=" %sapflow / % BA", font=2, font.lab=2, cex.lab=1.2)
cols<-c('orange','forest green','dark red','blue')
for(i in 1:4){
  lines(syv.perba[,i], col=cols[i], lwd=5)
}
abline(h=1, lty=2)
polygon(x=c(155,155,177, 177),y=c(-0.2,5,5,-0.2), col='white', border=NA)
#legend(x=155,y=4.6,legend=c("ACSA","OSVI","TSCA","BEAL"), col=c('orange','dark red','forest green','blue'), ncol=2, lwd=3, cex=0.8)


#Big trees

hist(syv.forest.rad$DBH)
hist(wcr.forest.rad$DBH)

syv.treesums<-colSums(alltree.syv.pd, na.rm=TRUE)

#Top which percent do 50%
quantile(syv.treesums,c(0.88))
sum(syv.treesums[syv.treesums>2793])
sum(syv.treesums[syv.treesums<2793])
#Top 12% do half of transpiration
quantile(syv.forest.rad$BA,0.88)
sum(syv.treesums[syv.forest.rad$BA<1337])
sum(syv.treesums[syv.forest.rad$BA>1337])
#Largest 12% do more than half transpiration

#Dominants do what percent
sum(syv.treesums[syv.forest.rad$CC=='D'])/sum(syv.treesums)
length(which(syv.forest.rad$CC=='D'))/nrow(syv.forest.rad)
#Dominants are 10% of trees but do 42% of transp


#Compare to WCR?
wcr.treesums<-colSums(alltree.wcr.pd, na.rm=TRUE)
#Top which percent do 50%
quantile(wcr.treesums,c(0.81))
sum(wcr.treesums[wcr.treesums>3738])
sum(wcr.treesums[wcr.treesums<3738])
#Top 18.5% do half of transpiration
quantile(wcr.forest.rad$BA,0.69)
sum(wcr.treesums[wcr.forest.rad$BA<635])
sum(wcr.treesums[wcr.forest.rad$BA>635])
#Largest 31% do more than half transpiration

#Dominants do what percent
sum(wcr.treesums[wcr.forest.rad$CC=='D'])/sum(wcr.treesums)
length(which(wcr.forest.rad$CC=='D'))/nrow(wcr.forest.rad)
#Dominants are 6% of trees and do 16% of transp
#Dominants twice as influential at SYV?

#####
#Doing stuff wrong
#First, right answer
plot(rowSums(alltree.syv.pd), ylim=c(0,9000))
plot(rowSums(alltree.wcr.pd), ylim=c(0,9000))

#Reminder of what these things are
HL.syv<-c(1:4,8,10,19:20)  #11 is left out because bad data quality
SM.syv<-c(9,13:18)
HB.syv<-c(5,6)
YB.syv<-c(7,12)

AB.wcr<-c(1:2,8,11)  
SM.wcr<-c(4:6,12:13)
HB.wcr<-c(3,7,14)
GA.wcr<-c(9:10)

#####
#Pooled sm fluxes
#####
SM.syv<-c(9,13:18)
SM.wcr<-c(4:6,12:13)
SM.pool<-cbind(wcr.flux.daily[,SM.wcr+1],syv.flux.daily[,SM.syv+1])

SM.tr.f.new<-rowMeans(SM.pool, na.rm=TRUE) #pooled SM fluxes

syv.lut.smpool<-data.frame(cbind(HL.tr.f,SM.tr.f.new,HB.tr.f,YB.tr.f))
colnames(syv.lut.smpool)<-c('TSCA','ACSA','OSVI','BEAL')
syv.scale(syv.lut.smpool,treemult.syv)
alltree.syv.pd.new->syv.smpool

wcr.lut.smpool<-data.frame(cbind(SM.tr.f.new,AB.tr.f,HB.tr.f,GA.tr.f))
colnames(wcr.lut.smpool)<-c('ACSA','TIAM','OSVI','FRPE')
wcr.scale(wcr.lut.smpool,treemult.wcr)
alltree.wcr.pd.new->wcr.smpool

#test
plot(rowSums(alltree.syv.pd), ylim=c(0,9000), main='original syv', xlab='DOY')
plot(rowSums(alltree.wcr.pd), ylim=c(0,9000), main='original wcr', xlab='DOY')

mean(rowSums(alltree.syv.pd)-rowSums(alltree.wcr.pd), na.rm=TRUE)

plot(rowSums(syv.smpool), ylim=c(0,9000), main='pooled acsa syv', xlab='DOY')
plot(rowSums(wcr.smpool), ylim=c(0,9000), main='pooled acsa wcr', xlab='DOY')

mean(rowSums(alltree.syv.pd.new)-rowSums(alltree.wcr.pd.new), na.rm=TRUE)

#With pooled fluxes, the relationship reverses
#originally, wcr transpires ~530L more per day. With pooled fluxes, syv transpires ~650L more per day

#####
#Pooled across all deciduous
#####
dc.pool.syv<-sort(c(SM.syv,HB.syv,YB.syv))
dc.pool.wcr<-c(1:14)
dc.pool<-cbind(wcr.flux.daily[dc.pool.wcr+1],syv.flux.daily[dc.pool.syv+1])

dc.tr.f.new<-rowMeans(dc.pool, na.rm=TRUE)

syv.lut.dcpool<-data.frame(cbind(HL.tr.f,dc.tr.f.new,dc.tr.f.new,dc.tr.f.new))
colnames(syv.lut.dcpool)<-c('TSCA','ACSA','OSVI','BEAL')
syv.scale(syv.lut.dcpool,treemult.syv)
alltree.syv.pd.new->syv.dcpool

wcr.lut.dcpool<-data.frame(cbind(dc.tr.f.new,dc.tr.f.new,dc.tr.f.new,dc.tr.f.new))
colnames(wcr.lut.dcpool)<-c('ACSA','TIAM','OSVI','FRPE')
wcr.scale(wcr.lut.dcpool,treemult.wcr)
alltree.wcr.pd.new->wcr.dcpool

#test
plot(rowSums(alltree.syv.pd), ylim=c(0,9000), main='original syv', xlab='DOY')
plot(rowSums(alltree.wcr.pd), ylim=c(0,9000), main='original wcr', xlab='DOY')

mean(rowSums(alltree.syv.pd)-rowSums(alltree.wcr.pd), na.rm=TRUE)

plot(rowSums(syv.dcpool), ylim=c(0,9000), main='pooled dc syv', xlab='DOY')
plot(rowSums(wcr.dcpool), ylim=c(0,9000), main='pooled dc wcr', xlab='DOY')

mean(rowSums(syv.dcpool)-rowSums(wcr.dcpool), na.rm=TRUE)
#May come back for more later


####also SAPWOOD AREA

# DAT<-syv.forest.rad.dum
# syv.treemult.dum<-(DAT$Multiplier*(DAT$SWA/10000))
# syv.lut.alspool<-syv.lut.dcpool
# syv.lut.alspool[,1]<-syv.lut.alspool[,2]
# syv.scale(syv.lut.alspool, syv.treemult.dum)
# alltree.syv.pd.new->syv.swpool
# 
# DAT<-wcr.forest.rad.dum
# wcr.treemult.dum<-(DAT$Multiplier*(DAT$SWA/10000))
# wcr.lut.alspool<-wcr.lut.dcpool
# wcr.lut.alspool[,1]<-wcr.lut.alspool[,2]
# wcr.scale(wcr.lut.alspool, wcr.treemult.dum)
# alltree.wcr.pd.new->wcr.swpool

######
#What about all codominants

#From above (to remind what this is)
#treemult.wcr<-(DAT$Multiplier*(DAT$SWA/10000))  #convert cm2 to m2

#WCR
treemult.wcr.nd<-treemult.wcr

#make averages for codiminant trees
wcr.acsa.c<-mean(treemult.wcr.nd[wcr.forest.rad$SPP=='acsa' & wcr.forest.rad$CC=='C'])
wcr.tiam.c<-mean(treemult.wcr.nd[wcr.forest.rad$SPP=='tiam' & wcr.forest.rad$CC=='C'])
wcr.osvi.c<-mean(treemult.wcr.nd[wcr.forest.rad$SPP=='osvi' & wcr.forest.rad$CC=='C'])
wcr.frpe.c<-mean(treemult.wcr.nd[wcr.forest.rad$SPP=='frpe' & wcr.forest.rad$CC=='C'])

#reassign
treemult.wcr.nd[wcr.forest.rad$SPP=='acsa' & wcr.forest.rad$CC=='D']<-wcr.acsa.c
treemult.wcr.nd[wcr.forest.rad$SPP=='tiam' & wcr.forest.rad$CC=='D']<-wcr.tiam.c
treemult.wcr.nd[wcr.forest.rad$SPP=='osvi' & wcr.forest.rad$CC=='D']<-wcr.osvi.c
treemult.wcr.nd[wcr.forest.rad$SPP=='frpe' & wcr.forest.rad$CC=='D']<-wcr.frpe.c

wcr.scale(wcr.lut,treemult.wcr.nd)
alltree.wcr.pd.new->wcr.nd

#SYV
treemult.syv.nd<-treemult.syv

#make averages for codiminant trees
syv.acsa.c<-mean(treemult.syv.nd[syv.forest.rad$SPP=='acsa' & syv.forest.rad$CC=='C'])
syv.beal.c<-mean(treemult.syv.nd[syv.forest.rad$SPP=='beal' & syv.forest.rad$CC=='C'])
syv.osvi.c<-mean(treemult.syv.nd[syv.forest.rad$SPP=='osvi' & syv.forest.rad$CC=='C'])
syv.tsca.c<-mean(treemult.syv.nd[syv.forest.rad$SPP=='tsca' & syv.forest.rad$CC=='C'])

#reassign
treemult.syv.nd[syv.forest.rad$SPP=='acsa' & syv.forest.rad$CC=='D']<-syv.acsa.c
treemult.syv.nd[syv.forest.rad$SPP=='beal' & syv.forest.rad$CC=='D']<-syv.beal.c
treemult.syv.nd[syv.forest.rad$SPP=='osvi' & syv.forest.rad$CC=='D']<-syv.osvi.c
treemult.syv.nd[syv.forest.rad$SPP=='tsca' & syv.forest.rad$CC=='D']<-syv.tsca.c

syv.scale(syv.lut,treemult.syv.nd)
alltree.syv.pd.new->syv.nd

#####


#Barplots of total flow

stackflow<-function(syv.flow,wcr.flow,syv.tree, wcr.tree, gs){

wcr.flow.b<-colMeans(wcr.flow[gs,], na.rm=TRUE)
syv.flow.b<-colMeans(syv.flow[gs,], na.rm=TRUE)
syv.tree.b<-syv.tree
wcr.tree.b<-wcr.tree

wcr.b<-aggregate(wcr.flow.b, by=list(wcr.tree.b$SPP), FUN=sum,na.rm=TRUE)
syv.b<-aggregate(syv.flow.b, by=list(syv.tree.b$SPP), FUN=sum, na.rm=TRUE)

sites.b<-merge(x=syv.b,y=wcr.b, by='Group.1', all=TRUE)
sites.b<-sites.b[,2:3]
sites.b[is.na(sites.b)]<-0

sites.b<-rbind(sites.b[1:4,],sites.b[6:8,],sites.b[5,])

barplot(as.matrix(sites.b), col=c('orange','blue','dark red', 'forest green','darkolivegreen3','navajowhite4','yellow', 'gray'),
        main='Total Sap Flow',names.arg=c('SYV','WCR'),ylab='Sap Flow (L day-1)', ylim=c(0,7000), 
        cex.axis=2, cex.lab=2,cex.main=2.5, cex.names=2, font.axis=2,font.lab=2,font.main=2)
}
par(mar=c(4,5,4,2))
stackflow(alltree.syv.pd,alltree.wcr.pd,syv.forest.rad,wcr.forest.rad,c(177:238))  
stackflow(syv.smpool,wcr.smpool,syv.forest.rad,wcr.forest.rad,c(177:238)) #Total reversal
stackflow(syv.dcpool,wcr.dcpool,syv.forest.rad,wcr.forest.rad,c(177:238)) #About the same
stackflow(syv.nd,wcr.nd,syv.forest.rad,wcr.forest.rad,c(177:238)) #Bigger difference
#stackflow(syv.swpool,wcr.swpool,syv.forest.rad,wcr.forest.rad,c(177:238)) #Bigger difference


#Do these in parallel?

#Basal area
wcr.basal<-aggregate(wcr.forest.rad$BA, by=list(wcr.forest.rad$SPP), FUN=sum)
syv.basal<-aggregate(syv.forest.rad$BA, by=list(syv.forest.rad$SPP), FUN=sum)

sites.ba<-merge(x=syv.basal,y=wcr.basal, by='Group.1', all=TRUE)
sites.ba<-sites.ba[,2:3]
sites.ba[is.na(sites.ba)]<-0

sites.ba<-rbind(sites.ba[1:4,],sites.ba[6:8,],sites.ba[5,])


barplot(as.matrix(sites.ba), col=c('orange','blue','dark red', 'forest green','darkolivegreen3','navajowhite4','yellow', 'gray'),
        main='Basal area',names.arg=c('SYV','WCR'),ylab='Total Basal area (cm2)', ylim=c(0,200000), 
        cex.axis=2, cex.lab=2,cex.main=2.5, cex.names=2, font.axis=2,font.lab=2,font.main=2)
#Seasonality

