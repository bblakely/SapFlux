
#Sap data

syv.wpf<-read.csv('SYV_SAP_WaterPotential_2018.csv')
wcr.wpf<-read.csv('WCR_SAP_WaterPotential_2018.csv')
colnames(wcr.wpf)[7:20]<-colnames(syv.wpf)[7:20]


par(mfrow=c(2,2))
for(i in 1:(ncol(syv.wpf)-7)){
  plot(syv.wpf[,i+7]~syv.wpf$DecDOY, type='l')
  abline(v=unique(syv.wpf$DOY));abline(v=which(wcr.wpf$HOUR==3), lty=2)
}

for(i in 1:(ncol(wcr.wpf)-7)){
  plot(wcr.wpf[,i+7]~wcr.wpf$DecDOY, type='l')
  abline(v=unique(wcr.wpf$DOY)); abline(v=which(wcr.wpf$HOUR==3), lty=2)
}




syv.wpf.dat<-syv.wpf[,7:26];syv.wpf.ts<-syv.wpf[,1:6]
wcr.wpf.dat<-wcr.wpf[,7:20];wcr.wpf.ts<-wcr.wpf[,1:6]

rm('wcr.wpf','syv.wpf')

dat<-syv.wpf.dat;ts<-syv.wpf.ts

calc_wpflow<-function(dat, ts){
  daymax<-(aggregate(dat, by=list(ts$DOY), FUN='max'))
  maxes<-matrix(dat=0,nrow=nrow(dat), ncol=ncol(dat))
  for(i in 1:nrow(daymax)){
    for (f in which(ts$DOY==daymax$Group.1[i])){
    maxes[f,]<- as.numeric(daymax[i,2:ncol(daymax)])
    }
  }
    v<-(maxes-dat)/maxes
    sapflux<-119e-6*(v^1.231)
    sapflux.g<-sapflux*10000*300 #Converts to g/m2*5min; can add to get totals
    return(sapflux.g)
}

syv.sap<-calc_wpflow(dat=syv.wpf.dat,ts=syv.wpf.ts)
wcr.sap<-calc_wpflow(dat=wcr.wpf.dat,ts=wcr.wpf.ts)


par(mfrow=c(2,2))
for(i in 1:(ncol(syv.sap)-0)){
  plot(syv.sap[,i+0]~syv.wpf.ts$DecDOY, type='l', main=i)
  abline(v=unique(syv.sap$DOY));abline(v=syv.wpf.ts$DecDOY[which(syv.wpf.ts$H==14|syv.wpf.ts$H==16)], col='blue');abline(v=syv.wpf.ts$DecDOY[which(syv.wpf.ts$H==4)], col='orange')
}

for(i in 1:(ncol(wcr.sap)-0)){
  plot(wcr.sap[,i+0]~wcr.wpf.ts$DecDOY, type='l', main=i)
  abline(v=unique(wcr.wpf.ts$DOY)); abline(v=wcr.wpf.ts$DecDOY[which(wcr.wpf.ts$H==14|wcr.wpf.ts$H==16)], col='blue')
}

syv.goodsens<-c(1,2,5,7,10,13:20); syv.badsens<-c(3:4,6,8:9,11:12) #3, 4 are borderline
wcr.goodsens<-c(1,3:6,9:14); wcr.badsens<-c(2,7:8) # 7,8 are borderline


#Water potential data

wp.dat<-read.csv("Water Potential.csv")

wp.wcr<-wp.dat[wp.dat$SITE=="WCR",]
wp.syv<-wp.dat[wp.dat$SITE=="SYV",]

####Plot for WP####
wp.wcr$SPP<-as.character(wp.wcr$SPP)
col.wcr<-rep('black',9); col.wcr[wp.wcr$SPP=='acsa']<-'orange';col.wcr[wp.wcr$SPP=="osvi"]<-'dark red'; col.wcr[wp.wcr$SPP=="tiam"]<-'yellow'
dat.wcr<-t(wp.wcr[,9:13])
boxplot(dat.wcr, col=col.wcr)


wp.syv$SPP<-as.character(wp.syv$SPP)
col.syv<-rep('black',9); col.syv[wp.syv$SPP=='acsa']<-'orange';col.syv[wp.syv$SPP=="osvi"]<-'dark red'; col.syv[wp.syv$SPP=="beal"]<-'blue'; col.syv[wp.syv$SPP=="tsca"]<-'forest green'
dat.syv<-t(wp.syv[,9:13])
boxplot(dat.syv, col=col.syv)


m.wcr<-dat.wcr[,wp.wcr$SPP=='acsa']; m.syv<-dat.syv[,wp.syv$SPP=='acsa']
par(mfrow=c(1,2))
boxplot(m.wcr, ylim=c(0,1.2), col='orange');boxplot(m.syv, ylim=c(0,1.2), col='orange')
#####

colMeans(m.wcr, na.rm=TRUE);colMeans(m.syv, na.rm=TRUE)

mean(colMeans(m.wcr, na.rm=TRUE));mean(colMeans(m.syv, na.rm=TRUE))

#Conductance will be WP/sapflow...

#####Gotta match WP to real trees#####

source('Prepare_Treedata.R')

idmatch<-match(wcr.tree$ID, wp.wcr$ID)
smatch<-match(wcr.tree$Sens, gsub("S","",wp.wcr$ID))
idmatch[is.na(idmatch)]<-smatch[is.na(idmatch)]
wcr.wp.ord<-wp.wcr[idmatch,]

wcr.combo<-cbind(wcr.tree, wcr.wp.ord[,4:ncol(wcr.wp.ord)])


wp.syv$Alt.ID2<-rep(NA, nrow(wp.syv))
wp.syv$Alt.ID2[c(3,10,11)]<-c(24,602,584)

wp.syv$Alt.ID[which(wp.syv$Alt.ID==255)]<-225

idmatch.syv<-match(syv.tree$ID, wp.syv$ID)
altmatch.syv<-match(syv.tree$ID, wp.syv$Alt.ID)
alt2match.syv<-match(syv.tree$ID, wp.syv$Alt.ID2)

idmatch.syv[is.na(idmatch.syv)]<-altmatch.syv[is.na(idmatch.syv)]
idmatch.syv[is.na(idmatch.syv)]<-alt2match.syv[is.na(idmatch.syv)]



#Some problems:
#2726 is a maple, but alt-id matches to a TSCA in SYV.tree
#280 is an orphan; it matches to nothing and has no alt-id

#Randomly assign those 2 trees to the remaining 2 slots
#2754 and 282 are missing water-potential matches

idmatch.syv[16]<-3; idmatch.syv[14]<-6;idmatch.syv[4]<-NA  #16<-6 and 14<-3 are random choices. Can Switch. 

syv.wp.ord<-wp.syv[idmatch.syv,]
syv.combo<-cbind(syv.tree, syv.wp.ord[,4:ncol(syv.wp.ord)]) #Still two mismatched/unmatched trees


#####

#Calculate means

syv.combo$wpmean<-rowMeans(syv.combo[,20:25], na.rm=TRUE)
wcr.combo$wpmean<-rowMeans(wcr.combo[,20:25], na.rm=TRUE)


syv.pmtime<-which(syv.wpf.ts$DOY==246 & syv.wpf.ts$H%in%c(2:4));wcr.pmtime<-which(wcr.wpf.ts$DOY==244 & wcr.wpf.ts$H%in%c(2:4))
syv.sapr<-colMeans(syv.sap[syv.pmtime,])
wcr.sapr<-colMeans(wcr.sap[wcr.pmtime,])


syv.combo$sap<-syv.sapr;wcr.combo$sap<-wcr.sapr
syv.combo$sap[syv.badsens]<-NA; wcr.combo$sap[wcr.badsens]<-NA


syv.cond<-syv.combo$sap/syv.combo$wpmean
wcr.cond<-wcr.combo$sap/wcr.combo$wpmean



syv.cond.map<-syv.cond[syv.combo$SPP=='ACSA']
wcr.cond.map<-wcr.cond[wcr.combo$SPP=='ACSA']


mean(syv.cond.map, na.rm=TRUE);mean(wcr.cond.map)

