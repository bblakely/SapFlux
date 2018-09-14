#Figures! Yay!

source('Prepare_Data.R')
source('Refine_TowerData.R')
source('Calc_Sapflow_Full.R')

dev.off()

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

###Forest specs
par(mfrow=c(1,4));
#Legend
par(mar=c(0,0,0,0))
plot(c(1:10),c(1:10), col='white', bty='n', xaxt='n', yaxt='n', ylab='', xlab='')
legend(2,6,legend=c('Sugar Maple','Eastern Hemlock','Yellow Birch','Basswood','Hophornbeam','Other'),
       fill=c('orange','forest green','blue','yellow','dark red','gray'), text.font=1.8, x.intersp=0.5,y.intersp=0.8, bty='n')

syv.forest.rad<-read.csv('SYV_FOREST_RAD.csv')
wcr.forest.rad<-read.csv('WCR_FOREST_RAD.csv')

####Process basal area####
wcr.basal<-aggregate(wcr.forest.rad$BA, by=list(wcr.forest.rad$SPP), FUN=sum)
syv.basal<-aggregate(syv.forest.rad$BA, by=list(syv.forest.rad$SPP), FUN=sum)

sites.ba<-merge(x=syv.basal,y=wcr.basal, by='Group.1', all=TRUE)
sites.ba<-sites.ba[,2:3];sites.ba[is.na(sites.ba)]<-0
sites.ba<-rbind(sites.ba[1:4,],sites.ba[6:8,],sites.ba[5,])
#####
####Processing LAI####
LAI<-read.csv('LAI_2016_2017.csv')
LAI.2016<-LAI[LAI$YEAR==2016,]
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
lai.wcr<-mean(LAI.dat$WCR[LAI.dat$DOY%in%gs]);rg.wcr<-range(LAI.dat$WCR[LAI.dat$DOY%in%gs])
lai.syv<-mean(LAI.dat$SYV[LAI.dat$DOY%in%gs]);rg.syv<-range(LAI.dat$SYV[LAI.dat$DOY%in%gs])

#####

par(mar=c(4,5.5,4,2))
#Basal area
conv.m2h<-1e-4/0.64 #1e-4 m2 per cm2, 0.64 hectares
lab.ba<-expression("Basal Area ("*m^2~ha^-1*")")
barplot(as.matrix(sites.ba*conv.m2h), col=c('orange','blue','dark red', 'forest green','darkolivegreen3','navajowhite4','yellow', 'gray'),
        names.arg=c('PF','SF'),ylab=lab.ba,cex.axis=1.5, cex.lab=2, cex.names=2, font.axis=2,font.lab=2,font.main=2)

#LAI
barplot(c(lai.syv, lai.wcr), ylab="LAI (unitless)",col=c('gray30', 'dark gray'),names.arg=c('PF','SF'),ylim=c(0,6.5),
        cex.axis=1.5, cex.lab=2, cex.names=2, font.axis=2,font.main=2)
centers<-barplot(c(lai.wcr,lai.syv), plot=FALSE)
arrows(centers[1],rg.syv[1],centers[1],rg.syv[2],length=0, lwd=2);arrows(centers[2],rg.wcr[1],centers[2],rg.wcr[2],length=0, lwd=2)

#Stem density
ylab.s<-expression("Density ("*stems~ha^-1*")")
barplot(c(nrow(syv.forest)/0.64, nrow(wcr.forest)/0.64), ylab=ylab.s, col=c('gray30', 'dark gray'),names.arg=c('PF','SF'),
        cex.axis=1.5, cex.lab=2, cex.names=2, font.axis=2,font.lab=2)

par(cex.axis=1.5, cex.lab=1.5, font.axis=2,font.lab=2, cex.main=1.5)

###Maple barplots!
par(mfrow=c(1,2))

wcr.maple<-wcr.sapday[,1+which(wcr.tree$SPP=="ACSA")]
wcr.maple$mean<-rowMeans(wcr.maple)

syv.maple<-syv.sapday[,1+which(syv.tree$SPP=="ACSA")]
syv.maple$mean<-rowMeans(syv.maple)

par(mfrow=c(1,2), mar=c(4,5,4,1))

boxplot(wcr.maple[,1:5], ylim=c(0,45), xlim=c(0,9),col=c(rep(NA,5), "orange"), main="SF", xaxt='n', ylab=expression("J"[s]~"(ACSA)"~(g~m^-2~s^-1)))
boxplot(wcr.maple[6],at=8, add=TRUE, col='orange', names="Mean", width=1.2);axis(side=1, at=8, tick=F, lab="Avg.")
box(lwd=3)

boxplot(syv.maple, ylim=c(0,45), xlim=c(0,9),col=c(rep(NA,7), "orange"), main="PF", xaxt='n');axis(side=1, at=8, tick=F, lab="Avg.")
box(lwd=3)


###Measured trees, flux
par(mfrow=c(1,2))
wcr.tree$flux<-colMeans(wcr.sapday[2:15])
syv.tree$flux<-colMeans(syv.sapday[2:21], na.rm=TRUE)

fluxord.wcr<-c(1,4,2,3)
boxplot(wcr.tree$flux~wcr.tree$SPP, at=fluxord.wcr, col=c('orange', ' yellow green','dark red','yellow'), ylim=c(0,30), ylab=expression("J"[s]~(g~m^-2~s^-1)), main='SF')

fluxord.syv<-c(3,1,2,4)
boxplot(syv.tree$flux~syv.tree$SPP, at=fluxord.syv, col=c('orange',' blue','dark red','forest green'), ylim=c(0,30), main='PF')


###Profiles

####Option 1, multipanel####
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
#####

#Option 2, all on one
par(mfrow=c(1,2))

plot(wcr.sapfig[,1]~ts.gs$HOUR, col='white', ylim=c(0,70), xlab='Hour', main='SF',ylab=expression("J"[s]~(g~m^-2~s^-1)))

for(p in 1:length(unique(wcr.tree$SPP))){
  name<-as.character(unique(wcr.tree$SPP)[p])
  for(i in which(wcr.tree$SPP==name)){
    lines((aggregate(wcr.sapfig[,i], by=list(ts.gs$HOUR), FUN='mean', na.rm=TRUE))$x, col=wcr.tree$col[wcr.tree$SPP==name],lwd=2, lty=3)
  }
  sp<-rowMeans(wcr.sapfig[,wcr.tree$SPP==name], na.rm=TRUE)
  lines(aggregate(sp, by=list(ts.gs$HOUR), FUN='mean', na.rm=TRUE)$x, col=wcr.tree$col[wcr.tree$SPP==name], lwd=4)
  
}
box(lwd=3);abline(v=13, col='dark gray',lty=2,lwd=2)

legend(0,73,legend=c('ACSA','TSCA','BEAL','TIAM','OSVI'),
       fill=c('orange','forest green','blue','yellow','dark red'), text.font=2, x.intersp=0.5,y.intersp=0.8, bty='n', cex=0.7)


plot(syv.sapfig[,1]~ts.gs$HOUR, col='white', ylim=c(0,70),ylab='', xlab='Hour', main='PF')

for(p in 1:length(unique(syv.tree$SPP))){
  name<-as.character(unique(syv.tree$SPP)[p])
  for(i in which(syv.tree$SPP==name)){
    lines((aggregate(syv.sapfig[,i], by=list(ts.gs$HOUR), FUN='mean', na.rm=TRUE))$x, col=syv.tree$col[syv.tree$SPP==name],lwd=2, lty=3)
  }
  sp<-rowMeans(syv.sapfig[,syv.tree$SPP==name], na.rm=TRUE)
  lines(aggregate(sp, by=list(ts.gs$HOUR), FUN='mean', na.rm=TRUE)$x, col=syv.tree$col[syv.tree$SPP==name], lwd=4)
  
}
box(lwd=3);abline(v=13, col='dark gray',lty=2,lwd=2)


legend(0,75, legend='Solar noon', col='dark gray', lty=2, lwd=2, cex=0.8, bty='n',text.font=2, x.intersp=0.5,y.intersp=0.8, seg.len=0.8)


####Option 3, maybe with error bars?####
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
#####


###Measured trees, flow

wcr.site<-sweep(wcr.gap, 2,((wcr.tree$SWA_calc/10000)*wcr.tree$MULT_calc), FUN='*')*1800/1000
syv.site<-sweep(syv.gap, 2,((syv.tree$SWA_calc/10000)*syv.tree$MULT_calc), FUN='*')*1800/1000

wcr.flow.gs<-wcr.site[wcr.ts$DOY%in%gs,]
wcr.flow.day<-aggregate(wcr.flow.gs,by=list(ts.gs$DOY), FUN='mean')

syv.flow.gs<-syv.site[syv.ts$DOY%in%gs,]
syv.flow.day<-aggregate(syv.flow.gs,by=list(ts.gs$DOY), FUN='mean')

wcr.flow.tr<-colMeans(wcr.flow.day)[2:15]
syv.flow.tr<-colMeans(syv.flow.day, na.rm=TRUE)[2:21]

wcr.tree$flow<-wcr.flow.tr
syv.tree$flow<-syv.flow.tr

#Currently 
#boxplot(wcr.tree$flow~wcr.tree$SPP)
#boxplot(syv.tree$flow~syv.tree$SPP)



###Plots with flow scaled

wcr.forest.day<-aggregate(wcr.mega[dayind,], by=list(wcr.master$DOY[dayind]), FUN='sum')[,2:305]
syv.forest.day<-aggregate(syv.mega[dayind,], by=list(syv.master$DOY[dayind]), FUN='sum')[,2:303]

boxplot(colMeans(wcr.forest.day[gs,], na.rm=TRUE)~toupper(wcr.forest$species), col=c('orange', 'yellow green','dark red','orange4','yellow','gray'), ylim=c(0,300), ylab=expression("Tree T. ("~L~day-1*")"), main="SF", cex.axis=0.8)
boxplot(colMeans(syv.forest.day[gs,], na.rm=TRUE)~toupper(syv.forest$species), col=c('orange','blue','dark red','forest green','gray'), ylim=c(0,300), main="PF", cex.axis=0.8)

#boxplot(colMeans(wcr.forest.day[gs,], na.rm=TRUE)~wcr.forest$species, col=c('orange', 'yellow green','dark red','orange4','yellow','gray'), ylim=c(0,100))
#boxplot(colMeans(syv.forest.day[gs,], na.rm=TRUE)~syv.forest$species, col=c('orange','blue','dark red','forest green','gray'), ylim=c(0,100))

#Decided against flow profiles; if plot level, redundant with flux.
#If stand level, waaay too many curves.


###Sap flux breakdown

par(mfrow=c(1,1))
syv.forest.rad<-read.csv('SYV_FOREST_RAD.csv')#Check these; probably not updated for TSCA allometry...
wcr.forest.rad<-read.csv('WCR_FOREST_RAD.csv')

stackflow<-function(syv.flow,wcr.flow,syv.tree, wcr.tree, gs=gs, ylim=8500, errbar=TRUE){
  
  wcr.flow.b<-colMeans(wcr.flow[gs,], na.rm=TRUE) #Mean daily transp. for each tree
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
          names.arg=c('PF','SF'),ylab=expression('T ('*L~day^-1*')'), ylim=c(0,ylim), 
          cex.axis=2, cex.lab=2,cex.main=2.5, cex.names=2, font.axis=2,font.lab=2,font.main=2)
  if (errbar==TRUE){
  centers<-barplot(as.matrix(sites.b), plot=FALSE)
  
  syv.all<-rowSums(syv.flow[gs,]); wcr.all<-rowSums(wcr.flow[gs,])
  syv.sd<-sd(syv.all, na.rm=TRUE); wcr.sd<-sd(wcr.all, na.rm=TRUE)
  
  arrows(centers[1],mean(syv.all, na.rm=TRUE)-syv.sd,centers[1],mean(syv.all, na.rm=TRUE)+syv.sd, length=0, lwd=2)
  arrows(centers[2],mean(wcr.all, na.rm=TRUE)-wcr.sd,centers[2],mean(wcr.all, na.rm=TRUE)+wcr.sd, length=0, lwd=2)
  }
  
}
par(mar=c(4,5.5,4,2))
stackflow(syv.forest.day,wcr.forest.day,syv.forest.rad,wcr.forest.rad,gs)

#Original
#source('SeasonProcess.R')
#stackflow(syv.forest.day,wcr.forest.day,syv.forest.rad,wcr.forest.rad,c(177:238))



###Hydromet profiles

plotsmooth<-function(dat1, dat2, ndays,func='mean', varset, allhr=TRUE, allplot='ALL', set.par=TRUE){
  
  if(set.par=="TRUE"){par(mfrow=c(1,2), mar=c(5,5,1,1))}
  
  for(v in 1:length(varset)){
    
    varcol1<-which(colnames(dat1)==varset[v])
    varcol2<-which(colnames(dat2)==varset[v])
    
    if(allhr==FALSE){
      hrmult<-length(unique(dat1$HOUR))/24
      ndays<-ndays*hrmult}
    
    
    sm1<-rollapply(dat1[,varcol1], 48*ndays, FUN='mean', na.rm=TRUE, fill=NA, partial=TRUE)
    sm2<-rollapply(dat2[,varcol2], 48*ndays, FUN='mean', na.rm=TRUE, fill=NA, partial=TRUE)
    
    date<-rollapply(dat1$DTIME, 48*ndays, FUN='mean', na.rm=TRUE, fill=NA, partial=TRUE)
    
    sm1[dat1$DTIME>41&dat1$DTIME<55]<-NA #NAN spiky sefction
    sm2[dat2$DTIME>41&dat2$DTIME<55]<-NA #NAN spiky sefction
    
    
    ylim=c(min(c(sm1,sm2), na.rm=TRUE), max(c(sm1,sm2), na.rm=TRUE))
    
    nicename<-sub( "_1", "",colnames(dat1)[colnames(dat1)==varset[v]])
    print(nicename)
    
  
    if(varset[v]=="TS"|varset[v]=="TA_1"){ylab<-bquote(.(nicename)~"("*paste(degree,'C')*")")}else{ylab<-bquote(.(nicename)~"("*Wm^-2*")")}
    if(varset[v]=="VPD_PI_1"){ylab<-expression("VPD"~"(hPa)")};if(varset[v]=="SWC_1"|varset[v]=="SWC_1_2_1"){ylab<-expression("Soil Water Content"~"(%)")}
    if(varset[v]=="TD"){ylab<-expression("TS - TA ("~paste(degree,'C')*")")}
    
    ##Absolute plot
    if(allplot=="ALL"|allplot=="ABS"){
      plot(sm1~date, type='l', col='blue', ylab=ylab, ylim=ylim,xlab='DOY', lwd=3, font=2, font.lab=2)
      lines(sm2~date, type='l',lwd=2)
      box(lwd=3)
      
      legend(x=min(dat1$DOY), y=quantile(ylim,0.15), legend=c('SF','PF'), col=c('blue', 'black'), lwd=2, cex=0.6, x.intersp = 0.1,y.intersp = 0.5)
    }
    
    ##Difference plot
    if(allplot=="ALL"|allplot=="DIF"){
      #Set plot margins to force zero
      ylim=c(min(sm1-sm2, na.rm=TRUE),max(sm1-sm2, na.rm=TRUE))
      if(min(sm1-sm2, na.rm=TRUE)>0){ylim=c(0, max(sm1-sm2, na.rm=TRUE))}
      if(max(sm1-sm2, na.rm=TRUE)<0){ylim=c(min(sm1-sm2, na.rm=TRUE), 0)}
      
      #Plotting
      plot((sm1-sm2)~date, type='l', ylab=expression(Delta),xlab='DOY', lwd=3, col='white', font=2,font.lab=2, ylim=ylim)
      lines((sm1-sm2)~date, type='l', lwd=3, col='dark red')
      clip(min(date), max(date),0, max(sm1-sm2,na.rm=TRUE))
      lines((sm1-sm2)~date, type='l', lwd=3, col='forest green')
      abline(h=0, lwd=4, lty=2)
      box(lwd=3)
    }
    
  }
}

plotsmooth(dat1=wcr.twr[daygs,],dat2=syv.twr[daygs,], ndays=7,varset=c("LE_1","VPD_PI_1","SWC_1"), allhr=FALSE)

###Hysteresis
par(mfrow=c(1,2))
####Process Hysteresis####
dry<-8 #vpd cutoff for dryish days
syv.maple<-syv.gap[syv.tree$SPP=='ACSA' & syv.tree$CC =='C']
syv.dryind<-which(syv.twr$VPD_PI_1>dry & syv.twr$DOY%in%gs)

wcr.maple<-wcr.gap[wcr.tree$SPP=='ACSA' & wcr.tree$CC =='C']
wcr.dryind<-which(wcr.twr$VPD_PI_1>dry & wcr.twr$DOY%in%gs)

syv.dryhour<-aggregate(rowMeans(syv.maple)[syv.dryind], by=list(syv.twr$HOUR[syv.dryind]), FUN='mean', na.rm=TRUE)
wcr.dryhour<-aggregate(rowMeans(wcr.maple)[wcr.dryind], by=list(wcr.twr$HOUR[wcr.dryind]), FUN='mean', na.rm=TRUE)

syv.dryvp<-aggregate(syv.twr$VPD_PI_1[syv.dryind], by=list(syv.twr$HOUR[syv.dryind]), FUN='mean', na.rm=TRUE)
wcr.dryvp<-aggregate(wcr.twr$VPD_PI_1[wcr.dryind], by=list(wcr.twr$HOUR[wcr.dryind]), FUN='mean', na.rm=TRUE)
#####

colcode<-rep('black', 24); colcode[7:13]<-"orange"; colcode[13:22]<-"blue"
par(mar=c(4.1,5,4,2))
daytime<-c(7:22)

#Normalized VPD and Sapflux

dayhr<-which(syv.dryhour$Group.1%in%daytime)
syv.hrn<-(syv.dryhour$x/max(syv.dryhour$x))[dayhr]
syv.vpdn<-(syv.dryvp$x/max(syv.dryvp$x))[dayhr]
plot(syv.hrn[dayhr]~syv.vpdn[dayhr], col=colcode[syv.dryhour$Group.1], ylim=c(0,1), xlim=c(0.5, 1), xlab="VPD (norm.)", ylab=expression(J[s]~(norm.)), main='PF', type='l')
arr<-seq(from=1, to=length(syv.hrn), by=1); arrows(syv.vpdn[arr],syv.hrn[arr],syv.vpdn[arr+1],syv.hrn[arr+1], length=0.07,code=2, col=colcode[dayhr], lwd=2)

legend(0.5,1, legend=c("Morning", "Afternoon"), col=c('orange','blue'), lwd=2, cex=0.8, bty='n', x.intersp = 0.2, y.intersp=0.5, seg.len=1, text.font=2)

wcr.hrn<-(wcr.dryhour$x/max(wcr.dryhour$x))
wcr.vpdn<-(wcr.dryvp$x/max(wcr.dryvp$x))
plot(wcr.hrn~wcr.vpdn, col=colcode[wcr.dryhour$Group.1], ylim=c(0,1), xlim=c(0.5, 1), xlab="VPD (norm.)", ylab='', main='SF', type='l')
arr<-seq(from=1, to=length(wcr.hrn), by=1); arrows(wcr.vpdn[arr],wcr.hrn[arr],wcr.vpdn[arr+1],wcr.hrn[arr+1], length=0.07,code=2, col=colcode[daytime], lwd=2)




###Temp profiles

#Setup temp vars
syv.temp<-rowMeans(syv.master[7:10])
wcr.temp<-Lag(rowMeans(wcr.master[7:9]),-2)

####NAN areas with implausible jumps in temperature####

wcr.twr$TA_1[wcr.twr$TA_1<(-32)]<-NA
jumps<-unique(c(which(abs(diff(wcr.twr$TA_1))>10), which(abs(diff(wcr.twr$TA_1))>10)+1))
wcr.twr$TA_1[jumps]<-NA

jump.a<-which(abs(diff(wcr.master$TA_1,1))>8) 
wcr.master$TA_1[jump.a]<-NA

jump.s<-which(abs(diff(wcr.temp))>8) 
wcr.temp[jump.s]<-NA

syv.temp[is.na(wcr.temp)]<-NA
wcr.temp[is.na(syv.temp)]<-NA
#####

#Ts - Ta; negative: surface cooler. Positive: Atm cooler
syv.td<-syv.temp-syv.master$TA_1; wcr.td<-wcr.temp-wcr.master$TA_1

wcr.master$TS<-wcr.temp; colnames(wcr.master)[3]<-'DTIME'; wcr.master$TD<-wcr.td
syv.master$TS<-syv.temp; colnames(syv.master)[3]<-'DTIME'; syv.master$TD<-syv.td

colnames(wcr.master)[colnames(wcr.master)=='H']<-'HOUR';colnames(syv.master)[colnames(syv.master)=='H']<-'HOUR'

plotsmooth(dat1=wcr.master[daygs,],dat2=syv.master[daygs,], ndays=7,varset=c("TA_1","TS","TD"), allhr=FALSE)
