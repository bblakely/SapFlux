#Figures! Yay!

source('Prepare_Data.R')
source('Refine_TowerData.R')
source('Calc_Sapflow_Full.R')

dev.off()

#Subset to GS

wcr.sapfig<-wcr.gap[wcr.ts$DOY%in%gs,]
syv.sapfig<-syv.gap[syv.ts$DOY%in%gs,]

ts.gs<-wcr.ts[wcr.ts$DOY%in%gs,]

#To pair w/o editing gap
wcr.sapfig[is.na(rowMeans(syv.sapfig, na.rm=TRUE)),]<-NA


#Create daily average flux rates
wcr.sapday<-aggregate(wcr.gap[wcr.ts$DOY%in%gs,], by=list(wcr.ts$DOY[wcr.ts$DOY%in%gs]), FUN='mean')
syv.sapday<-aggregate(syv.gap[syv.ts$DOY%in%gs,], by=list(syv.ts$DOY[syv.ts$DOY%in%gs]), FUN='mean')

#To pair without pairing gap
wcr.sapday[is.na(rowMeans(syv.sapday[,2:21])),2:15]<-NA

#Boxplots each tree. Informative, but crowded
#boxplot((wcr.sapday[2:15])[order(wcr.tree$SPP)], col=wcr.tree$col[order(wcr.tree$SPP)])
#boxplot((syv.sapday[2:21])[order(syv.tree$SPP)], col=syv.tree$col[order(syv.tree$SPP)])

###Forest specs####
par(mfrow=c(1,4));
#Legend
par(mar=c(0,0,0,0))
plot(c(1:10),c(1:10), col='white', bty='n', xaxt='n', yaxt='n', ylab='', xlab='')
legend(2,6,legend=c('Sugar Maple','E.Hemlock','Yellow Birch','Basswood','Hophornbeam','Other'),
       fill=c('orange','forest green','blue','yellow','dark red','gray'), text.font=1.8, x.intersp=0.5,y.intersp=0.8, bty='n', cex=1.3)

syv.forest.rad<-read.csv('SYV_FOREST_RAD.csv')
wcr.forest.rad<-read.csv('WCR_FOREST_RAD.csv')

syv.forest.rad$col<-gray; syv.forest.rad$col[syv.forest.rad$SPP==ACSA]<-"Orange"; syv.forest.rad

####Process basal area
wcr.basal<-aggregate(wcr.forest.rad$BA, by=list(wcr.forest.rad$SPP), FUN=sum)
syv.basal<-aggregate(syv.forest.rad$BA, by=list(syv.forest.rad$SPP), FUN=sum)
#fuckin colors
wcr.basal$col<-"gray"; 
wcr.basal$col[wcr.basal$Group.1=="acsa"]<-"orange"; wcr.basal$col[wcr.basal$Group.1=="osvi"]<-"dark red";
wcr.basal$col[wcr.basal$Group.1=="frpe"]<-"darkolivegreen3";wcr.basal$col[wcr.basal$Group.1=="tiam"]<-"yellow";
wcr.basal$col[wcr.basal$Group.1=="quru"]<-"orange4"; wcr.basal$col[wcr.basal$Group.1=="tsca"]<-"forest green";
wcr.basal$col[wcr.basal$Group.1=="beal"]<-"blue"

syv.basal$col<-"gray"; 
syv.basal$col[syv.basal$Group.1=="acsa"]<-"orange"; syv.basal$col[syv.basal$Group.1=="osvi"]<-"dark red";
syv.basal$col[syv.basal$Group.1=="frpe"]<-"darkolivegreen3";syv.basal$col[syv.basal$Group.1=="tiam"]<-"yellow";
syv.basal$col[syv.basal$Group.1=="quru"]<-"orange4"; syv.basal$col[syv.basal$Group.1=="tsca"]<-"forest green";
syv.basal$col[syv.basal$Group.1=="beal"]<-"blue"

sites.ba<-merge(x=wcr.basal,y=syv.basal, by='Group.1', all=TRUE)

col.ba<-sites.ba[,c(3,5)]; col.ba$col.y[is.na(col.ba$col.y)]<-col.ba$col.x[is.na(col.ba$col.y)]; col.ba$col.x[is.na(col.ba$col.x)]<-col.ba$col.y[is.na(col.ba$col.x)]
sp.ba<-sites.ba[,c(2,4)];sp.ba[is.na(sp.ba)]<-0
sites.ba<-cbind(sp.ba, col.ba$col.x); colnames(sites.ba)[3]<-"col"

sites.ba<-rbind(sites.ba[1:4,],sites.ba[6:8,],sites.ba[5,])#reorder species

####Processing LAI
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



par(mar=c(4,5.5,4,2))
#Basal area
conv.m2h<-1e-4/0.64 #1e-4 m2 per cm2, 0.64 hectares
lab.ba<-expression("Basal Area ("*m^2~ha^-1*")")
barplot(as.matrix(sites.ba[1:2]*conv.m2h), col=as.character(sites.ba$col),
        names.arg=c('MF','OF'),ylab=lab.ba,cex.axis=1.5, cex.lab=2, cex.names=2, font.axis=2,font.lab=2,font.main=2)

#LAI
barplot(c(lai.wcr, lai.syv), ylab="LAI (unitless)",col=c('dark gray', 'gray30'),names.arg=c('MF','OF'),ylim=c(0,6.5),
        cex.axis=1.5, cex.lab=2, cex.names=2, font.axis=2,font.main=2)
centers<-barplot(c(lai.wcr,lai.syv), plot=FALSE)
arrows(centers[2],rg.syv[2],centers[2],rg.syv[1],length=0, lwd=2);arrows(centers[1],rg.wcr[1],centers[1],rg.wcr[2],length=0, lwd=2)

#Stem density
ylab.s<-expression("Density ("*stems~ha^-1*")")
barplot(c(nrow(wcr.forest)/0.64, nrow(syv.forest)/0.64), ylab=ylab.s, col=c('dark gray','gray30'),names.arg=c('MF','OF'),
        cex.axis=1.5, cex.lab=2, cex.names=2, font.axis=2,font.lab=2)

par(cex.axis=1.5, cex.lab=1.5, font.axis=2,font.lab=2, cex.main=1.5)

dev.copy(png, filename="Figures/Ch4_Forest.png", width=780, height=440); dev.off()
#####


###Maple barplots####
par(mfrow=c(1,2))

wcr.maple<-wcr.sapday[,1+which(wcr.tree$SPP=="ACSA")]
wcr.maple$mean<-rowMeans(wcr.maple)

syv.maple<-syv.sapday[,1+which(syv.tree$SPP=="ACSA")]
syv.maple$mean<-rowMeans(syv.maple)

par(mfrow=c(1,2), mar=c(4,5,4,1))

boxplot(wcr.maple[,1:5], ylim=c(0,45), xlim=c(0,9),col=c(rep(NA,5), "orange"), main="MF", xaxt='n', ylab=expression("V"[s]~"(ACSA)"~(g~m^-2~s^-1)))
boxplot(wcr.maple[6],at=8, add=TRUE, col='orange', names="Mean", width=1.2);axis(side=1, at=8, tick=F, lab="Avg.")
box(lwd=3)

boxplot(syv.maple, ylim=c(0,45), xlim=c(0,9),col=c(rep(NA,7), "orange"), main="OF", xaxt='n');axis(side=1, at=8, tick=F, lab="Avg.")
box(lwd=3)



dev.copy(png, filename="Figures/Ch4_MapleFlux.png", width=560, height=380); dev.off()
#####

###Species flux and flow####

#Option 2, all on one
par(mfrow=c(1,2), mar=c(4,5,3,1))

plot(wcr.sapfig[,1]~ts.gs$HOUR, col='white', ylim=c(0,70), xlab='Hour', main='MF',ylab=expression("V"[s]~(g~m^-2~s^-1)), cex.lab=1.5)

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
       fill=c('orange','forest green','blue','yellow','dark red'), text.font=2, x.intersp=0.5,y.intersp=0.8, bty='n')


plot(syv.sapfig[,1]~ts.gs$HOUR, col='white', ylim=c(0,70),ylab='', xlab='Hour', main='OF', cex.lab=1.5)

for(p in 1:length(unique(syv.tree$SPP))){
  name<-as.character(unique(syv.tree$SPP)[p])
  for(i in which(syv.tree$SPP==name)){
    lines((aggregate(syv.sapfig[,i], by=list(ts.gs$HOUR), FUN='mean', na.rm=TRUE))$x, col=syv.tree$col[syv.tree$SPP==name],lwd=2, lty=3)
  }
  sp<-rowMeans(syv.sapfig[,syv.tree$SPP==name], na.rm=TRUE)
  lines(aggregate(sp, by=list(ts.gs$HOUR), FUN='mean', na.rm=TRUE)$x, col=syv.tree$col[syv.tree$SPP==name], lwd=4)
  
}
box(lwd=3);abline(v=13, col='dark gray',lty=2,lwd=2)


#legend(0,60, legend='Solar noon', col='dark gray', lty=2, lwd=2, bty='n',text.font=2, x.intersp=0.5,y.intersp=0.8, seg.len=0.8)

dev.copy(png, filename="Figures/Ch4_fluxcurve.png", width=780, height=425); dev.off()

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

boxplot(colMeans(wcr.forest.day[gs,], na.rm=TRUE)~toupper(wcr.forest$species), col=c('orange', 'yellow green','dark red','orange4','yellow','gray'), ylim=c(0,100), ylab=expression("Tree T. ("~L~day-1*")"))
boxplot(colMeans(syv.forest.day[gs,], na.rm=TRUE)~toupper(syv.forest$species), col=c('orange','blue','dark red','forest green','gray'), ylim=c(0,100))

#dev.copy(png, filename="Figures/Ch4_Species.png", width=800, height=720); dev.off()

# a break for maple stats
wcr.maps<-unname(colMeans(wcr.forest.day[wcr.forest$species=="acsa"], na.rm=TRUE))
syv.maps<-unname(colMeans(syv.forest.day[syv.forest$species=="acsa"], na.rm=TRUE))
mean(wcr.maps)/mean(syv.maps); t.test(wcr.maps, syv.maps)

#for panels
par(mfrow=c(1,2), mar=c(2,3,2,1))
boxplot(colMeans(wcr.forest.day[gs,], na.rm=TRUE)~toupper(wcr.forest$species), col=c('orange', 'yellow green','dark red','orange4','yellow','gray'), ylim=c(0,300), main="MF", ylab=expression("Tree T. ("~L~day-1*")"))
boxplot(colMeans(syv.forest.day[gs,], na.rm=TRUE)~toupper(syv.forest$species), col=c('orange','blue','dark red','forest green','gray'), ylim=c(0,300), main="OF")
dev.copy(png, filename="Figures/Ch4_SpeciesPanels.png", width=1200, height=600); dev.off()

#####


###Scaling ####
source("SeasonProcess.R")


#Scaled T

par(mfrow=c(2,2), mar=c(4,4.5,2,1))
gs<-c(140:260)

plot(wcr.sm.tot[gs], col='white', ylim=c(0,8000), xlim=c(min(gs),max(gs)),main='MF', ylab=expression("Transpiration"~(L~day^-1)), xlab="", font=2, font.lab=2, cex.lab=1.2)
polygon(y=c(wcr.sm.tot[gs]+wcr.ab.tot[gs]+wcr.ga.tot[gs]+wcr.hb.tot[gs]+wcr.uk.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='gray')
polygon(y=c(wcr.sm.tot[gs]+wcr.ab.tot[gs]+wcr.ga.tot[gs]+wcr.hb.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='dark red')
polygon(y=c(wcr.sm.tot[gs]+wcr.ab.tot[gs]+wcr.ga.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='yellow green')
polygon(y=c(wcr.sm.tot[gs]+wcr.ab.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='yellow')
polygon(y=c(wcr.sm.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)), col='orange')
#mask out weird smoothing
polygon(y=c(0,8000,8000,0), x=c(247,247,252,252), col='white',border=NA)
polygon(y=c(0,8000,8000,0), x=c(39,39,46,46), col='white',border=NA)
polygon(y=c(0,8000,8000,0), x=c(330,330,365,365), col='white',border=NA)

polygon(x=c(156,156,177,177),y=c(0,8000,8000,0), col='white', border=NA)
polygon(x=c(75,75,110,110),y=c(0,0800,8000,0), col='white', border=NA)


abline(v=c(150, 250), lty=3)

plot(syv.sm.tot[gs], col='white', ylim=c(0,8000), xlim=c(min(gs),max(gs)), main='OF', ylab="",xlab="", font=2, font.lab=2)
polygon(y=c(syv.sm.tot[gs]+syv.hl.tot[gs]+syv.yb.tot[gs]+syv.hb.tot[gs]+syv.uk.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='gray')
polygon(y=c(syv.sm.tot[gs]+syv.hl.tot[gs]+syv.yb.tot[gs]+syv.hb.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='dark red')
polygon(y=c(syv.sm.tot[gs]+syv.hl.tot[gs]+syv.yb.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='blue')
polygon(y=c(syv.sm.tot[gs]+syv.hl.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='forest green')
polygon(y=c(syv.sm.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)), col='orange')
#mask out weird smoothing
polygon(x=c(156,156,177,177),y=c(0,5000,5000,0), col='white', border=NA)
polygon(x=c(75,75,110,110),y=c(0,5000,5000,0), col='white', border=NA)

polygon(y=c(0,8000,8000,0), x=c(247,247,252,252), col='white',border=NA)
polygon(y=c(0,8000,8000,0), x=c(39,39,46,46), col='white',border=NA)
polygon(y=c(0,8000,8000,0), x=c(330,330,365,365), col='white',border=NA)


abline(v=c(150, 250), lty=3)


plot(wcr.sm.pct[gs], col='white', ylim=c(0,1), xlim=c(min(gs), max(gs)),  ylab="%", xlab="DOY", font=2, cex.lab=1.2)
polygon(y=c(0,1,1,0),x=c(min(gs),min(gs),max(gs),max(gs)), col='gray')
#polygon(y=c(wcr.sm.pct[gs]+wcr.ab.pct[gs]+wcr.ga.pct[gs]+wcr.hb.pct[gs]+wcr.uk.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)),col='gray')
polygon(y=c(wcr.sm.pct[gs]+wcr.ab.pct[gs]+wcr.ga.pct[gs]+wcr.hb.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)),col='dark red')
polygon(y=c(wcr.sm.pct[gs]+wcr.ab.pct[gs]+wcr.ga.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)),col='yellow green')
polygon(y=c(wcr.sm.pct[gs]+wcr.ab.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)),col='yellow')
polygon(y=c(wcr.sm.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)), col='orange')

polygon(y=c(0,1.01,1.01,0), x=c(247,247,252,252), col='white',border=NA)
polygon(y=c(0,1.01,1.01,0), x=c(39,39,46,46), col='white',border=NA)
polygon(y=c(0,1.01,1.01,0), x=c(330,330,365,365), col='white',border=NA)

#polygon(x=c(155,155,175,175),y=c(0,1.01,1.01,0), col='white', border=NA)
#polygon(x=c(75,75,110,110),y=c(0,1.01,1.01,0), col='white', border=NA)
abline(v=c(150, 250), lty=3)


abline(v=c(150, 250), lty=3)

    
plot(syv.sm.pct[gs], col='white', ylim=c(0,1), xlim=c(min(gs), max(gs)), xlab="DOY",ylab="", font=2,font.lab=2, cex.lab=1.2)
polygon(y=c(0,1,1,0),x=c(min(gs),min(gs),max(gs),max(gs)), col='gray')
polygon(y=c(syv.sm.pct[gs]+syv.hl.pct[gs]+syv.yb.pct[gs]+syv.hb.pct[gs]+syv.uk.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)),col='gray')
polygon(y=c(syv.sm.pct[gs]+syv.hl.pct[gs]+syv.yb.pct[gs]+syv.hb.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)),col='dark red')
polygon(y=c(syv.sm.pct[gs]+syv.hl.pct[gs]+syv.yb.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)),col='blue')
polygon(y=c(syv.sm.pct[gs]+syv.hl.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)),col='forest green')
polygon(y=c(syv.sm.pct[gs], rep(0,length(gs))),x=c(gs,rev(gs)), col='orange')

polygon(x=c(155,155,175,175),y=c(0,1.01,1.01,0), col='white', border=NA)
polygon(x=c(75,75,110,110),y=c(0,1.01,1.01,0), col='white', border=NA)

polygon(y=c(0,1.01,1.01,0), x=c(247,247,252,252), col='white',border=NA)
polygon(y=c(0,1.01,1.01,0), x=c(39,39,46,46), col='white',border=NA)
polygon(y=c(0,1.01,1.01,0), x=c(330,330,365,365), col='white',border=NA)

abline(v=c(150, 250), lty=3)

dev.copy(png, filename="Figures/Ch4_ScaledSeas.png", width=550, height=500); dev.off()

par(mfrow=c(1,2), mar=c(1,5,1,1))
plot(colMeans(syv.perba[gs,]), col=cols.syv, pch=18, cex=3, ylim=c(0,2),xlim=c(0.5,4.5), xlab='', ylab=" % T / % BA", xaxt='n', cex.lab=2.5); abline(h=1, lty=3)
plot(colMeans(wcr.perba[gs,]), col=cols.wcr, pch=18, cex=3, ylim=c(0,2),xlim=c(0.5,4.5), xlab='', ylab=" % T / % BA", xaxt='n', cex.lab=2.5); abline(h=1, lty=3)

dev.copy(png, filename="Figures/Ch4_TbyBA.png", width=600, height=230); dev.off()





stackflow<-function(syv.flow,wcr.flow,syv.tree, wcr.tree, gs, title="Total Sap Flow"){
  
  wcr.flow.b<-colMeans(wcr.flow[gs,], na.rm=TRUE)
  syv.flow.b<-colMeans(syv.flow[gs,], na.rm=TRUE)
  syv.tree.b<-syv.tree
  wcr.tree.b<-wcr.tree
  
  
  wcr.b<-aggregate(wcr.flow.b, by=list(wcr.tree.b$SPP), FUN=sum,na.rm=TRUE)
  syv.b<-aggregate(syv.flow.b, by=list(syv.tree.b$SPP), FUN=sum, na.rm=TRUE)

  #fuckin colors
  wcr.b$col<-"gray"; 
  wcr.b$col[wcr.b$Group.1=="acsa"]<-"orange"; wcr.b$col[wcr.b$Group.1=="osvi"]<-"dark red";
  wcr.b$col[wcr.b$Group.1=="frpe"]<-"darkolivegreen3";wcr.b$col[wcr.b$Group.1=="tiam"]<-"yellow";
  wcr.b$col[wcr.b$Group.1=="quru"]<-"orange4"; wcr.b$col[wcr.b$Group.1=="tsca"]<-"forest green";
  wcr.b$col[wcr.b$Group.1=="beal"]<-"blue"
  
  syv.b$col<-"gray"; 
  syv.b$col[syv.b$Group.1=="acsa"]<-"orange"; syv.b$col[syv.b$Group.1=="osvi"]<-"dark red";
  syv.b$col[syv.b$Group.1=="frpe"]<-"darkolivegreen3";syv.b$col[syv.b$Group.1=="tiam"]<-"yellow";
  syv.b$col[syv.b$Group.1=="quru"]<-"orange4"; syv.b$col[syv.b$Group.1=="tsca"]<-"forest green";
  syv.b$col[syv.b$Group.1=="beal"]<-"blue"
  
  

  
  sites.b<-merge(x=wcr.b,y=syv.b, by='Group.1', all=TRUE)
  
  col.b<-sites.b[,c(3,5)]; col.b$col.y[is.na(col.b$col.y)]<-col.b$col.x[is.na(col.b$col.y)]; col.b$col.x[is.na(col.b$col.x)]<-col.b$col.y[is.na(col.b$col.x)]
  print(col.b)
  sp.b<-sites.b[,c(2,4)];sp.b[is.na(sp.b)]<-0
  sites.b<-cbind(sp.b, col.b$col.x); colnames(sites.b)[3]<-"col"
  
  print(sites.b)
  sites.b<-rbind(sites.b[1:5,],sites.b[7:8,],sites.b[6,])
  
  print(sites.b)
  
  barplot(as.matrix(sites.b[,1:2]), col=as.character(sites.b$col),
          main=title,names.arg=c('MF','OF'),ylab=expression(Transpiration~(L~day^-1)), ylim=c(0,7000), 
          cex.axis=2, cex.lab=2,cex.main=2.5, cex.names=2, font.axis=2,font.lab=2,font.main=2)
}
par(mar=c(4,5,4,2))
par(mfrow=c(1,3))
stackflow(alltree.syv.pd,alltree.wcr.pd,syv.forest.rad,wcr.forest.rad,c(150:250), title="Original")  
stackflow(syv.smpool,wcr.smpool,syv.forest.rad,wcr.forest.rad,c(150:250), title="Pooled by Species") #Total reversal
stackflow(syv.dcpool,wcr.dcpool,syv.forest.rad,wcr.forest.rad,c(150:250), title="Pooled by PFT") #About the same

dev.copy(png, filename="Figures/Ch4_Scaling.png", width=810, height=400); dev.off()

#overall...
treeday.wcr<-aggregate(wcr.flow.pd[,5:ncol(wcr.flow.pd)], by=list(wcr.flow.pd$DOY), FUN=sum)/1000 #Now in L/day
gs<-c(150:250)

par(mfrow=c(1,2), mar=c(4,4.5,4,1)); ylab=expression(bold("Transpiration"~(L~day^-1)))
plot(colMeans(treeday.wcr[gs,2:ncol(treeday.wcr)], na.rm=TRUE)~wcr.tree$DBH, col=col.class.wcr, pch=pch.class.wcr, ylim=c(0,150), xlim=c(0,90), cex=2, xlab='DBH', ylab=ylab, 
     font=2,font.lab=2, cex.lab=1.2, cex.main=1.5)

# legend(-3,1000,legend=c('Sugar Maple','Basswood','Green Ash','Hophornbeam'), 
#        fill=c('orange','yellow','yellow green','dark red'), cex=0.9,
#         x.intersp=0.8,y.intersp=0.6, bty='n', text.font=2)
#abline(a=-20,b=5)

treeday.syv<-aggregate(syv.flow.pd[,5:ncol(syv.flow.pd)], by=list(syv.flow.pd$DOY), FUN=sum)/1000 #Now in L/day
gs<-c(150:250)

plot(colMeans(treeday.syv[gs,2:ncol(treeday.syv)], na.rm=TRUE)~syv.tree$DBH, col=col.class.syv, pch=pch.class.syv, ylim=c(0,150), xlim=c(0,90), cex=2, xlab='DBH', ylab="",
      font=2,font.lab=2, cex.lab=1.2, cex.main=1.5)



dev.copy(png, filename="Figures/Ch4_DBHvTransp.png", width=700, height=350); dev.off()


#####

###Hydromet profiles####
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
      
      legend(x=min(dat1$DOY), y=quantile(ylim,0.15), legend=c('MF','OF'), col=c('blue', 'black'), lwd=2, cex=0.6, x.intersp = 0.1,y.intersp = 0.5)
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

#plotsmooth(dat1=wcr.twr[daygs,],dat2=syv.twr[daygs,], ndays=7,varset=c("LE_1","VPD_PI_1","SWC_1"), allhr=FALSE)

par(mfrow=c(3,2), mar=c(4,5,1,1))
plotsmooth(dat1=wcr.twr[daygs,],dat2=syv.twr[daygs,], ndays=7,varset=c("NETRAD_1","VPD_PI_1","SWC_1"), allhr=FALSE, set.par=FALSE)

dev.copy(png, filename="Figures/Ch4_Drivers.png", width=520, height=600); dev.off()

#####

###Hysteresis
par(mfrow=c(2,1));gs<-c(150:250)
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
wcr.dryvp$x[is.infinite(wcr.dryvp$x)]<-NA; wcr.dryvp$x<-na.spline(wcr.dryvp$x)
#####



colcode<-rep('black', 24); colcode[6:14]<-"orange"; colcode[14:23]<-"blue"
par(mar=c(4.1,5,4,2))
daytime<-c(7:23)

#Normalized VPD and Sapflux

#dayhr<-which(wcr.dryhour$Group.1%in%daytime)
wcr.hrn<-(wcr.dryhour$x/max(wcr.dryhour$x))#[dayhr]
wcr.vpdn<-(wcr.dryvp$x/max(wcr.dryvp$x))#[dayhr]
plot(wcr.hrn~wcr.vpdn, col=colcode[wcr.dryhour$Group.1], ylim=c(0,1), xlim=c(0.5, 1), xlab="", ylab=expression(V[s]~(norm.)), main='MF', type='l')
arr<-seq(from=1, to=length(wcr.hrn), by=1); arrows(wcr.vpdn[arr],wcr.hrn[arr],wcr.vpdn[arr+1],wcr.hrn[arr+1], length=0.07,code=2, col=colcode[daytime], lwd=2)

legend(0.5,1, legend=c("Morning", "Afternoon"), col=c('orange','blue'), lwd=2, cex=0.8, bty='n', x.intersp = 0.2, y.intersp=0.5, seg.len=1, text.font=2)

dayhr<-which(syv.dryhour$Group.1%in%daytime)
syv.hrn<-(syv.dryhour$x/max(syv.dryhour$x))[dayhr]
syv.vpdn<-(syv.dryvp$x/max(syv.dryvp$x))[dayhr]
plot(syv.hrn[dayhr]~syv.vpdn[dayhr], col=colcode[syv.dryhour$Group.1], ylim=c(0,1), xlim=c(0.5, 1), xlab="VPD (norm.)", ylab=expression(V[s]~(norm.)), main='OF', type='l')
arr<-seq(from=1, to=length(syv.hrn), by=1); arrows(syv.vpdn[arr],syv.hrn[arr],syv.vpdn[arr+1],syv.hrn[arr+1], length=0.07,code=2, col=colcode[dayhr], lwd=2)

dev.copy(png, filename="Figures/Ch4_Hysteresis.png", width=350, height=400); dev.off()


source("EnvironPlots.R")

dev.copy(png, filename="Figures/Ch4_Environ.png", width=620, height=350); dev.off()
