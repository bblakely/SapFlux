#Script for multi-year work

##Data for measured trees####
source("Prepare_treedata.R")
rm('syv.calc.rp','wcr.calc.rp') #These sheets show breakdown of RP calculations; not used
syv.tree$CC[c(1,5,11,14,17)]<-substr(syv.tree$CC[c(1,5,11,14,17)],1,1)

print('Tree data ready')

##Data for surveyed trees####
source("Prepare_forestdata.R")
rm('syv.forest.raw', 'wcr.forest.raw')#Raw files, not currently using

assign.color<-function(data, spcol=which(colnames(data)=="species")){
  colvec<-rep("gray", nrow(data))
  spp<-data[,spcol]
  colvec[spp=='acsa'|spp=='ACSA']<-"orange"
  colvec[spp=='tiam'|spp=='TIAM']<-"yellow"
  colvec[spp=='quru'|spp=='QURU']<-"orange4"
  colvec[spp=='frpe'|spp=='FRPE']<-"yellow green"
  colvec[spp=='osvi'|spp=='OSVI']<-"dark red"
  colvec[spp=='beal'|spp=='BEAL']<-"blue"
  colvec[spp=='tsca'|spp=='TSCA']<-"dark green"
  data$col<-colvec
  return(data)
}

syv.forest<-assign.color(syv.forest)
wcr.forest<-assign.color(wcr.forest)

syv.tree<-assign.color(syv.tree, spcol=3)
wcr.tree<-assign.color(wcr.tree, spcol=3)

calc.height<-function(data, maxheight, anglecol=which(colnames(data)=="height.angle"), dcol=which(colnames(data)=='distance')){
  if(is.factor(data[,anglecol])){
    data[,anglecol]<-as.numeric(levels(data[,anglecol]))[data[,anglecol]]
  }
  data$height<-data[,dcol]*tan(data[,anglecol]*3.14/180)+1.75
  data$height[data$height>maxheight]<-NA
  return(data)
}

syv.forest<-calc.height(syv.forest, maxheight=40)
wcr.forest<-calc.height(wcr.forest, maxheight=40)
print('Forest data ready')



##Now tower####
source('Prepare_TowerData.R')
rm("SYV_FullRaw","SYV_notfill","syv.rn","syv.twr.extra","WCR_FullRaw","WCR_notfill",
   "shared","shared.rn","syv.orphans","usevars","wcr.orphans", "wcr.twr.extra")
#rm('wcr.twr.std', 'syv.twr.std') #Three-year timeseries. Not currently using.
rm('wcr.twr.2016','wcr.twr.2015','syv.twr.2016','syv.twr.2015')

print('Tower data ready')



syv.2015<-read.csv("SYV_2015_SAPFLUX.csv")[,5:24]
wcr.2015<-read.csv("WCR_2015_SAPFLUX.csv")[,5:18]

syv.2016.k<-read.csv('SYV_2016_K.csv')
wcr.2016.k<-read.csv('WCR_2016_K.csv', header=FALSE)

syv.2017.k<-read.csv('SYV_2017_K.csv', header=FALSE);colnames(syv.2017.k)<-colnames(syv.2016.k)
wcr.2017.k<-read.csv('WCR2017_K.csv', header=FALSE);colnames(wcr.2017.k)<-colnames(wcr.2016.k)


granierconv<-function(sap.in){
  dat<-sap.in[6:ncol(sap.in)]
  ms<-syv.vs<-(119e-6)*(dat^1.231)*1000000
  return(ms)
}

syv.2016<-granierconv(syv.2016.k); wcr.2016<-granierconv(wcr.2016.k)
syv.2017<-granierconv(syv.2017.k); wcr.2017<-granierconv(wcr.2017.k)

colnames(wcr.2017)<-colnames(wcr.2016)<-colnames(wcr.2015)

rm('syv.2016.k', 'syv.2017.k','wcr.2016.k','wcr.2017.k')


#Super combined sapflux


syv.sap<-rbind(syv.2015, syv.2016, syv.2017)
wcr.sap<-rbind(wcr.2015, wcr.2016, wcr.2017)

colvec<-rep('black', nrow(syv.sap));colvec[syv.twr.std$YEAR==2015]<-'red';colvec[syv.twr.std$YEAR==2017]<-'blue'

# for(i in 1:ncol(syv.sap)){
#   plot(syv.sap[,i], pch='.', main=paste(syv.tree$SPP[i], syv.tree$Sens[i]))
# }
# 
# for(i in 1:ncol(wcr.sap)){
#   plot(wcr.sap[,i], pch='.', main=paste(wcr.tree$SPP[i], wcr.tree$Sens[i]))
# }


# for(i in 1:ncol(wcr.sap)){
#   plot(wcr.sap[,i]~wcr.twr.std$DTIME, pch='.', main=paste(wcr.tree$SPP[i], wcr.tree$Sens[i]), col=colvec)
# }
# 
# for(i in 1:ncol(syv.sap)){
#   plot(syv.sap[,i]~syv.twr.std$DTIME, pch='.', main=paste(syv.tree$SPP[i], syv.tree$Sens[i]), col=colvec)
# }


wcr.twr.std[wcr.twr.std==-9999]<-NA; syv.twr.std[syv.twr.std==-9999]<-NA

gsind<-which(wcr.twr.std$MONTH%in%c(5:9)&wcr.twr.std$HOUR%in%c(6:20))

# for(i in 1:ncol(wcr.sap)){
#   plot(wcr.sap[gsind,i]~wcr.twr.std$VPD_PI_1[gsind], pch='.', main=paste(wcr.tree$SPP[i], wcr.tree$Sens[i]), col=colvec[gsind])
# }
# 
# for(i in 1:ncol(syv.sap)){
#   plot(syv.sap[gsind,i]~syv.twr.std$VPD_PI_1[gsind], pch='.', main=paste(syv.tree$SPP[i], syv.tree$Sens[i]), col=colvec[gsind])
# }
# 

syv.16.17<-aggregate(syv.sap[syv.twr.std$YEAR!=2015,], by=list(syv.twr.std$DTIME[syv.twr.std$YEAR!=2015]), FUN='mean', na.rm=TRUE)
wcr.16.17<-aggregate(wcr.sap[wcr.twr.std$YEAR!=2015,], by=list(wcr.twr.std$DTIME[wcr.twr.std$YEAR!=2015]), FUN='mean', na.rm=TRUE)

# 
# for(i in 1:ncol(syv.sap)){
#   plot(syv.16.17[,i+1], pch='.', main=paste(syv.tree$SPP[i], syv.tree$Sens[i]))
# }
# 
# for(i in 1:ncol(wcr.sap)){
#   plot(wcr.16.17[,i+1], pch='.', main=paste(wcr.tree$SPP[i], wcr.tree$Sens[i]))
# }


####PLOTS####

twrdat<-syv.twr.std #twrdat is the input for setind
source('Set_ind.R') #short script sets indices; may modify to have days of gs as input
syv.ts<-syv.twr.std[1:8]; wcr.ts<-wcr.twr.std[1:8]

#Subset to GS
wcr.sapfig<-wcr.sap[wcr.ts$DOY%in%gs,];syv.sapfig<-syv.sap[syv.ts$DOY%in%gs,]
ts.gs<-wcr.ts[wcr.ts$DOY%in%gs,]

#To pair w/o editing gap
wcr.sapfig[is.na(rowMeans(syv.sapfig, na.rm=TRUE)),]<-NA

source('Calc_Sapflow_Full_v2.R')


#Create lists of years of flux/flow rates
yearlist<-unique(syv.twr.std$YEAR)
syv.sapday<-vector('list',length(yearlist)); wcr.sapday<-vector('list',length(yearlist))
syv.meg<-vector('list',length(yearlist)); wcr.meg<-vector('list',length(yearlist)); 
ts.list<-vector('list',length(yearlist)); 

for(i in 1:length(yearlist)){
ind<-which(ts.gs$YEAR==yearlist[i])

#daily flux rates
wcr.sapday[[i]]<-aggregate(wcr.sapfig[ind,], by=list(wcr.ts$DOY[ind]), FUN='mean')[2:(ncol(wcr.sapfig)+1)]
syv.sapday[[i]]<-aggregate(syv.sapfig[ind,], by=list(syv.ts$DOY[ind]), FUN='mean')[2:(ncol(syv.sapfig)+1)]

#30min flows
wcr.meg[[i]]<-wcr.mega[wcr.ts$YEAR==yearlist[i],]
syv.meg[[i]]<-syv.mega[syv.ts$YEAR==yearlist[i],]

#Timestamp
ts.list[[i]]<-syv.ts[syv.ts$YEAR==yearlist[i],]

#To pair without pairing gap
#wcr.sapday[is.na(rowMeans(syv.sapday[,2:21])),2:15]<-NA

}

###Maple barplots####

par(mfrow=c(3,2))
for(i in 1:length(wcr.sapday)){
  
wcr.sapyr<-wcr.sapday[[i]]
syv.sapyr<-syv.sapday[[i]]
#par(mfrow=c(1,2))

wcr.maple<-wcr.sapyr[,which(wcr.tree$SPP=="ACSA")]
wcr.maple$mean<-rowMeans(wcr.maple)

syv.maple<-syv.sapyr[,which(syv.tree$SPP=="ACSA")]
syv.maple$mean<-rowMeans(syv.maple)

par(mar=c(2,5,2,1))

boxplot(wcr.maple[,1:5], ylim=c(0,45), xlim=c(0,9),col=c(rep(NA,5), "orange"), main="MF", xaxt='n', ylab=expression("V"[s]~"(ACSA)"~(g~m^-2~s^-1)))
boxplot(wcr.maple[6],at=8, add=TRUE, col='orange', names="Mean", width=1.2);axis(side=1, at=8, tick=F, lab="Avg.")
box(lwd=3)

boxplot(syv.maple, ylim=c(0,45), xlim=c(0,9),col=c(rep(NA,7), "orange"), main="OF", xaxt='n');axis(side=1, at=8, tick=F, lab="Avg.")
box(lwd=3)
text(7,45, yearlist[i])
}

###Species flow bricks####
par(mfrow=c(3,2))
for(i in 1:length(yearlist)){
wcr.megyr<-wcr.meg[[i]]; syv.megyr<-syv.meg[[i]]
ts<-ts.list[[i]]; day<-which(ts$HOUR%in%dayhr)

wcr.forest.day<-aggregate(wcr.megyr[day,], by=list(wcr.ts$DOY[day]), FUN='sum')[,2:305]
syv.forest.day<-aggregate(syv.megyr[day,], by=list(syv.ts$DOY[day]), FUN='sum')[,2:303]

boxplot(colMeans(wcr.forest.day[gs,], na.rm=TRUE)~toupper(wcr.forest$species), col=c('orange', 'yellow green','dark red','orange4','yellow','gray'), ylim=c(0,300), ylab=expression("Tree T. ("~L~day-1*")"))
text(6,290, yearlist[i])
boxplot(colMeans(syv.forest.day[gs,], na.rm=TRUE)~toupper(syv.forest$species), col=c('orange','blue','dark red','forest green','gray'), ylim=c(0,300))
text(5,290, yearlist[i])

}

for(i in 1:length(yearlist)){
  wcr.megyr<-wcr.meg[[i]]; syv.megyr<-syv.meg[[i]]
  ts<-ts.list[[i]]; day<-which(ts$HOUR%in%dayhr)
  
  wcr.forest.day<-aggregate(wcr.megyr[day,], by=list(wcr.ts$DOY[day]), FUN='sum')[,2:305]
  syv.forest.day<-aggregate(syv.megyr[day,], by=list(syv.ts$DOY[day]), FUN='sum')[,2:303]
  
  boxplot(colMeans(wcr.forest.day[gs,], na.rm=TRUE)~toupper(wcr.forest$species), col=c('orange', 'yellow green','dark red','orange4','yellow','gray'), ylim=c(0,100), ylab=expression("Tree T. ("~L~day-1*")"))
  text(6,95, yearlist[i])
  boxplot(colMeans(syv.forest.day[gs,], na.rm=TRUE)~toupper(syv.forest$species), col=c('orange','blue','dark red','forest green','gray'), ylim=c(0,100))
  text(5,95, yearlist[i])
  
}

####Stacked plots, curve####

for(i in 1:length(yearlist)){
  
ind<-c(1:365)
wcr.megyr<-wcr.meg[[i]]; syv.megyr<-syv.meg[[i]]
ts<-ts.list[[i]]
  
alltree.syv.pd<-aggregate(syv.megyr, by=list(ts$DOY), FUN='sum')[,2:303]
alltree.wcr.pd<-aggregate(wcr.megyr, by=list(ts$DOY), FUN='sum')[,2:305]


#prep lines####
syv.hl.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest.dat$species=='tsca']), 14, FUN='mean', na.rm=TRUE)
syv.sm.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest.dat$species=='acsa']), 14, FUN='mean', na.rm=TRUE)
syv.hb.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest.dat$species=='osvi']), 14, FUN='mean', na.rm=TRUE)
syv.yb.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest.dat$species=='beal']), 14, FUN='mean', na.rm=TRUE)
syv.uk.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest.dat$species=='uk']), 14, FUN='mean', na.rm=TRUE)

#tots<-data.frame(cbind(syv.hl.tot,syv.sm.tot,syv.hb.tot,syv.yb.tot, syv.uk.tot))

syv.sm.tot[is.na(syv.sm.tot)]<-0
syv.hl.tot[is.na(syv.hl.tot)]<-0
syv.yb.tot[is.na(syv.yb.tot)]<-0
syv.hb.tot[is.na(syv.hb.tot)]<-0
syv.uk.tot[is.na(syv.uk.tot)]<-0

wcr.ab.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.dat$species=='tiam']), 14, FUN='mean', na.rm=TRUE)
wcr.sm.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.dat$species=='acsa']), 14, FUN='mean', na.rm=TRUE)
wcr.hb.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.dat$species=='osvi']), 14, FUN='mean', na.rm=TRUE)
wcr.ga.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.dat$species=='frpe']), 14, FUN='mean', na.rm=TRUE)
wcr.uk.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.dat$species=='uk']), 14, FUN='mean', na.rm=TRUE)

#tots<-data.frame(cbind(wcr.ab.tot,wcr.sm.tot,wcr.hb.tot,wcr.ga.tot))

wcr.sm.tot[is.na(wcr.sm.tot)]<-0
wcr.ab.tot[is.na(wcr.ab.tot)]<-0
wcr.ga.tot[is.na(wcr.ga.tot)]<-0
wcr.hb.tot[is.na(wcr.hb.tot)]<-0
wcr.uk.tot[is.na(wcr.uk.tot)]<-0
#####

#wcr profile
plot(wcr.sm.tot[ind], col='white', ylim=c(0,10000), xlim=c(min(ind),max(ind)),main='WCR', ylab="Sap flow (L day-1)", xlab='DOY', font=2, font.lab=2)
polygon(y=c(wcr.sm.tot[ind]+wcr.ab.tot[ind]+wcr.ga.tot[ind]+wcr.hb.tot[ind]+wcr.uk.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)),col='gray')
polygon(y=c(wcr.sm.tot[ind]+wcr.ab.tot[ind]+wcr.ga.tot[ind]+wcr.hb.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)),col='dark red')
polygon(y=c(wcr.sm.tot[ind]+wcr.ab.tot[ind]+wcr.ga.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)),col='yellow green')
polygon(y=c(wcr.sm.tot[ind]+wcr.ab.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)),col='yellow')
polygon(y=c(wcr.sm.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)), col='orange')
if(yearlist[i]=='2015'){polygon(y=c(0,10000,10000,0), x=c(0,0,177,177), col='light gray',border=NA)}
if(yearlist[i]=='2016'){polygon(y=c(0,10000,10000,0), x=c(330,330,365,365), col='dark red',border=NA, density=20);text(280,9000,"Nov 26")}
if(yearlist[i]=='2017'){polygon(y=c(0,10000,10000,0), x=c(0,0,128,128), col='dark red',border=NA, density=20); text(165,9000,"May 8")}


#syv profile
plot(syv.sm.tot[ind], col='white', ylim=c(0,10000), xlim=c(min(ind),max(ind)), main='SYV', ylab="Sap flow (L day-1)", xlab='DOY', font=2, font.lab=2)
polygon(y=c(syv.sm.tot[ind]+syv.hl.tot[ind]+syv.yb.tot[ind]+syv.hb.tot[ind]+syv.uk.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)),col='gray')
polygon(y=c(syv.sm.tot[ind]+syv.hl.tot[ind]+syv.yb.tot[ind]+syv.hb.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)),col='dark red')
polygon(y=c(syv.sm.tot[ind]+syv.hl.tot[ind]+syv.yb.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)),col='blue')
polygon(y=c(syv.sm.tot[ind]+syv.hl.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)),col='forest green')
polygon(y=c(syv.sm.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)), col='orange')
if(yearlist[i]=='2015'){polygon(y=c(0,10000,10000,0), x=c(0,0,137,137), col='light gray',border=NA)}
if(yearlist[i]=='2016'){polygon(y=c(0,10000,10000,0), x=c(156,156,177,177), col='dark red',border=NA, density=20); text(120,9000,"Jun 5"); text(215,9000,"Jun 26")}
if(yearlist[i]=='2017'){polygon(y=c(0,10000,10000,0), x=c(280,280,365,365), col='dark red',border=NA, density=20);text(245,9000,"Oct 7") }


text(330,9000,yearlist[i], font=2)
}

####Profs all together####

par(mfrow=c(1,2))
wcr.megday<-aggregate(wcr.mega, by=list(wcr.ts$DTIME), FUN='mean', na.rm=TRUE)
syv.megday<-aggregate(syv.mega, by=list(syv.ts$DTIME), FUN='mean', na.rm=TRUE)

ts<-syv.ts[syv.ts$YEAR==2016,]

alltree.syv.pd<-aggregate(syv.megday, by=list(ts$DOY), FUN='sum')[,3:304]
alltree.wcr.pd<-aggregate(wcr.megday, by=list(ts$DOY), FUN='sum')[,3:306]

#prep again...####
syv.hl.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest.dat$species=='tsca']), 14, FUN='mean', na.rm=TRUE)
syv.sm.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest.dat$species=='acsa']), 14, FUN='mean', na.rm=TRUE)
syv.hb.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest.dat$species=='osvi']), 14, FUN='mean', na.rm=TRUE)
syv.yb.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest.dat$species=='beal']), 14, FUN='mean', na.rm=TRUE)
syv.uk.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest.dat$species=='uk']), 14, FUN='mean', na.rm=TRUE)

#tots<-data.frame(cbind(syv.hl.tot,syv.sm.tot,syv.hb.tot,syv.yb.tot, syv.uk.tot))

syv.sm.tot[is.na(syv.sm.tot)]<-0
syv.hl.tot[is.na(syv.hl.tot)]<-0
syv.yb.tot[is.na(syv.yb.tot)]<-0
syv.hb.tot[is.na(syv.hb.tot)]<-0
syv.uk.tot[is.na(syv.uk.tot)]<-0

wcr.ab.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.dat$species=='tiam']), 14, FUN='mean', na.rm=TRUE)
wcr.sm.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.dat$species=='acsa']), 14, FUN='mean', na.rm=TRUE)
wcr.hb.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.dat$species=='osvi']), 14, FUN='mean', na.rm=TRUE)
wcr.ga.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.dat$species=='frpe']), 14, FUN='mean', na.rm=TRUE)
wcr.uk.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest.dat$species=='uk']), 14, FUN='mean', na.rm=TRUE)

#tots<-data.frame(cbind(wcr.ab.tot,wcr.sm.tot,wcr.hb.tot,wcr.ga.tot))

wcr.sm.tot[is.na(wcr.sm.tot)]<-0
wcr.ab.tot[is.na(wcr.ab.tot)]<-0
wcr.ga.tot[is.na(wcr.ga.tot)]<-0
wcr.hb.tot[is.na(wcr.hb.tot)]<-0
wcr.uk.tot[is.na(wcr.uk.tot)]<-0
#####

#wcr profile
plot(wcr.sm.tot[ind], col='white', ylim=c(0,10000), xlim=c(min(ind),max(ind)),main='WCR', ylab="Sap flow (L day-1)", xlab='DOY', font=2, font.lab=2)
polygon(y=c(wcr.sm.tot[ind]+wcr.ab.tot[ind]+wcr.ga.tot[ind]+wcr.hb.tot[ind]+wcr.uk.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)),col='gray')
polygon(y=c(wcr.sm.tot[ind]+wcr.ab.tot[ind]+wcr.ga.tot[ind]+wcr.hb.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)),col='dark red')
polygon(y=c(wcr.sm.tot[ind]+wcr.ab.tot[ind]+wcr.ga.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)),col='yellow green')
polygon(y=c(wcr.sm.tot[ind]+wcr.ab.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)),col='yellow')
polygon(y=c(wcr.sm.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)), col='orange')


#syv profile
plot(syv.sm.tot[ind], col='white', ylim=c(0,10000), xlim=c(min(ind),max(ind)), main='SYV', ylab="Sap flow (L day-1)", xlab='DOY', font=2, font.lab=2)
polygon(y=c(syv.sm.tot[ind]+syv.hl.tot[ind]+syv.yb.tot[ind]+syv.hb.tot[ind]+syv.uk.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)),col='gray')
polygon(y=c(syv.sm.tot[ind]+syv.hl.tot[ind]+syv.yb.tot[ind]+syv.hb.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)),col='dark red')
polygon(y=c(syv.sm.tot[ind]+syv.hl.tot[ind]+syv.yb.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)),col='blue')
polygon(y=c(syv.sm.tot[ind]+syv.hl.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)),col='forest green')
polygon(y=c(syv.sm.tot[ind],rep(0,length(ind))),x=c(ind,rev(ind)), col='orange')

