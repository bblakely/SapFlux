#Script for multi-year work

##Data for measured trees####
source("Prepare_treedata.R")
rm('syv.calc.rp','wcr.calc.rp') #These sheets show breakdown of RP calculations; not used
syv.tree$CC[c(1,5,11,14,17)]<-substr(syv.tree$CC[c(1,5,11,14,17)],1,1) #Fixes indeterminate canopy classes
print('Tree data ready')
#####

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

rm(wcr.forest.dat,syv.forest.dat)

#####

##Now tower####
source('Prepare_TowerData.R')
rm("SYV_FullRaw","SYV_notfill","syv.rn","syv.twr.extra","WCR_FullRaw","WCR_notfill",
   "shared","shared.rn","syv.orphans","usevars","wcr.orphans", "wcr.twr.extra")
#rm('wcr.twr.std', 'syv.twr.std') #Three-year timeseries. Not currently using.
rm('wcr.twr.2016','wcr.twr.2015','syv.twr.2016','syv.twr.2015')

print('Tower data ready')
#####

##Sapflux####

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
#####
#Super combined sapflux:
syv.sap<-rbind(syv.2015, syv.2016, syv.2017)
wcr.sap<-rbind(wcr.2015, wcr.2016, wcr.2017)

rm(wcr.2015, wcr.2016, wcr.2017,syv.2015, syv.2016, syv.2017)

##Misc other processing####
#Add color to tower data
colvec<-rep('black', nrow(syv.sap));colvec[syv.twr.std$YEAR==2015]<-'red';colvec[syv.twr.std$YEAR==2017]<-'blue'


wcr.twr.std[wcr.twr.std==-9999]<-NA; syv.twr.std[syv.twr.std==-9999]<-NA

#gsind<-which(wcr.twr.std$MONTH%in%c(5:9)&wcr.twr.std$HOUR%in%c(6:20)) #gsind is daytime in growing season
#syv.16.17<-aggregate(syv.sap[syv.twr.std$YEAR!=2015,], by=list(syv.twr.std$DTIME[syv.twr.std$YEAR!=2015]), FUN='mean', na.rm=TRUE)
#wcr.16.17<-aggregate(wcr.sap[wcr.twr.std$YEAR!=2015,], by=list(wcr.twr.std$DTIME[wcr.twr.std$YEAR!=2015]), FUN='mean', na.rm=TRUE)

#####



####PLOTS - setup ####

#Establish indices and timestamps
twrdat<-syv.twr.std #twrdat is the input for setind
source('Set_ind.R') #short script sets indices; may modify to have days of gs as input
syv.ts<-syv.twr.std[1:8]; wcr.ts<-wcr.twr.std[1:8]

rm(twrdat, midday, midgs, daygs, dayhr, dayind, gsind, daymid)

#Subset to GS
wcr.sapfig<-wcr.sap[wcr.ts$DOY%in%gs,];syv.sapfig<-syv.sap[syv.ts$DOY%in%gs,]
ts.gs<-wcr.ts[wcr.ts$DOY%in%gs,]

#To pair w/o editing gap
#wcr.sapfig[is.na(rowMeans(syv.sapfig, na.rm=TRUE)),]<-NA #Remove values where wcr has and SYV does not


##Finding dry days####
syv.twr.d<-aggregate(syv.twr.std, by=list(syv.twr.std$DOY, syv.twr.std$YEAR), FUN=mean, na.rm=TRUE)
#syv.pet.d<-aggregate(syv.pet$LE_pot,by=list(syv.twr$DOY, syv.twr$YEAR), FUN=mean, na.rm=TRUE)
wcr.twr.d<-aggregate(wcr.twr.std, by=list(wcr.twr.std$DOY, wcr.twr.std$YEAR), FUN=mean, na.rm=TRUE)
#wcr.pet.d<-aggregate(wcr.pet$LE_pot,by=list(wcr.twr$DOY, wcr.twr$YEAR), FUN=mean, na.rm=TRUE)

decyr<-syv.twr.d$YEAR+(syv.twr.d$DOY/365)

syv.airdry<-quantile(syv.twr.d$VPD_PI_1, 0.85, na.rm=TRUE)
syv.dirtdry<-quantile(syv.twr.d$SWC_1, 0.15, na.rm=TRUE)

wcr.airdry<-quantile(wcr.twr.d$VPD_PI_1, 0.85, na.rm=TRUE)
wcr.dirtdry<-quantile(wcr.twr.d$SWC_1, 0.15, na.rm=TRUE)

syv.drytimes<-which(syv.twr.d$VPD_PI_1>syv.airdry & syv.twr.d$SWC_1<syv.dirtdry)
wcr.drytimes<-which(wcr.twr.d$VPD_PI_1>wcr.airdry & wcr.twr.d$SWC_1<wcr.dirtdry)
####

#Quick look at where dry times land

plot(syv.twr.d$LE_1)
points(syv.twr.d$LE_1[syv.drytimes]~syv.drytimes, col='red')

plot(wcr.twr.d$LE_1)
points(wcr.twr.d$LE_1[wcr.drytimes]~wcr.drytimes, col='red')

par(mfrow=c(1,2))
hist(wcr.twr.d$MONTH[wcr.drytimes], breaks = c(5,6,7,8,9,10));hist(syv.twr.d$MONTH[syv.drytimes], breaks = c(5,6,7,8,9,10))
par(mfrow=c(1,1))

#Calculate plot-level sapflux
source('Calc_Sapflow_Full_v2.R')

#Cleanup of 'values'
rm(list=ls(pattern="^big"));rm(list=ls(pattern="dirtdry"));rm(list=ls(pattern="airdry"))

#####

##Make 'master' datasets####
yearlist<-unique(syv.twr.std$YEAR)

#Empty spaces
syv.day<-syv.sapday<-syv.sapday.full<-syv.sap.1<-syv.meg<-ts.list<-vector('list',length(yearlist))
wcr.day<-wcr.sapday<-wcr.sapday.full<-wcr.sap.1<-wcr.meg<-ts.list<-vector('list',length(yearlist))

for(i in 1:length(yearlist)){
  
ind<-which(ts.gs$YEAR==yearlist[i])
ind.full<-which(wcr.ts$YEAR==yearlist[i]) #use of wcr is arbitrary; both are the same

#average daily flux rates, measured trees, growing season
wcr.sapday[[i]]<-aggregate(wcr.sapfig[ind,], by=list(wcr.ts$DOY[ind]), FUN='mean')[2:(ncol(wcr.sapfig)+1)]
syv.sapday[[i]]<-aggregate(syv.sapfig[ind,], by=list(syv.ts$DOY[ind]), FUN='mean')[2:(ncol(syv.sapfig)+1)]

#aveerage daily flux rates, measured trees, all year
wcr.sapday.full[[i]]<-aggregate(wcr.sap[ind.full,], by=list(wcr.ts$DOY[ind.full]), FUN='mean')[2:(ncol(wcr.sap)+1)]
syv.sapday.full[[i]]<-aggregate(syv.sap[ind.full,], by=list(syv.ts$DOY[ind.full]), FUN='mean')[2:(ncol(syv.sap)+1)]

#Originally measured 30 min flux rates, all year.
wcr.sap.1[[i]]<-wcr.sap[wcr.ts$YEAR==yearlist[i],]
syv.sap.1[[i]]<-syv.sap[syv.ts$YEAR==yearlist[i],]

#Flows for all trees, 30min intervals, all year.
wcr.meg[[i]]<-wcr.mega[wcr.ts$YEAR==yearlist[i],]
syv.meg[[i]]<-syv.mega[syv.ts$YEAR==yearlist[i],]


#Timestamp
ts.list[[i]]<-syv.ts[syv.ts$YEAR==yearlist[i],]

#Canopy level sums of daily flows. na.rm may affect results.
syv.day[[i]]<-aggregate(syv.meg[[i]], by=list(ts.list[[i]]$DOY), FUN='sum', na.rm=TRUE)[,2:303]
wcr.day[[i]]<-aggregate(wcr.meg[[i]], by=list(ts.list[[i]]$DOY), FUN='sum', na.rm=TRUE)[,2:305]


}

rm(syv.mega, wcr.mega, ind.full)
#####

###Maple barplots####

par(mfrow=c(3,2))
for(i in 1:length(wcr.sapday)){
  
wcr.sapyr<-wcr.sapday[[i]] #Reminder: wcr.sapday is daily growing season fluxes for each measured tree
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

rm(wcr.maple, syv.maple)

###Species flow bricks####
par(mfrow=c(3,2))

#These are species average flows in the daytime during the growing season
for(i in 1:length(yearlist)){
wcr.megyr<-wcr.meg[[i]]; syv.megyr<-syv.meg[[i]] #Reminder: wcr.meg is sap flows for all trees
ts<-ts.list[[i]]; day<-which(ts$HOUR%in%dayhr)

#Notice subset to day; these are daytime fluxes only
wcr.forest.day<-aggregate(wcr.megyr[day,], by=list(wcr.ts$DOY[day]), FUN='sum')[,2:305]
syv.forest.day<-aggregate(syv.megyr[day,], by=list(syv.ts$DOY[day]), FUN='sum')[,2:303]

#Notice the gs subset; these are for growing season days
boxplot(colMeans(wcr.forest.day[gs,], na.rm=TRUE)~toupper(wcr.forest$species), col=c('orange', 'yellow green','dark red','orange4','yellow','gray'), ylim=c(0,300), ylab=expression("Tree T. ("~L~day-1*")"))
text(6,290, yearlist[i])
boxplot(colMeans(syv.forest.day[gs,], na.rm=TRUE)~toupper(syv.forest$species), col=c('orange','blue','dark red','forest green','gray'), ylim=c(0,300))
text(5,290, yearlist[i])

}

#Same thing but scaled with birch out of the picture, to highlight differences among trees
for(i in 1:length(yearlist)){
  wcr.megyr<-wcr.meg[[i]]; syv.megyr<-syv.meg[[i]]
  ts<-ts.list[[i]]; day<-which(ts$HOUR%in%dayhr)
  boxplot(colMeans(wcr.forest.day[gs,], na.rm=TRUE)~toupper(wcr.forest$species), col=c('orange', 'yellow green','dark red','orange4','yellow','gray'), ylim=c(0,100), ylab=expression("Tree T. ("~L~day-1*")"))
  text(6,95, yearlist[i])
  boxplot(colMeans(syv.forest.day[gs,], na.rm=TRUE)~toupper(syv.forest$species), col=c('orange','blue','dark red','forest green','gray'), ylim=c(0,100))
  text(5,95, yearlist[i])
  
}

rm(wcr.forest.day, syv.forest.day)

####Stacked plots, curve####

for(i in 1:length(yearlist)){
  
ind<-c(1:365)
wcr.megyr<-wcr.meg[[i]]; syv.megyr<-syv.meg[[i]]
ts<-ts.list[[i]]

#These are different than wcr.forest.day because they are all year and all hours  
alltree.syv.pd<-aggregate(syv.megyr, by=list(ts$DOY), FUN='sum')[,2:303]
alltree.wcr.pd<-aggregate(wcr.megyr, by=list(ts$DOY), FUN='sum')[,2:305]

library(zoo)
#prep lines####
syv.hl.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest$species=='tsca']), 14, FUN='mean', na.rm=TRUE)
syv.sm.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest$species=='acsa']), 14, FUN='mean', na.rm=TRUE)
syv.hb.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest$species=='osvi']), 14, FUN='mean', na.rm=TRUE)
syv.yb.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest$species=='beal']), 14, FUN='mean', na.rm=TRUE)
syv.uk.tot<-rollapply(rowSums(alltree.syv.pd[,syv.forest$species=='uk']), 14, FUN='mean', na.rm=TRUE)

#tots<-data.frame(cbind(syv.hl.tot,syv.sm.tot,syv.hb.tot,syv.yb.tot, syv.uk.tot))

syv.sm.tot[is.na(syv.sm.tot)]<-0
syv.hl.tot[is.na(syv.hl.tot)]<-0
syv.yb.tot[is.na(syv.yb.tot)]<-0
syv.hb.tot[is.na(syv.hb.tot)]<-0
syv.uk.tot[is.na(syv.uk.tot)]<-0

wcr.ab.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest$species=='tiam']), 14, FUN='mean', na.rm=TRUE)
wcr.sm.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest$species=='acsa']), 14, FUN='mean', na.rm=TRUE)
wcr.hb.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest$species=='osvi']), 14, FUN='mean', na.rm=TRUE)
wcr.ga.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest$species=='frpe']), 14, FUN='mean', na.rm=TRUE)
wcr.uk.tot<-rollapply(rowSums(alltree.wcr.pd[,wcr.forest$species=='uk']), 14, FUN='mean', na.rm=TRUE)

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

rm(ts)
rm(list=ls(pattern=".tot"))


#Comparison bars
stackflow<-function(syv.flow,wcr.flow,syv.tree, wcr.tree, gs, title="Total Sap Flow"){
  
  wcr.flow.b<-colMeans(wcr.flow[gs,], na.rm=TRUE)
  syv.flow.b<-colMeans(syv.flow[gs,], na.rm=TRUE)
  syv.tree.b<-syv.tree
  wcr.tree.b<-wcr.tree
  
  
  wcr.b<-aggregate(wcr.flow.b, by=list(wcr.tree.b$species), FUN=sum,na.rm=TRUE)
  syv.b<-aggregate(syv.flow.b, by=list(syv.tree.b$species), FUN=sum, na.rm=TRUE)
  
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
          main=title,names.arg=c('MF','OF'),ylab=expression(Transpiration~(L~day^-1)), ylim=c(0,10000), 
          cex.axis=2, cex.lab=2,cex.main=2.5, cex.names=2, font.axis=2,font.lab=2,font.main=2)
}
par(mar=c(4,5,4,2))
par(mfrow=c(3,3))


#Barplots (original + set up structural pooling for later) ####
diff.orig<-rep(0,3)
diff.swa<-rep(0,3)
diff.lai<-rep(0,3)
for(i in 1:length(yearlist)){
  
  #ind<-c(1:365)
  wcr.megyr<-wcr.meg[[i]]; syv.megyr<-syv.meg[[i]]
  ts<-ts.list[[i]]
  
  alltree.syv.pd<-aggregate(syv.megyr, by=list(ts$DOY), FUN='sum')[,2:303]
  alltree.wcr.pd<-aggregate(wcr.megyr, by=list(ts$DOY), FUN='sum')[,2:305]
  
  
  stackflow(alltree.syv.pd,alltree.wcr.pd,syv.forest,wcr.forest,c(150:250), title=yearlist[i])  
  
  diff.orig[i]<-sum(colMeans(alltree.wcr.pd[150:250,], na.rm=TRUE))-sum(colMeans(alltree.syv.pd[150:250,], na.rm=TRUE))
  
  diff.swa[i]<-(sum(colMeans(alltree.wcr.pd[150:250,], na.rm=TRUE))*1.13)-(sum(colMeans(alltree.syv.pd[150:250,], na.rm=TRUE))*0.9)

  diff.lai[i]<-(sum(colMeans(alltree.wcr.pd[150:250,], na.rm=TRUE))*1.09)-(sum(colMeans(alltree.syv.pd[150:250,], na.rm=TRUE))*0.924)

}

rm(wcr.megyr, syv.megyr)

#####
#Pooled sm fluxes####
diff.sm<-rep(0,3)
for(i in 1:length(yearlist)){
#Prep####
#SYV
HL.syv<-c(1:4,8,10,19:20)  #11 is left out because bad data quality
SM.syv<-c(9,13:18);HB.syv<-c(5,6);YB.syv<-c(7,12)
HL.tr.f<-rowMeans(syv.sap.1[[i]][,HL.syv], na.rm=TRUE)
SM.tr.f<-rowMeans(syv.sap.1[[i]][,SM.syv], na.rm=TRUE) #Few trees dead in winter; this gives better timeseries
HB.tr.f<-rowMeans(syv.sap.1[[i]][,HB.syv], na.rm=TRUE)
YB.tr.f<-rowMeans(syv.sap.1[[i]][,YB.syv], na.rm=TRUE)
#WCR
AB.wcr<-c(1:2,8,11);SM.wcr<-c(4:6,12:13);HB.wcr<-c(3,7,14);GA.wcr<-c(9:10)
AB.tr.f<-rowMeans(wcr.sap.1[[i]][,AB.wcr], na.rm=TRUE)
SM.tr.f<-rowMeans(wcr.sap.1[[i]][,SM.wcr], na.rm=TRUE) #Few trees dead in winter; this gives better timeseries
HB.tr.f<-rowMeans(wcr.sap.1[[i]][,HB.wcr], na.rm=TRUE)
GA.tr.f<-rowMeans(wcr.sap.1[[i]][,GA.wcr], na.rm=TRUE)
#####

SM.syv<-c(9,13:17)
SM.wcr<-c(4:6,12:13)

SM.pool<-cbind(wcr.sap.1[[i]][,SM.wcr],syv.sap.1[[i]][,SM.syv])
SM.tr.f.new<-rowMeans(SM.pool, na.rm=TRUE) #pooled SM fluxes

syv.lut.smpool<-data.frame(cbind(HL.tr.f,SM.tr.f.new,HB.tr.f,YB.tr.f))
colnames(syv.lut.smpool)<-c('TSCA','ACSA','OSVI','BEAL')
syv.lut.smpool$UK<-rowMeans(syv.lut.smpool)

wcr.lut.smpool<-data.frame(cbind(SM.tr.f.new,AB.tr.f,HB.tr.f,GA.tr.f))
colnames(wcr.lut.smpool)<-c('ACSA','TIAM','OSVI','FRPE')
wcr.lut.smpool$UK<-wcr.lut.smpool$QURU<-rowMeans(wcr.lut.smpool)

syv.smpool.hh<-modeltrees(syv.lut.smpool, syv.forest)
wcr.smpool.hh<-modeltrees(wcr.lut.smpool, wcr.forest)

syv.smpool<-aggregate(syv.smpool.hh, by=list(ts.list[[i]]$DOY), FUN='sum')[,2:303]
wcr.smpool<-aggregate(wcr.smpool.hh, by=list(ts.list[[i]]$DOY), FUN='sum')[,2:305]

stackflow(syv.smpool,wcr.smpool,syv.forest,wcr.forest,c(150:250), title=yearlist[i])
diff.sm[i]<-sum(colMeans(wcr.smpool[150:250,], na.rm=TRUE))-sum(colMeans(syv.smpool[150:250,], na.rm=TRUE))

}

#####
#Pooled deciduous fluxes####
diff.dc<-rep(0,3)
for(i in 1:length(yearlist)){
  #Prep####
  #SYV
  HL.syv<-c(1:4,8,10,19:20)  #11 is left out because bad data quality
  SM.syv<-c(9,13:18);HB.syv<-c(5,6);YB.syv<-c(7,12)
  HL.tr.f<-rowMeans(syv.sap.1[[i]][,HL.syv], na.rm=TRUE)
  SM.tr.f<-rowMeans(syv.sap.1[[i]][,SM.syv], na.rm=TRUE) #Few trees dead in winter; this gives better timeseries
  HB.tr.f<-rowMeans(syv.sap.1[[i]][,HB.syv], na.rm=TRUE)
  YB.tr.f<-rowMeans(syv.sap.1[[i]][,YB.syv], na.rm=TRUE)
  #WCR
  AB.wcr<-c(1:2,8,11);SM.wcr<-c(4:6,12:13);HB.wcr<-c(3,7,14);GA.wcr<-c(9:10)
  AB.tr.f<-rowMeans(wcr.sap.1[[i]][,AB.wcr], na.rm=TRUE)
  SM.tr.f<-rowMeans(wcr.sap.1[[i]][,SM.wcr], na.rm=TRUE) #Few trees dead in winter; this gives better timeseries
  HB.tr.f<-rowMeans(wcr.sap.1[[i]][,HB.wcr], na.rm=TRUE)
  GA.tr.f<-rowMeans(wcr.sap.1[[i]][,GA.wcr], na.rm=TRUE)
  #####
  
dc.pool.syv<-sort(c(SM.syv,HB.syv,YB.syv))
dc.pool.wcr<-c(1:14)
dc.pool<-cbind(wcr.sap.1[[i]][,dc.pool.wcr],syv.sap.1[[i]][,dc.pool.syv])

dc.tr.f.new<-rowMeans(dc.pool, na.rm=TRUE)

syv.lut.dcpool<-data.frame(cbind(HL.tr.f,dc.tr.f.new,dc.tr.f.new,dc.tr.f.new))
colnames(syv.lut.dcpool)<-c('TSCA','ACSA','OSVI','BEAL')
syv.lut.dcpool$UK<-rowMeans(syv.lut.dcpool)

wcr.lut.dcpool<-data.frame(cbind(dc.tr.f.new,dc.tr.f.new,dc.tr.f.new,dc.tr.f.new))
colnames(wcr.lut.dcpool)<-c('ACSA','TIAM','OSVI','FRPE')
wcr.lut.dcpool$UK<-wcr.lut.dcpool$QURU<-rowMeans(wcr.lut.dcpool)

syv.dcpool.hh<-modeltrees(syv.lut.dcpool, syv.forest)
wcr.dcpool.hh<-modeltrees(wcr.lut.dcpool, wcr.forest)

syv.dcpool<-aggregate(syv.dcpool.hh, by=list(ts.list[[i]]$DOY), FUN='sum')[,2:303]
wcr.dcpool<-aggregate(wcr.dcpool.hh, by=list(ts.list[[i]]$DOY), FUN='sum')[,2:305]

stackflow(syv.dcpool,wcr.dcpool,syv.forest,wcr.forest,c(150:250), title=yearlist[i])
diff.dc[i]<-sum(colMeans(wcr.dcpool[150:250,], na.rm=TRUE))-sum(colMeans(syv.dcpool[150:250,], na.rm=TRUE))

}

#Cleanup
rm(list=ls(pattern="smpool"));rm(list=ls(pattern="dc.pool"));rm(list=ls(pattern=".tr.f"))
rm(list=ls(pattern="AB."));rm(list=ls(pattern="HL."));rm(list=ls(pattern="SM."));rm(list=ls(pattern="YB."));rm(list=ls(pattern="HB."));rm(list=ls(pattern="GA."))

#####

##Scaling experiment plot####

par(mfrow=c(1,1), xpd=FALSE); xax<-c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5)*0.5
cols=rep(c('red','black','blue'), 3)
#pch=c(15,16,17,15,16,17,15,16,17,15,16,17)
pch=c(1,2,0)
 
plot(c(diff.orig-mean(diff.orig), diff.sm-mean(diff.orig), diff.dc-mean(diff.orig), diff.swa-mean(diff.orig),diff.lai-mean(diff.orig))~xax, ylim=c(-7000,4000),xlim=c(0.4, 2.6), 
     xaxt='n', yaxt='n', ylab="OF-MF (L/day)",xlab='', col=cols, pch=pch, cex=1.5, cex.lab=1.2, cex.axis=1.2)

axis(side=1, at=c(1,2,3,4,5)*0.5, labels=c("Original", "Pooled Species", "Pooled PFT", "Pooled SWA", "Pooled LAI"), cex.axis=1)
opos<-c(-6000,-4000,-2000,0,2000,4000)
axis(side=2, at=opos, labels=opos +1500, cex.axis=1)

abline(h=0);abline(v=0.75, lty=2)
legend(2.2,-3000, legend=c(2015,2016,2017), col=cols[1:3], pch=pch[1:3], cex=1.3)


#Barplot form
diffs<-data.frame(c(diff.orig-mean(diff.orig), diff.sm-mean(diff.orig), diff.dc-mean(diff.orig), diff.swa-mean(diff.orig),diff.lai-mean(diff.orig)));colnames(diffs)[1]<-"num"
diffs$trt<-rep(c("Fully Informed", "Pooled Species", "Pooled PFT", "Pooled SWA", "Pooled LAI"), each=3)
diffs$yr<-rep(c(2015,2016,2017), 5)

sd.o<-sd(diff.orig-mean(diff.orig));sd.s<-sd(diff.sm-mean(diff.orig));sd.p<-sd(diff.dc-mean(diff.orig));sd.w<-sd(diff.swa-mean(diff.orig));sd.l<-sd(diff.lai-mean(diff.orig))
diffs$sd<-rep(c(sd.o,sd.s,sd.p,sd.w,sd.l), each=3)
diffs.agg<-aggregate(diffs, by=list(diffs$trt), FUN=mean);colnames(diffs.agg)[1]<-"trt1"
library(ggplot2)
ggplot(diffs.agg) +
  geom_hline(yintercept=0)+
  annotate("rect", xmin = 0, xmax = Inf, ymin = -1500, ymax = Inf, alpha = .2)+
  geom_bar(aes(x = trt1, y = num),stat="identity") +
  geom_errorbar(aes(x=trt1, ymin=num-sd, ymax=num+sd), width=0.1)+
  scale_x_discrete(limits=c("Fully Informed", "Pooled Species", "Pooled PFT", "Pooled SWA","Pooled LAI"))+
  ylab("OF-MF (L/day)")+
  xlab("")+
  scale_y_continuous(labels = function(y) y + 1500)+
  annotate("text", x = 0.5, y = 3000, label = "MF > OF", size=5)+
  annotate("text", x = 5, y = -5000, label = "OF > MF", size=5)+
  theme_minimal()

#####

##VPD vs sap####

library(RColorBrewer)
dry<-30
par(mfrow=c(2,3), mar=c(4,4,3,1))
col<-brewer.pal(n=9, name='Oranges')

syv.dayvpd<-aggregate(syv.twr.std$VPD_PI_1, by=list(syv.twr.std$YEAR, syv.twr.std$DOY), FUN='mean', na.rm=TRUE)
syv.dayvpd<-syv.dayvpd[syv.dayvpd$Group.2%in%gs,]

syv.daysoil<-aggregate(syv.twr.std$SWC_1, by=list(syv.twr.std$DOY, syv.twr.std$YEAR), FUN='mean', na.rm=TRUE)
syv.daysoil<-syv.daysoil[syv.daysoil$Group.1%in%gs,]
syv.daysoil$pch<-rep(1, length(syv.daysoil$x));syv.daysoil$pch[syv.daysoil$x<syv.dirtdry]<-19


for (y in 1:3){
plot(syv.sapday[[2]][,1]~syv.dayvpd$x[syv.dayvpd$Group.1==yearlist[2]],pch=pch, ylim=c(0,45),xlim=c(0,18), col='white', main=yearlist[y], xlab='', ylab="Daily avg. Js (g/m2 s)", font.lab=2)
colcount<-0
for(i in c(SM.syv, 11)){
colcount<-colcount+1
points(syv.sapday[[y]][,i]~syv.dayvpd$x[syv.dayvpd$Group.1==yearlist[y]], pch=syv.daysoil$pch[syv.daysoil$Group.2==yearlist[y]], main=i, ylim=c(0,20),col=col[colcount+1])
}
}

wcr.dayvpd<-aggregate(wcr.twr.std$VPD_PI_1, by=list(wcr.twr.std$YEAR, wcr.twr.std$DOY), FUN='mean', na.rm=TRUE)
wcr.dayvpd<-wcr.dayvpd[wcr.dayvpd$Group.2%in%gs,]

wcr.daysoil<-aggregate(wcr.twr.std$SWC_1, by=list(wcr.twr.std$DOY, wcr.twr.std$YEAR), FUN='mean', na.rm=TRUE)
wcr.daysoil<-wcr.daysoil[wcr.daysoil$Group.1%in%gs,]
wcr.daysoil$pch<-rep(1, length(wcr.daysoil$x));wcr.daysoil$pch[wcr.daysoil$x<wcr.dirtdry]<-19


for (y in 1:3){
  plot(wcr.sapday[[2]][,1]~wcr.dayvpd$x[wcr.dayvpd$Group.1==yearlist[2]],pch=pch, ylim=c(0,45),xlim=c(0,18), col='white', main=yearlist[y], xlab="VPD (HPa)", ylab="Daily avg. Js (g/m2 s)", font.lab=2)
  colcount<-0
  for(i in SM.wcr){
    colcount<-colcount+1
    points(wcr.sapday[[y]][,i]~wcr.dayvpd$x[wcr.dayvpd$Group.1==yearlist[y]], pch=wcr.daysoil$pch[wcr.daysoil$Group.2==yearlist[y]], main=i, ylim=c(0,20), col=col[colcount+1])
  }
}


#Overview####

wcr.drytimes.1<-which(wcr.twr.d$SWC_1<30);syv.drytimes.1<-which(syv.twr.d$SWC_1<30);


# par(mfrow=c(2,1), mar=c(2,4.5,1,1))
# xax1<-aggregate((wcr.ts$YEAR)+(wcr.ts$DOY/366), by=list(wcr.ts$DOY, wcr.ts$YEAR), FUN='mean')$x
# wcr.pchvec<-rep(1, length(xax1));wcr.pchvec[wcr.drytimes]<-19
# syv.pchvec<-rep(2, length(xax1));syv.pchvec[syv.drytimes]<-17
# 
# wcr.sapday.all<-rbind(wcr.day[[1]],wcr.day[[2]],wcr.day[[3]])
# plot(rowSums(wcr.sapday.all)~xax1, pch=wcr.pchvec, ylim=c(0,12000), xaxt='n', ylab="Sapflow, L/day")
# 
# syv.sapday.all<-rbind(syv.day[[1]],syv.day[[2]],syv.day[[3]])
# plot(rowSums(syv.sapday.all)~xax1, pch=syv.pchvec,ylim=c(0,12000), ylab="Sapflow, L/day")
# 

par(mfrow=c(2,1), mar=c(2,4.5,1,1))
xax1<-aggregate((wcr.ts$YEAR)+(wcr.ts$DOY/366), by=list(wcr.ts$DOY, wcr.ts$YEAR), FUN='mean')$x
wcr.pchvec<-rep(1, length(xax1));wcr.pchvec[wcr.drytimes]<-19
syv.pchvec<-rep(2, length(xax1));syv.pchvec[syv.drytimes]<-17

wcr.sapday.all<-rbind(wcr.sapday.full[[1]],wcr.sapday.full[[2]],wcr.sapday.full[[3]])
plot(rowMeans(wcr.sapday.all, na.rm=TRUE)~xax1, pch=wcr.pchvec, xaxt='n', ylab="Sapflux g/m2s", ylim=c(0,20), xlim=c(2015.4, 2018))

syv.sapday.all<-rbind(syv.sapday.full[[1]],syv.sapday.full[[2]],syv.sapday.full[[3]])
plot(rowMeans(syv.sapday.all, na.rm=TRUE)~xax1, pch=syv.pchvec, ylab="Sapflux g/m2s", ylim=c(0,20), xlim=c(2015.4, 2018))

#Aggregated plots####
dry.ts<-syv.twr.d[syv.drytimes,1:10]
index<-0
index.r<-0
for(i in 1:nrow(dry.ts)){
  
  subindex<- which(syv.twr$YEAR==dry.ts$YEAR[i]&syv.twr$DOY==dry.ts$DOY[i])
  index<-c(index, subindex)
  
  randyr<-sample(c(2015:2017), 1)
  randDOY<-sample(c(144:259),1)
  subindex.r<- which(syv.twr$YEAR==randyr&syv.twr$DOY==randDOY)
  index.r<-c(index.r, subindex.r)
}

dryind<-index[2:length(index)]
randind<-index.r[2:length(index.r)]

syv.sap.f<-rowMeans(syv.sap, na.rm=TRUE)
drysyv<-cbind(syv.twr, syv.sap.f)[dryind,]
randsyv<-cbind(syv.twr, syv.sap.f)[randind,]

#plot(syv.sap.f~HOUR, data=randsyv, ylim=c(0, 22), pch=19)
#points(syv.sap.f~HOUR, data=drysyv, col='red', pch='*')

#Again for wcr
par(mfrow=c(2,2))
dry.ts<-wcr.twr.d[wcr.drytimes,1:10]
index<-0
index.r<-0
for(i in 1:nrow(dry.ts)){
  
  subindex<- which(wcr.twr$YEAR==dry.ts$YEAR[i]&wcr.twr$DOY==dry.ts$DOY[i])
  index<-c(index, subindex)
  
  randyr<-sample(c(2015:2017), 1)
  randDOY<-sample(c(144:259),1)
  subindex.r<- which(wcr.twr$YEAR==randyr&wcr.twr$DOY==randDOY)
  index.r<-c(index.r, subindex.r)
}

dryind<-index[2:length(index)]
randind<-index.r[2:length(index.r)]

wcr.sap.f<-rowMeans(wcr.sap, na.rm=TRUE)
drywcr<-cbind(wcr.twr, wcr.sap.f)[dryind,]
randwcr<-cbind(wcr.twr, wcr.sap.f)[randind,]

#plot(wcr.sap.f~HOUR, data=randwcr, ylim=c(0, 40), pch=19)
#points(wcr.sap.f~HOUR, data=drywcr, col='red', pch='*')
#####

#Panel 2

par(mfrow=c(2,1))

dryhr<-aggregate(drysyv, by=list(drysyv$HOUR), FUN='mean', na.rm=TRUE);randhr<-aggregate(randsyv, by=list(randsyv$HOUR), FUN='mean', na.rm=TRUE)

plot(syv.sap.f~HOUR, data=randhr, ylim=c(0, 22), pch=1)
points(syv.sap.f~HOUR, data=dryhr, pch=19)

dryhr<-aggregate(drywcr, by=list(drywcr$HOUR), FUN='mean', na.rm=TRUE);randhr<-aggregate(randwcr, by=list(randwcr$HOUR), FUN='mean', na.rm=TRUE)

plot(wcr.sap.f~HOUR, data=randhr, ylim=c(0, 40), pch=1)
points(wcr.sap.f~HOUR, data=dryhr, pch=19)



