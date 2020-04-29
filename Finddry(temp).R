source('Prepare_TowerData_v2.R')


syv.twr<-syv.twr.std;syv.twr[syv.twr==-9999]<-NA
syv.twr<-syv.twr[syv.twr$YEAR%in%c(2015:2017),]

wcr.twr<-wcr.twr.std;wcr.twr[wcr.twr==-9999]<-NA
wcr.twr<-wcr.twr[wcr.twr$YEAR%in%c(2015:2017),]

wcr.ts<-syv.ts<-ts<-syv.twr[syv.twr$YEAR%in%c(2015:2017), 1:8]


# syv.extra<-syv.twr.extra;syv.extra[syv.extra==-9999]<-NA
# syv.extra<-syv.extra[syv.twr.std$YEAR%in%c(2001:2018),]

library(bigleaf)

syv.pet<-potential.ET(Tair=syv.twr$TA_1, pressure=syv.twr$PA_1, Rn=syv.twr$NETRAD_1, VPD=syv.twr$VPD_PI_1, approach="Priestley-Taylor")
wcr.pet<-potential.ET(Tair=wcr.twr$TA_1, pressure=wcr.twr$PA_1, Rn=wcr.twr$NETRAD_1, VPD=wcr.twr$VPD_PI_1, approach="Priestley-Taylor")

# syv.pet<-syv.pet$ET_pot*(60*30)
# pet.month<-aggregate(syv.pet, by=list( syv.twr$MONTH,syv.twr$YEAR), FUN=sum, na.rm=TRUE)$x
# 
# library(scPDSI)
# 
# syv.twr$P_PI_F[is.na(syv.twr$P_PI_F)]<-0
# 
# precip.month<-aggregate(syv.twr$P_PI_F, by=list( syv.twr$MONTH,syv.twr$YEAR), FUN=sum)$x
# 
# test<-pdsi(P=precip.month, PE=pet.month)
# #faketest<-pdsi(Lubuge$P, Lubuge$PE)
# 
# plot(test)

#looks like it was pretty wet all years I measured

gs<-c(200:240)

syv.twr.d<-aggregate(syv.twr, by=list(syv.twr$DOY, syv.twr$YEAR), FUN=mean, na.rm=TRUE)
syv.pet.d<-aggregate(syv.pet$LE_pot,by=list(syv.twr$DOY, syv.twr$YEAR), FUN=mean, na.rm=TRUE)
wcr.twr.d<-aggregate(wcr.twr, by=list(wcr.twr$DOY, wcr.twr$YEAR), FUN=mean, na.rm=TRUE)
wcr.pet.d<-aggregate(wcr.pet$LE_pot,by=list(wcr.twr$DOY, wcr.twr$YEAR), FUN=mean, na.rm=TRUE)



decyr<-syv.twr.d$YEAR+(syv.twr.d$DOY/365)

plot(syv.pet.d$x~decyr);
points(syv.twr.d$LE_1~decyr, col='blue')

plot(wcr.pet.d$x~decyr)
points(wcr.twr.d$LE_1~decyr, col='blue')
#syv.twr.d<-syv.twr.d[syv.twr.d$DOY%in%gs,]

syv.airdry<-quantile(syv.twr.d$VPD_PI_1, 0.85, na.rm=TRUE)
syv.dirtdry<-quantile(syv.twr.d$SWC_1, 0.15, na.rm=TRUE)

wcr.airdry<-quantile(wcr.twr.d$VPD_PI_1, 0.85, na.rm=TRUE)
wcr.dirtdry<-quantile(wcr.twr.d$SWC_1, 0.15, na.rm=TRUE)

syv.drytimes<-which(syv.twr.d$VPD_PI_1>syv.airdry & syv.twr.d$SWC_1<syv.dirtdry)
wcr.drytimes<-which(wcr.twr.d$VPD_PI_1>wcr.airdry & wcr.twr.d$SWC_1<wcr.dirtdry)


plot(syv.twr.d$LE_1)
points(syv.twr.d$LE_1[syv.drytimes]~syv.drytimes, col='red')

plot(wcr.twr.d$LE_1)
points(wcr.twr.d$LE_1[wcr.drytimes]~wcr.drytimes, col='red')


#rain<-which(syv.twr.d$P_PI_F>0.2);abline(v=rain)
#t.test(syv.twr.d$LE_1[drytimes], sample(syv.twr.d$LE_1[syv.twr.d$DOY%in%gs], 20))

plot((syv.pet.d$x-syv.twr.d$LE_1)~decyr, ylab="PLE - LE")
points((syv.pet.d$x-syv.twr.d$LE_1)[syv.drytimes]~decyr[syv.drytimes], col='red')

plot((wcr.pet.d$x-wcr.twr.d$LE_1)~decyr, ylab="PLE - LE")
points((wcr.pet.d$x-wcr.twr.d$LE_1)[wcr.drytimes]~decyr[wcr.drytimes], col='red')



####Pull in sapflux and its prerequisites (copied from Full_TS)####

##Data for measured trees###
source("Prepare_treedata.R")
rm('syv.calc.rp','wcr.calc.rp') #These sheets show breakdown of RP calculations; not used
syv.tree$CC[c(1,5,11,14,17)]<-substr(syv.tree$CC[c(1,5,11,14,17)],1,1)

print('Tree data ready')

##Data for surveyed trees###
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

#Super combined sapflux
syv.sap<-rbind(syv.2015, syv.2016, syv.2017)
wcr.sap<-rbind(wcr.2015, wcr.2016, wcr.2017)

#Before doing this, run tree and forest bits
source('Calc_Sapflow_Full_v2.R') #To get full canopy flow

yearlist<-unique(syv.twr.d$YEAR)
syv.sapday<-vector('list',length(yearlist)); wcr.sapday<-vector('list',length(yearlist))
syv.meg<-vector('list',length(yearlist)); wcr.meg<-vector('list',length(yearlist)); 
ts.list<-vector('list',length(yearlist)); 

for(i in 1:length(yearlist)){
  ind<-which(ts$YEAR==yearlist[i])
  
  #daily flux rates
  wcr.sapday[[i]]<-aggregate(wcr.sap[ind,], by=list(wcr.ts$DOY[ind]), FUN='mean')[2:(ncol(wcr.sap))]
  syv.sapday[[i]]<-aggregate(syv.sap[ind,], by=list(syv.ts$DOY[ind]), FUN='mean')[2:(ncol(syv.sap))]
  
  #30min flows
  wcr.meg[[i]]<-wcr.mega[wcr.ts$YEAR==yearlist[i],]
  syv.meg[[i]]<-syv.mega[syv.ts$YEAR==yearlist[i],]
  
  #Timestamp
  ts.list[[i]]<-syv.ts[syv.ts$YEAR==yearlist[i],]
  
}

par(mfrow=c(1,2))
wcr.sapdays<-rbind(wcr.sapday[[1]], wcr.sapday[[2]], wcr.sapday[[3]])
plot(rowMeans(wcr.sapdays, na.rm=TRUE))
points(rowMeans(wcr.sapdays, na.rm=TRUE)[wcr.drytimes]~wcr.drytimes, col='red')


syv.sapdays<-rbind(syv.sapday[[1]], syv.sapday[[2]], syv.sapday[[3]])
plot(rowMeans(syv.sapdays, na.rm=TRUE))
points(rowMeans(syv.sapdays, na.rm=TRUE)[syv.drytimes]~syv.drytimes, col='red')



#idea: make a 'dryness score' and color code timeseries by this
#Definitely: pick x normal days and x dry days, plot together to show lack of difference

par(mfrow=c(2,2))
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

syv.sap.f<-rowSums(syv.mega)
drysyv<-cbind(syv.twr, syv.sap.f)[dryind,]
randsyv<-cbind(syv.twr, syv.sap.f)[randind,]

plot(syv.sap.f~HOUR, data=randsyv, ylim=c(0, 600), pch=19)
points(syv.sap.f~HOUR, data=drysyv, col='red', pch='*')

#neater agg plots

dryhr<-aggregate(drysyv, by=list(drysyv$HOUR), FUN='mean', na.rm=TRUE);randhr<-aggregate(randsyv, by=list(randsyv$HOUR), FUN='mean', na.rm=TRUE)

plot(syv.sap.f~HOUR, data=randhr, ylim=c(0, 600), pch=19)
points(syv.sap.f~HOUR, data=dryhr, col='red', pch=9)

#Do it all again for WCR. Functionalize  later if desired

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

wcr.sap.f<-rowSums(wcr.mega)
drywcr<-cbind(wcr.twr, wcr.sap.f)[dryind,]
randwcr<-cbind(wcr.twr, wcr.sap.f)[randind,]

plot(wcr.sap.f~HOUR, data=randwcr, ylim=c(0, 700), pch=19)
points(wcr.sap.f~HOUR, data=drywcr, col='red', pch='*')

#neater agg plots

dryhr<-aggregate(drywcr, by=list(drywcr$HOUR), FUN='mean', na.rm=TRUE);randhr<-aggregate(randwcr, by=list(randwcr$HOUR), FUN='mean', na.rm=TRUE)

plot(wcr.sap.f~HOUR, data=randhr, ylim=c(0, 600), pch=19)
points(wcr.sap.f~HOUR, data=dryhr, col='red', pch=9)



#Figure out whats going on with PET
#try pet minus sap maybe

dryseas<-which(syv.twr.d$DOY%in%c(144:259))
deficit<-(syv.pet.d$x) - (syv.twr.d$LE_1)
plot(rowMeans(syv.sapdays, na.rm=TRUE)[dryseas]~deficit[dryseas])
plot(rowMeans(syv.sapdays, na.rm=TRUE)[dryseas]~syv.pet.d$x[dryseas])

#"What would sf be without biotic control in response to water stress"
# Some part of the surfac resistance is mesophyll, trunk, fully open stomata, etc. 
# That number would probably be the 90th percentile or whatever of conductance


# sketch.df<-cbind(syv.twr.d, rowMeans(syv.sapdays))
# library(esquisse)
# esquisser()


