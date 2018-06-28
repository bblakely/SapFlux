#Read in ALL the standardized data. Currently only for 2016.

#Data for measured trees
source("Prepare_treedata.R")
rm('syv.calc.rp','wcr.calc.rp') #These sheets show breakdown of RP calculations; not used
syv.tree$CC[c(1,5,11,14,17)]<-substr(syv.tree$CC[c(1,5,11,14,17)],1,1)

print('Tree data ready')

#Data for surveyed trees
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

#Sapflow
#non-gapfilled
wcr.raw<-read.csv('WCR_2016_SAPFLUX.csv')
wcr.ts<-wcr.raw[1:4]

#gapfilled
wcr.gapfill<- read.csv('WCR_gapfill.csv')[2:15]
wcr.gap<-data.frame(wcr.gapfill)
wcr.gap[wcr.gap<0]<-0
wcr.names<-colnames(read.csv('WCR_2016_SAPFLUX.csv'))
wcr.ts<-read.csv('WCR_2016_SAPFLUX.csv')[1:4]
colnames(wcr.gap)<-wcr.names[5:18]

syv.gapfill<- read.csv('SYV_gapfill.csv')[2:21]
syv.gap<-data.frame(syv.gapfill)
syv.gap[syv.gap<0]<-0
syv.names<-colnames(read.csv('SYV_2016_SAPFLUX.csv'))
syv.ts<-(read.csv('SYV_2016_SAPFLUX.csv'))[1:4]
colnames(syv.gap)<-syv.names[5:24]

rm("wcr.gapfill", "syv.gapfill")

print('Sapflux data ready')


#Now apogee; easiest to just call a script
source('Prepare_Apogee.R')
rm("wcr.ap.2016","wcr.ap.2016.dat","wcr.ap.30", "wcr.clean")

print('ST data ready')

#Now tower. Script for this one too
source('Prepare_TowerData.R')
rm("SYV_FullRaw","SYV_notfill","syv.rn","syv.twr.extra","WCR_FullRaw","WCR_notfill",
   "shared","shared.rn","syv.orphans","usevars","wcr.orphans", "wcr.twr.extra")
rm('wcr.twr.std', 'syv.twr.std') #Three-year timeseries. Not currently using.

syv.twr.2016[syv.twr.2016==(-9999)]<-NA
wcr.twr.2016[wcr.twr.2016==(-9999)]<-NA

print('Tower data ready')


syv.master<-cbind(syv.ts$YEAR,syv.twr.2016[,7],syv.ap[,3:10],syv.gap,syv.twr.2016[,10:35])
colnames(syv.master)[1]<-'YEAR'

wcr.master<-cbind(wcr.ts$YEAR,wcr.twr.2016[,7],wcr.ap[,3:10],wcr.gap,wcr.twr.2016[,10:35])
colnames(wcr.master)[1]<-'YEAR'


