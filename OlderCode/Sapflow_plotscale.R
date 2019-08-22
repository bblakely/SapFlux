source('SapFlow_Calc.R')
#rm(list=setdiff(ls(), c("syv.flux.sp", "wcr.flux.sp", 'syv.plot.sort', 'wcr.plot.sort')))
#source('Prepare_forestdata.R')

syv.forest.rad<-read.csv('SYV_FOREST_RAD.csv')
wcr.forest.rad<-read.csv('WCR_FOREST_RAD.csv')

#Fucking factors
syv.forest.rad$Multiplier<-as.numeric(levels(syv.forest.rad$Multiplier)[syv.forest.rad$Multiplier])
#wcr.forest.rad$Multiplier<-as.numeric(levels(wcr.forest.rad$Multiplier)[wcr.forest.rad$Multiplier])


#SYV
DAT<-syv.forest.rad

treemult<-(DAT$Multiplier*(DAT$SWA/10000))
treemult[297]<-(DAT$SWA[297])/10000

DAT$flow.mean<-NaN
DAT$flow.mean[DAT$SPP=='acsa']<-treemult[DAT$SPP=='acsa']*syv.flux.sp[syv.flux.sp$Group.1=='ACSA',2]
  #1.1e-05
DAT$flow.mean[DAT$SPP=='beal']<-treemult[DAT$SPP=='beal']*syv.flux.sp[syv.flux.sp$Group.1=='BEAL',2]
DAT$flow.mean[DAT$SPP=='osvi']<-treemult[DAT$SPP=='osvi']*syv.flux.sp[syv.flux.sp$Group.1=='OSVI',2]
DAT$flow.mean[DAT$SPP=='tsca']<-treemult[DAT$SPP=='tsca']*syv.flux.sp[syv.flux.sp$Group.1=='TSCA',2]
DAT$flow.mean[DAT$SPP=='uk']<-treemult[DAT$SPP=='uk']*mean(syv.flux.sp[1:3,2])

DAT$flow.mean<-DAT$flow.mean*86400*1000  #86400 seconds in a day, 1000L in 1 m3

syv.stand<-DAT


#WCR
DAT<-wcr.forest.rad

treemult<-(DAT$Multiplier*(DAT$SWA/10000))
treemult[297]<-(DAT$SWA[297])/10000

DAT$flow.mean<-NaN
DAT$flow.mean[DAT$SPP=='acsa']<-treemult[DAT$SPP=='acsa']*wcr.flux.sp[wcr.flux.sp$Group.1=='ACSA',2]
  #1.1e-05
DAT$flow.mean[DAT$SPP=='beal']<-treemult[DAT$SPP=='beal']*wcr.flux.sp[wcr.flux.sp$Group.1=='BEAL',2]
DAT$flow.mean[DAT$SPP=='osvi']<-treemult[DAT$SPP=='osvi']*wcr.flux.sp[wcr.flux.sp$Group.1=='OSVI',2]
  #9.36e-06
DAT$flow.mean[DAT$SPP=='tsca']<-treemult[DAT$SPP=='tsca']*wcr.flux.sp[wcr.flux.sp$Group.1=='TSCA',2]
DAT$flow.mean[DAT$SPP=='tiam']<-treemult[DAT$SPP=='tiam']*wcr.flux.sp[wcr.flux.sp$Group.1=='TIAM',2]
DAT$flow.mean[DAT$SPP=='frpe']<-treemult[DAT$SPP=='frpe']*wcr.flux.sp[wcr.flux.sp$Group.1=='FRPE',2]

DAT$flow.mean[DAT$SPP=='quru']<-treemult[DAT$SPP=='quru']*wcr.flux.sp[wcr.flux.sp$Group.1=='FRPE',2]  #Use ash becasue both are ring-porous

DAT$flow.mean[DAT$SPP=='uk']<-treemult[DAT$SPP=='uk']*mean(wcr.flux.sp[,2])

DAT$flow.mean<-DAT$flow.mean*86400*1000  #86400 seconds in a day, 1000L in 1 m3

wcr.stand<-DAT

#Play with data

#flow per basal area
sum(wcr.stand$flow.mean)/sum(wcr.stand$BA)
sum(syv.stand$flow.mean)/sum(syv.stand$BA)

#flow per sapwood area
sum(wcr.stand$flow.mean)/sum(wcr.stand$SWA)
sum(syv.stand$flow.mean)/sum(syv.stand$SWA)

#First conclusion: old growth trees are inefficient.
#25% less total flow
#39% less per basal area
#59% less per sapwood area


#64% difference in measured flux




