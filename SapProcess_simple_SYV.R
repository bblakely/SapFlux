DAT1=read.csv('SYV_SAP_Processed.csv')
DAT2=read.csv('WCR_SAP_Processed.csv')

##Calculate flux

DAT=DAT1

DOYs=unique(DAT$DOY)
nDays=length(DOYs)
Sensornames= names(DAT[7:26])

nSensors=length(Sensornames)
DAT$Dectime=(DAT$H)+((DAT$M)/60)
DAT$DecDay=(DAT$DOY)+(DAT$Dectime/24)

nObs<-nrow(DAT)

Sens<-DAT[7:26]

Sensormaxes=as.data.frame(matrix(data=NaN,nrow=nDays, ncol=nSensors))
names(Sensormaxes)=Sensornames

for (v in 1:nSensors){
  for (i in 1:nDays){
  SensCol<-Sens[,v]
  Sensormaxes[i,v]<-max(SensCol[DAT$DOY==DOYs[i]], na.rm=TRUE)
  }
}
  
Sensormaxes[Sensormaxes<0]<-NaN

#Clear out maxes (i.e. days) with maxes 2 sd above the mean that are likely to be incomplete
maxmeans=colMeans(Sensormaxes, na.rm=TRUE)
maxsd=apply(Sensormaxes, 2, sd, na.rm=TRUE)

Smaxtest<-Sensormaxes
for (o in 1:nSensors){
   index<-which(Smaxtest[,o]<((maxmeans[o])-(2*maxsd[o])))
   print(length(index))
   Smaxtest[index,o]<-NaN
}

Sensormaxes<-Smaxtest

#Make dataframe of daily maxes
Daymaxes<-as.data.frame(matrix(data=NaN,nrow=nObs, ncol=nSensors))
Daymax.names<-rep(NaN, nSensors)
for (v in 1:nSensors){
  print(v)
  Daymaxvec<-rep(NaN, nObs)
  for (i in 1:nDays){
  Daymaxvec[DAT$DOY==DOYs[i]]<-Sensormaxes[i,v]    
  }
  Daymaxes[,v]<-Daymaxvec 
  Daymax.names[v]<- paste(Sensornames[v],"max", sep='_')
}
colnames(Daymaxes)<-Daymax.names

####Flux calculations####
K<-as.data.frame(matrix(data=NaN,nrow=nObs, ncol=nSensors))
K.names<-rep(NaN, nSensors)
Flux<-as.data.frame(matrix(data=NaN,nrow=nObs, ncol=nSensors))
Flux.names<-rep(NaN, nSensors)
for(v in 1:nSensors){
  K[,v]<-((Daymaxes[,v]-Sens[,v])/Sens[,v])
  Flux[,v]<-0.000119*(K[,v])^1.231
  
  K.names[v]<-paste(Sensornames[v], "K",sep='_')
  Flux.names[v]<-paste(Sensornames[v], "Flux",sep='')                   
}

colnames(K)<-K.names
colnames(Flux)<-Flux.names

###### Specify Site #####

DAT.SYV<-cbind(DAT[27:28],Flux)

DATcore=DAT.SYV

##### Clear high early morning sapflux values. Sylvania only.####
GoodMornings<-DATcore
for (g in 1:nSensors){
  index=which(GoodMornings$Dectime<6 & GoodMornings[,g+2] > 0.000007)
GoodMornings[index,g+2]<-NaN
}

DATcore<-GoodMornings

####check for outliers####
for (p in 1:5){ 
  par(mfrow=c(2,2))
  for (i in 1:4){
    plot(DATcore$Dectime,DATcore[,(p*4-4+i)+2], pch='.', ylim=c(0,0.0001), main=c(p*4-4+i))
    abline(h=0.000007,col='red')
    abline(v=6, col='blue')
    print((p*4-4+i)+2)
  }
}

#### Corrections. Sylvania only####
DATcore$S6Flux[DATcore$S6Flux>0.00002]<-NaN #there are a few really really high values on this sensor for some reason.
DATcore$S7Flux[DATcore$S7Flux>0.000085]<-NaN
DATcore$S5Flux[DATcore$S5Flux>0.00005]<-NaN
DATcore$S11Flux[DATcore$S5Flux>0.000018]<-NaN

####Pivot table for diel cycle####
DataPiv=DATcore
#DataPiv=DATcore
names(DataPiv)<-tolower(names(DataPiv))
library(reshape2)
VoltPrePiv=melt(DataPiv,id='dectime',measure.vars=c('s1flux','s2flux', 's3flux','s4flux','s5flux',
                                                    's6flux','s7flux','s8flux','s9flux','s10flux',
                                                    's11flux','s12flux','s13flux','s14flux','s15flux',
                                                    's16flux','s17flux','s18flux','s19flux','s20flux'),na.rm=TRUE)

SYV.VoltPivot=dcast(VoltPrePiv,dectime ~ variable, mean)


#### start plotting####

par(mfrow=c(1,1))
plot(SYV.VoltPivot$dectime,SYV.VoltPivot$s1flux,type='l',ylim=c(0,7e-05),col='white', xlab="Hour", ylab="Flux")

###### THE FOLLOWING PLOT COMMANDS ARE FOR SYLVANIA #####

par(mfrow=c(1,1))
plot(SYV.VoltPivot$dectime,SYV.VoltPivot$s1flux,type='l',ylim=c(0,7e-05),col='white', xlab="Hour", ylab="Flux")
#Hemlock
##Dominant
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s1flux, type='l',col='light green',lwd=2)
##Codominant
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s2flux, type='l',col='green',lwd=2)
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s20flux, type='l',col='green',lwd=2)
##Intermediate
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s8flux, type='l',col='forest green',lwd=2)
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s10flux, type='l',col='forest green',lwd=2)
#lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s11flux, type='l',col='forest green',lwd=2)
#lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s19flux, type='l',col='forest green',lwd=2) #this one is weird; bad data quailty, probably unfixable.
##Suppressed
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s3flux, type='l',col='dark green',lwd=2)

#Birch
##Dominant
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s7flux, type='l',col='tan 4',lwd=2) #A little high at night
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s18flux, type='l',col='tan 4',lwd=2) #slightly early

#Hophornbeam
##Codiminant
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s6flux, type='l',col='red',lwd=2) #this one is weird but I fixed it
##Intermediate
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s5flux, type='l',col='dark red',lwd=2) #A little high at night

#Sugar maple
##Dominant
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s9flux, type='l',col='gold',lwd=2)
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s13flux, type='l',col=' gold',lwd=2)
##Codominant
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s4flux, type='l',col='orange',lwd=2)
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s14flux, type='l',col=' orange',lwd=2)
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s15flux, type='l',col=' orange',lwd=2)
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s17flux, type='l',col=' orange',lwd=2)
##Intermediate
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s16flux, type='l',col='dark orange',lwd=2)
#Suppressed
lines(SYV.VoltPivot$dectime,SYV.VoltPivot$s12flux, type='l',col='dark orange 4',lwd=2)

#Combine all to get mean flux
Flux.avg.syv<-rowMeans(SYV.VoltPivot[,2:21], na.rm=TRUE)
lines(SYV.VoltPivot$dectime, Flux.avg.syv, lwd=4)
plot(SYV.VoltPivot$dectime, Flux.avg.syv, type='l',lwd=3)

#When does site-level flux peak?
SYV.VoltPivot$dectime[which(Flux.avg.syv==max(Flux.avg.syv, na.rm=TRUE))]


#### Flux to Flow ####
#Sapwood allomery
WCR.DBHs<-c(35.8,38.5,5.4,14.1,27.5,31.1,14.5,7.4,28.2,31.4,7.1,14.5,21.9,5.3)
SYV.DBHs<-c(58.1,35.4,13.1,20.1,16.8,13.3,54.5,19.2,33.3,28.9,27,6.3,83.3,
            22.8,20.8,17.8,27.3,71.8,22.5,31.5)
  
  
WoodArea.SYV=3.14*((0.5*SYV.DBHs)^2)
SWcm.SYV=0.54*WoodArea.SYV #.54 is the average of ratio of DBH to SW in trees I've measured. Will be improved.
SW.SYV<-SWcm.SYV/10000

#Total sap flux

DayFlux.SYV<-as.data.frame(matrix(ncol=nSensors, nrow=nrow(SYV.VoltPivot)))
for (i in 1:nSensors){
  DayFlux.SYV[,i]<-(SYV.VoltPivot[,i+1])*300*SW.SYV[i]*1000#300s/5min,1000L/m^3
}

Dayfluxcore.SYV<-cbind(DayFlux.SYV, SYV.VoltPivot$dectime)
colnames(Dayfluxcore.SYV)<-c(Sensornames, 'dectime')

#Sums of entire day
DayTotals.SYV=colSums(DayFlux.SYV[1:nSensors], na.rm=TRUE)

names(DayTotals.SYV)<-Sensornames

Allflow.SYV<-sum(DayTotals.SYV)
Allbasal.SYV<-sum(WoodArea.SYV)
Flow.per.basal.SYV<-Allflow.SYV/Allbasal.SYV
Flow.per.basal.SYV


#### Summary Plots ####

#Dataframe for plotting
##For Willow Creek
colors=c('yellow','yellow','dark red','brown','orange','orange','red','gold',
         'forest green','forest green','gold','dark orange','dark orange','dark red')
CC=c('C','C','S','S','C','C','I','S','C','C','S','I','I','S')
Day.df<-data.frame(DayTotals,DBHs, CC, Sensornames, colors)

##For Sylvania
colors=c('light green','green','dark green','orange','dark red','red','tan4','forest green',
         'gold','forest green','forest green','brown','gold','orange','orange','dark orange','orange',
         'tan4','forest green','green')
Day.df<-data.frame(DayTotals,DBHs, Sensornames, colors)


#Overall Mean flux
#sensormeansSYV=colMeans(DATcore[3:22], na.rm=TRUE)
sensormeansWCR=colMeans(DATcore[3:16], na.rm=TRUE)

sensormeans=sensormeansWCR #Change as appropriate

index=order(sensormeans)
barplot((sensormeans[index]), col=as.character(colors[index]), main='Mean Sap Flux (SYV)', ylab='Flux (m/s)')


#Line plots of flux
plot(Dayfluxcore$dectime,Dayfluxcore$S1, type='l', col='white', ylab='L/5min', xlab='Time', main='Sylvania Flow')

#WCR: for (p in 1:nSensors)
for (p in c(1:10,12:18,20)){
  lines(Dayfluxcore$dectime,Dayfluxcore[,p+1], col=colors[p], lwd='2')
}

# in order of sapflux, coded by species
Day.df<-Day.df[with(Day.df,order(DayTotals)),]
barplot(Day.df$DayTotals, col=as.character(Day.df$colors),names.arg=rownames(Day.df),ylab='Total daily volume (L)', main='Sylvania')

#In order of DBH
Day.df<-Day.df[with(Day.df,order(DBHs)),]
barplot(Day.df$DayTotals, col=as.character(Day.df$colors), names.arg=rownames(Day.df), ylab='Total daily volume (L)',xlab=' -> Tree DBH increasing ->', main='Sylvania')


#Flux
SYV.flux<-colMeans(DAT.SYV[3:22], na.rm=TRUE)
names(SYV.flux)<-colnames(DAT.SYV[3:22])

WCR.flux<-colMeans(DAT.WCR[3:16], na.rm=TRUE)
names(WCR.flux)<-colnames(DAT.WCR[3:16])

barplot(c(mean(WCR.flux),mean(SYV.flux)), main='Flux', col=c('orange','forest green'), names.arg=(c("WCR","SYV")))

#Flow per tree
SYV.pertree<-Allflow.SYV/20
WCR.pertree<-Allflow.WCR/14

barplot(c(WCR.pertree,SYV.pertree), main='Flow Per Tree', col=c('orange','forest green'), names.arg=(c("WCR","SYV")))

#Flow per basal area
barplot(c(Flow.per.basal.WCR, Flow.per.basal.SYV),main='Flow Per Basal Area', col=c('orange','forest green'), names.arg=(c("WCR","SYV")))

#Flow per sapwood
Allsap.SYV<-sum(SWcm.SYV)
Allsap.WCR<-sum(SWcm.WCR)
SYV.persap<-Allflow.SYV/Allsap.SYV
WCR.persap<-Allflow.WCR/Allsap.WCR

barplot(c(WCR.persap, SYV.persap),main='Flow Per Sapwood Area', col=c('orange','forest green'), names.arg=(c("WCR","SYV")))

#peak time
FluxPeaks.WCR<-apply(Dayfluxcore.WCR,2,max, na.rm=TRUE)
FluxPeaks.SYV<-apply(Dayfluxcore.SYV,2,max, na.rm=TRUE)

Peak.index.SYV=rep(NaN, nSensors)
for (v in 1:nSensors){
  Peak.index.SYV[v]<-which(Dayfluxcore.SYV[v]==FluxPeaks.SYV[v])
}

Peak.dectime.SYV<-Dayfluxcore.SYV$dectime[Peak.index.SYV]

Peak.SYV<-mean(Peak.dectime.SYV, na.rm=TRUE)
Peak.WCR<-mean(Peak.dectime.WCR, na.rm=TRUE)

# For getting solar peak
library('insol')
nmins<-1440
JulTab<-matrix(nrow=365,ncol=nmins)
PosTab<-array(dim=c(nmins,3,365))
angles<-array(dim=c(nmins,2,365))
SUN<-array(dim=c(nmins,2,365))
insol<-matrix(nrow=365,ncol=nmins)
for (i in 1:365){
  JulTab[i,]<-seq(from=(i-0.5), to=(i+0.5), length.out=nmins)
  PosTab[,,i]<-sunvector(jd=JulTab[i,], latitude=46, longitude=-89.65,timezone=-6)
  angles[,,i]<-sunpos(PosTab[,,i])
  SUN[,,i]<-insolation(zenith=angles[,2,i],jd=JulTab[i,],height=200,visibility=10,RH=50,tempK=280,O3=.003,alphag=0)
  insol[i,]<-SUN[,1,i]+SUN[,2,i]
}

summerday<-insol[200,]
Minpeak<-which(summerday==max(summerday))
Peak.Rn<-Minpeak/60
