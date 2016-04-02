
DAT1=read.csv('SYV_SAP_Processed.csv')
DAT2=read.csv('WCR_SAP_Processed.csv')

##Calculate flux

DAT=DAT2

DOYs=unique(DAT$DOY)
nDays=length(DOYs)
Sensornames= names(DAT[6:19])

nSensors=length(Sensornames)
DAT$Dectime=(DAT$H)+((DAT$M)/60)
DAT$DecDay=(DAT$DOY)+(DAT$Dectime/24)

nObs<-nrow(DAT)

Sens<-DAT[6:19]

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
  Flux.names[v]<-paste(Sensornames[v], "Flux", sep='')                   
}

colnames(K)<-K.names
colnames(Flux)<-Flux.names

###### Specify Site #####

DAT.WCR<-cbind(DAT[20:21],Flux)

DATcore=DAT.WCR


####check for outliers####
for (p in 1:4){ 
  par(mfrow=c(2,2))
  for (i in 1:4){
    plot(DATcore$Dectime,DATcore[,(p*4-4+i)+2], pch='.', ylim=c(0,0.0001), main=c(p*4-4+i))
    abline(h=0.000007,col='red')
    abline(v=6, col='blue')
    print((p*4-4+i)+2)
  }
}


####Pivot table for diel cycle####
DataPiv=DATcore
#DataPiv=DATcore
names(DataPiv)<-tolower(names(DataPiv))
library(reshape2)
VoltPrePiv=melt(DataPiv,id='dectime',measure.vars=c('s1flux','s2flux', 's3flux','s4flux','s5flux',
                                                    's6flux','s7flux','s8flux','s9flux','s10flux',
                                                    's11flux','s12flux','s13flux','s14flux'),na.rm=TRUE)

WCR.VoltPivot=dcast(VoltPrePiv,dectime ~ variable, mean)

##Fix datalogger lag (logger is an hour behind real time)



#### start plotting####

par(mfrow=c(1,1))
plot(WCR.VoltPivot$dectime,WCR.VoltPivot$s1flux,type='l',ylim=c(0,7e-05),col='white', xlab="Hour", ylab="Flux")

####THE FOLLOWING PLOT COMMANDS ARE FOR WILLOW CREEK####
#Basswood
plot(WCR.VoltPivot$dectime,WCR.VoltPivot$s1flux,type='l',ylim=c(0,7e-05),col='white', xlab="Hour", ylab="Flux", main='Basswood')
legend(1,4e-05, legend=c('codominant','suppressed'), lwd=c(2,2), col=c('yellow','gold'))
##Codominant
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s1flux, type='l',col='yellow',lwd=2)
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s2flux, type='l',col='yellow',lwd=2)
##Suppressed
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s8flux, type='l',col='gold',lwd=2)
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s11flux, type='l',col='gold',lwd=2)

#Hophornbeam
plot(WCR.VoltPivot$dectime,WCR.VoltPivot$s1flux,type='l',ylim=c(0,7e-05),col='white', xlab="Hour", ylab="Flux", main='Hophornbeam')
legend(1,4e-05, legend=c('intermediate','suppressed'), lwd=c(2,2), col=c('red','dark red'))
##Intermediate
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s7flux, type='l',col='red',lwd=2)
##Suppressed
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s3flux, type='l',col='dark red',lwd=2)
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s14flux, type='l',col='dark red',lwd=2)

#Sugar Maple
plot(WCR.VoltPivot$dectime,WCR.VoltPivot$s1flux,type='l',ylim=c(0,7e-05),col='white', xlab="Hour", ylab="Flux", main='Sugar Maple')
legend(1,4e-05, legend=c('codominant','intermediate', 'suppressed'), lwd=c(2,2,2), col=c('orange','dark orange', 'brown'))

##Codominant
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s5flux, type='l',col='orange',lwd=2)
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s6flux, type='l',col='orange',lwd=2)
##Intermediate
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s12flux, type='l',col='dark orange',lwd=2)
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s13flux, type='l',col='dark orange',lwd=2)
##Suppressed
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s4flux, type='l',col='brown',lwd=2)

#Green Ash
plot(WCR.VoltPivot$dectime,WCR.VoltPivot$s1flux,type='l',ylim=c(0,7e-05),col='white', xlab="Hour", ylab="Flux", main='Green Ash')
legend(1,4e-05, legend='codominant', lwd=2, col='forest green')

lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s9flux, type='l',col='forest green',lwd=2)
lines(WCR.VoltPivot$dectime,WCR.VoltPivot$s10flux, type='l',col='forest green',lwd=2)


#### Flux to Flow ####
#Sapwood allomery
WCR.DBHs<-c(35.8,38.5,5.4,14.1,27.5,31.1,14.5,7.4,28.2,31.4,7.1,14.5,21.9,5.3)
SYV.DBHs<-c(58.1,35.4,13.1,20.1,16.8,13.3,54.5,19.2,33.3,28.9,27,6.3,83.3,
            22.8,20.8,17.8,27.3,71.8,22.5,31.5)


WoodArea.WCR=3.14*((0.5*WCR.DBHs)^2) #cm2
SWcm.WCR=0.54*WoodArea.WCR #.54 is the average of ratio of DBH to SW in trees I've measured. Will be improved.
SW.WCR<-SWcm.WCR/10000 #m2

#Total sap flux

DayFlux.WCR<-as.data.frame(matrix(ncol=nSensors, nrow=nrow(WCR.VoltPivot)))
  for (i in 1:nSensors){
  DayFlux.WCR[,i]<-(WCR.VoltPivot[,i+1])*300*SW.WCR[i]*1000 #300s/5min,1000L/m^3
}

Dayfluxcore.WCR<-cbind(DayFlux.WCR, WCR.VoltPivot$dectime)
colnames(Dayfluxcore.WCR)<-c(Sensornames, 'dectime')

#Sums of entire day
DayTotals.WCR=colSums(DayFlux.WCR[1:nSensors], na.rm=TRUE)
#DayTotals=colSums(DayFlux[16:29], na.rm=TRUE)
names(DayTotals.WCR)<-Sensornames

##Normalize by basal area
Allflow.WCR<-sum(DayTotals.WCR)
Allbasal.WCR<-sum(WoodArea.WCR)
Flow.per.basal.WCR<-Allflow.WCR/Allbasal.WCR
Flow.per.basal.WCR

#Peak time
FluxPeaks.WCR<-apply(Dayfluxcore.WCR,2,max, na.rm=TRUE)

Peak.index.WCR=rep(NaN, nSensors)
for (v in 1:nSensors){
  Peak.index.WCR[v]<-which(Dayfluxcore.WCR[v]==FluxPeaks.WCR[v])
}

Peak.dectime.WCR<-Dayfluxcore.WCR$dectime[Peak.index.WCR]

