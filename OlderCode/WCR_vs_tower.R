start=190
end=230
#Tower
tower.raw.wcr<-read.csv('WCR_TowerData_2015.csv',skip=1, header=TRUE)
tower.names.wcr<-colnames(read.csv('WCR_TowerData_2015.csv'))
names(tower.raw.wcr)<-tower.names.wcr

tower.match.wcr<-tower.raw.wcr[tower.raw.wcr$DTIME>=start & tower.raw.wcr$DTIME<end,]
tower.match.wcr<-tower.match.wcr[1:(nrow(tower.match.wcr)-1),]
#Ustar filter
tower.match.wcr[tower.match.wcr$UST<0.2,6:45]<-(-9999)
tower.match.wcr[tower.match.wcr==-9999]<-NA
#tower.match.wcr$VPD[tower.match.wcr$VPD==0]<-NA


#night<-which(tower.match.wcr$PAR==0)

#tower.match.wcr[night,]<-NA

#sap
source('SapProcess_simple_WCR.R')
rm(list=setdiff(ls(), c("tower.match.syv", "sap.match.syv","DAT.SYV", 
                        "tower.match.wcr", "sap.match.wcr", "DAT.WCR", "start","end")))


sap.match.wcr<-DAT.WCR[DAT.WCR$DecDay>=start & DAT.WCR$DecDay<end,]

sample.index<-seq(from=1, to=nrow(sap.match.wcr), by=6)
sap.match.wcr<-sap.match.wcr[sample.index,]
#sap.match.wcr<-sap.match.wcr[1:(nrow(sap.match.wcr)-1),]

sap.match.wcr$colind<-'black'
sap.match.wcr$colind[sap.match.wcr$Dectime<14 & sap.match.wcr$Dectime>=7]<-"green"
sap.match.wcr$colind[sap.match.wcr$Dectime>=14 & sap.match.wcr$Dectime<22]<-"orange"

#sap.match.wcr[is.na(sap.match.wcr)]<-0

par(mfrow=c(1,1))
strt<-481
numrg<-strt:(strt+47)

par(mfrow=c(2,2))
for(i in 2:5){
  plot(sap.match.wcr[,(i+2)]~tower.match.wcr$SWC1, pch='*',
       col=sap.match.wcr$colind, ylab="Sapflux (m/s)", xlab="SWC", main=paste("WCR",i), ylim=c(0,5e-05))
}

seq(from=1, to=nrow(sap.match.wcr), by=48)

