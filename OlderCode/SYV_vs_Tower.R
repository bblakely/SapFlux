start=190
end=230

tower.raw.syv<-read.csv('Syv_TowerData_2015.csv',skip=1, header=TRUE)
tower.names.syv<-colnames(read.csv('Syv_TowerData_2015.csv'))
names(tower.raw.syv)<-tower.names.syv

tower.match.syv<-tower.raw.syv[tower.raw.syv$DTIME>=start & tower.raw.syv$DTIME<end,]
#Ustar filter
#tower.match.syv[tower.match.syv$UST<0.2,6:45]<-(-9999)
#tower.match.syv[tower.match.syv$WD>330 | tower.match.syv$WD<90, 6:45]<-(-9999)
tower.match.syv[tower.match.syv==-9999]<-NA



source('SapProcess_simple_SYV.R')
rm(list=setdiff(ls(), c("tower.match.syv", "sap.match.syv","DAT.SYV", 
                        "tower.match.wcr", "sap.match.wcr", "DAT.WCR", "start","end")))

sap.match.syv<-DAT.SYV[DAT.SYV$DecDay>=start & DAT.SYV$DecDay<end,]

sample.index<-seq(from=1, to=nrow(sap.match.syv), by=6)
sap.match.syv<-sap.match.syv[sample.index,]
sap.match.syv<-sap.match.syv[1:(nrow(sap.match.syv)-1),]

sap.match.syv$colind<-'black'
 sap.match.syv$colind[sap.match.syv$Dectime<14 & sap.match.syv$Dectime>=6]<-"green"
 sap.match.syv$colind[sap.match.syv$Dectime>=14 & sap.match.syv$Dectime<22]<-"orange"

#sap.match.syv[is.na(sap.match.syv)]<-0

seq(from=1, to=nrow(sap.match.syv), by=48)

strt<-481
numrg<-strt:(strt+47)

#Treesp=c("TSCA","TSCA","TSCA","TSCA","OSVI","OSVI","BEAL", "TSCA","ACSA","TSCA","TSCA",
#         "12", "ACSA","ACSA","ACSA","ACSA", "ACSA","18","TSCA","TSCA")

par(mfrow=c(2,2))
for(i in c(2:5)){
  plot(sap.match.syv[,i+2]~tower.match.syv$SWC1, pch='*',
       col=sap.match.syv$colind, ylab="Sapflux (m/s)", xlab="SWC", main=paste("SYV",i),
       ylim=c(0,5e-05))
}


