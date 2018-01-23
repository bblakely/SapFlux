
#Date range
start<-190  #First day you want
end<-260    #Last day you want
site<-'WCR'
year<-2015
syv.startcol<-7 #First column with sapflux data at SYV
wcr.startcol<-6 #First column with sapflux data at WCR

#Sapflux
DAT1=read.csv('SYV_SAP_Processed.csv')
DAT2=read.csv('WCR_SAP_Processed.csv')
if (site=='WCR'){
  DAT=DAT2
  fc<-startcol<-wcr.startcol} else if (site=="SYV") {
  DAT=DAT1
  fc<-startcol<-syv.startcol}

lc<-endcol<-ncol(DAT)

DOYs=unique(DAT$DOY)
nDays=length(DOYs)
Sensornames= names(DAT[fc:lc])

nSensors=length(Sensornames)
DAT$Dectime=(DAT$H)+((DAT$M)/60)
DAT$DecDay=(DAT$DOY)+(DAT$Dectime/24)
nObs<-nrow(DAT)


T1<-read.csv('WCR_TowerData_2015.csv',skip=1, header=TRUE)
T1<-T1[2:nrow(T1),] #For some reason WCR has an extra row
T2<-read.csv('SYV_TowerData_2015.csv', skip=1,header=TRUE)

if (site=='WCR'){
  tower.raw<-T1
  namefile<-'WCR_TowerData_2015.csv'} else if (site=="SYV") {
  tower.raw<-T2
  namefile<-'SYV_TowerData_2015.csv'}

tower.names<-colnames(read.csv(namefile))  
#This makes the names variables not units; 
#I don't do this above becasue is causes R to read some parts of the data file as factors
names(tower.raw)<-tower.names

tower.match<-tower.raw[tower.raw$DTIME>=start & tower.raw$DTIME<=end,]
#tower.match<-tower.match[1:(nrow(tower.match)-1),]
#Ustar filter
#tower.match[tower.match$UST<0.2,6:45]<-(-9999)
tower.match[tower.match==-9999]<-NA

#Align

sap.match<-DAT[DAT$DecDay>=start & DAT$DecDay<=end,]
sample.index<-seq(from=1, to=nrow(sap.match), by=6)
sap.match<-sap.match[sample.index,]

sap.match<-sap.match[1:nrow(tower.match),]


#Combine
DATA<-cbind(tower.match, sap.match)
DATA$PLOT<-rep(2, nrow(DATA))
DATA$YEAR<-rep(year, nrow(DATA))



#Check
if(length(which(abs(round(DATA$DecDay, 2)-DATA$DTIME)>5e-02))>1){stop('Days do not line up!')}


#Subset
Vars<-c('PLOT','YEAR','DOY', 'HRMIN', 'VPD','PAR',Sensornames)
bl.dat<-subset(DATA, select=Vars)
writename<-paste(site, "_BL_Test.csv", sep='')
write.csv(bl.dat, writename, row.names=FALSE)
