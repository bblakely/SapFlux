


SYV_FullRaw<-read.csv("AMF_US-Syv_BASE_HH_12-5.csv", skip=2) #read.csv('SYV_Tower_2015_2017.csv')
  #read.csv('SYV_Tower_2015_2017.csv')
  #read.csv("AMF_US-Syv_BASE_HH_12-5.csv", skip=2) #read.csv('SYV_Tower_2015_2017.csv')
WCR_FullRaw<-read.csv("AMF_US-Wcr_BASE_HH_18-5.csv", skip=2)#'WCR_Tower_2015_2017.csv')
  #read.csv('WCR_Tower_2015_2017.csv')
  #read.csv("AMF_US-Wcr_BASE_HH_18-5.csv", skip=2)#'WCR_Tower_2015_2017.csv')

timestamp<-function(dat){
timeconv<-strptime(as.character(dat$TIMESTAMP_START), format="%Y%m%d%H%M")
ts<-data.frame(matrix(data=NA,nrow=length(timeconv), ncol=8)); colnames(ts)<-c('DTIME','DOY','MIN','HOUR','HRMIN','DAY','MONTH','YEAR')
ts$YEAR<-as.numeric(format(timeconv,'%Y'));ts$DOY<-as.numeric(format(timeconv,'%j'));ts$HOUR<-as.numeric(format(timeconv,'%H'));ts$MIN<-as.numeric(format(timeconv,'%M'))
ts$HRMIN<-as.numeric(format(timeconv,'%H%M'));ts$DAY<-as.numeric(format(timeconv,'%d'));ts$MONTH<-as.numeric(format(timeconv,'%m'))
ts$DTIME<-ts$DOY+ts$HOUR/24+ts$MIN/1440
dat.out<-cbind(ts, dat)
return(dat.out)
}

WCR_FullRaw.ts<-timestamp(dat=WCR_FullRaw)#WCR_FullRaw
SYV_FullRaw.ts<-timestamp(dat=SYV_FullRaw)#SYV_FullRaw

#Fucking factor
#WCR_FullRaw.ts$SC_PI_F_1_1_1<-as.numeric(levels(WCR_FullRaw.ts$SC_PI_F_1_1_1))[WCR_FullRaw.ts$SC_PI_F_1_1_1] 

SYV_notfill<-SYV_FullRaw.ts[,colMeans(SYV_FullRaw.ts)!=-9999]
WCR_notfill<-WCR_FullRaw.ts[,colMeans(WCR_FullRaw.ts, na.rm=TRUE)!=-9999]

shared<-colnames(WCR_notfill)[which(colnames(WCR_notfill)%in%colnames(SYV_notfill))]
wcr.orphans<-colnames(WCR_notfill)[!colnames(WCR_notfill)%in%shared]
syv.orphans<-colnames(SYV_notfill)[!colnames(SYV_notfill)%in%shared]

#Renaming some SYV to best match WCR
syv.rn<-SYV_notfill
colnames(syv.rn)[which(colnames(syv.rn)=="VPD_PI_F_1_1_1")]<-"VPD_PI_1_1_1"
colnames(syv.rn)[which(colnames(syv.rn)=="SW_IN_1_1_1")]<-"SW_IN"
colnames(syv.rn)[which(colnames(syv.rn)=="SW_OUT_1_1_1")]<-"SW_OUT"
colnames(syv.rn)[which(colnames(syv.rn)=="LW_IN_1_1_1")]<-"LW_IN"
colnames(syv.rn)[which(colnames(syv.rn)=="LW_OUT_1_1_1")]<-"LW_OUT"

shared.rn<-colnames(WCR_notfill)[which(colnames(WCR_notfill)%in%colnames(syv.rn))]

#Tower files with same variables for both sites
wcr.twr.std<-WCR_notfill[,colnames(WCR_notfill)%in%shared.rn]
syv.twr.std<-syv.rn[,colnames(syv.rn)%in%shared.rn]

#Tower files with the stuff unique to each site
wcr.twr.extra<-WCR_notfill[,!colnames(WCR_notfill)%in%shared.rn]
syv.twr.extra<-syv.rn[,!colnames(syv.rn)%in%shared.rn]

#Choose variables I'm likely to actually use
usevars<-c(1:9,16:41)
usenames<-c("H_1_1_1","LE_1_1_1", "SH_PI_F_1_1_1","SLE_PI_F_1_1_1",
            "WD_1_1_1","WS_1_1_1,", "USTAR_1_1_1","PA_1_1_1","TA_1_1_1",  
            "VPD_PI_1_1_1","SWC_1_1_1","SWC_1_2_1", "TS_1_1_1", "TS_1_2_1", "TS_PI_F_1_1_1",  
            "NETRAD_1_1_1","PPFD_IN_PI_F_1_1_1", "SW_IN" , "SW_OUT" ,"LW_IN", "LW_OUT",
            "P_PI_F", "NEE_PI","NEE_PI_F", "RECO_PI_F" ,"GPP_PI_F", "RH_1_1_1")

wcr.twr.std<-wcr.twr.std[c(1:9,which(colnames(wcr.twr.std)%in%usenames))]
syv.twr.std<-syv.twr.std[c(1:9,which(colnames(syv.twr.std)%in%usenames))]

#rearrange wcr columns to match syv
wcr.twr.std<-wcr.twr.std[,match(colnames(syv.twr.std),colnames(wcr.twr.std))]

#Clip off annoying ameriflux positioning tags (leave a _1 to identify things with a position)
colnames(wcr.twr.std)<-sub("_1_1","", colnames(wcr.twr.std))
colnames(syv.twr.std)<-sub("_1_1","", colnames(syv.twr.std))


write.csv(syv.twr.std, "SYV_Tower.csv")
write.csv(wcr.twr.std, "WCR_Tower.csv")

write.csv(syv.twr.extra, "SYV_Tower_ex.csv")
write.csv(wcr.twr.extra, "WCR_Tower_ex.csv")

wcr.twr.2015<-wcr.twr.std[wcr.twr.std$YEAR==2015,]
syv.twr.2015<-syv.twr.std[syv.twr.std$YEAR==2015,]

wcr.twr.2016<-wcr.twr.std[wcr.twr.std$YEAR==2016,]
syv.twr.2016<-syv.twr.std[syv.twr.std$YEAR==2016,]

rm(SYV_FullRaw, SYV_FullRaw.ts,WCR_FullRaw,WCR_FullRaw.ts, SYV_notfill, WCR_notfill, SYV_rn, WCR_rn, syv.twr.extra,wcr.twr.extra)
