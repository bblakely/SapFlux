SYV_FullRaw<-read.csv('SYV_Tower_2015_2017.csv')
WCR_FullRaw<-read.csv('WCR_Tower_2015_2017.csv')

#Fucking factor
WCR_FullRaw$SC_PI_F_1_1_1<-as.numeric(levels(WCR_FullRaw$SC_PI_F_1_1_1))[WCR_FullRaw$SC_PI_F_1_1_1] 

SYV_notfill<-SYV_FullRaw[,colMeans(SYV_FullRaw)!=-9999]
WCR_notfill<-WCR_FullRaw[,colMeans(WCR_FullRaw, na.rm=TRUE)!=-9999]

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

wcr.twr.std<-wcr.twr.std[,usevars]
syv.twr.std<-syv.twr.std[,usevars]

#Clip off annoying ameriflux positioning tags (leave a _1 to identify things with a position)
colnames(wcr.twr.std)<-sub("_1_1","", colnames(wcr.twr.std))
colnames(syv.twr.std)<-sub("_1_1","", colnames(syv.twr.std))


write.csv(syv.twr.std, "SYV_Tower.csv")
write.csv(wcr.twr.std, "WCR_Tower.csv")

write.csv(syv.twr.extra, "SYV_Tower_ex.csv")
write.csv(wcr.twr.extra, "WCR_Tower_ex.csv")


