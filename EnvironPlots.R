
if(!exists('wcr.master')){
source('Prepare_Data.R')
source('Refine_TowerData.R')
source('Calc_Sapflow_Full.R')
}

peakgs<-which(wcr.master$DOY>167& wcr.master$DOY<244 & wcr.master$H%in%dayhr)

wcr.master.day<-aggregate(wcr.master[daygs,], by=list(wcr.master$DOY[daygs]), FUN='mean', na.rm=TRUE)
syv.master.day<-aggregate(syv.master[daygs,], by=list(syv.master$DOY[daygs]), FUN='mean', na.rm=TRUE)

#wcr.precip<-aggregate(wcr.master.day$P_PI_F, by=list(wcr.master.day$DOY), FUN='sum')

# cols<-rainbow(18)
# par(mfrow=c(2,3), mar=c(2,5,3,1.1))
# 
# ylab=expression("V"[s]~(g~m^-2~s^-1))
# 
# plot(wcr.master.day[,12]~wcr.master.day$SWC_1,
#      xlim=c(15,45), ylim=c(0,100), col='white',main="Soil moisture (%)",ylab=ylab, xlab="");grid()
# for(i in (1+which(wcr.tree$SPP=="ACSA"))){
# points(wcr.master.day[,i+10]~wcr.master.day$SWC_1, col=cols[i])
# }
# 
# plot(wcr.master.day[,12]~wcr.master.day$VPD_PI_1,
#      xlim=c(0,25), ylim=c(0,100), col='white',main="VPD (hPa)",ylab=ylab, xlab="");grid()
# for(i in (1+which(wcr.tree$SPP=="ACSA"))){
#   points(wcr.master.day[,i+10]~wcr.master.day$VPD_PI_1, col=cols[i])
# }
# 
# plot(wcr.master.day[,12]~wcr.master.day$NETRAD_1,
#      xlim=c(0,450), ylim=c(0,100), col='white',main=expression(bold("Net Rad."~(Wm^-2))),ylab=ylab, xlab="");grid()
# for(i in (1+which(wcr.tree$SPP=="ACSA"))){
#   points(wcr.master.day[,i+10]~wcr.master.day$NETRAD_1, col=cols[i])
# }
# 
# 
# 
# plot(syv.master.day[,12]~syv.master.day$SWC_1, xlim=c(15,45), ylim=c(0,100), col='white',ylab=ylab, xlab="");grid()
# for(i in (1+which(syv.tree$SPP=="ACSA"))){
#   points(syv.master.day[,i+10]~syv.master.day$SWC_1, col=cols[i])
# }
# 
# plot(syv.master.day[,12]~syv.master.day$VPD_PI_1, xlim=c(0,25), ylim=c(0,100), col='white',ylab=ylab, xlab="");grid()
# for(i in (1+which(syv.tree$SPP=="ACSA"))){
#   points(syv.master.day[,i+10]~syv.master.day$VPD_PI_1, col=cols[i])
# }
# 
# plot(syv.master.day[,12]~syv.master.day$NETRAD_1, xlim=c(0,450), ylim=c(0,100), col='white',ylab=ylab, xlab="");grid()
# for(i in (1+which(syv.tree$SPP=="ACSA"))){
#   points(syv.master.day[,i+10]~syv.master.day$NETRAD_1, col=cols[i])
# }
# 
# 
# 
# for(i in (1+which(syv.tree$SPP=="ACSA"))){
#   print(summary(lm(syv.master.day[,i+10]~syv.master.day$SWC_1)))
# }
# 
# for(i in (1+which(wcr.tree$SPP=="ACSA"))){
#   print(summary(lm(wcr.master.day[,i+10]~wcr.master.day$SWC_1)))
# }

#### and OSVIs
library(scales)
sp.wcr<-as.character(wcr.tree$SPP); sp.syv<-as.character(syv.tree$SPP)


par(mfrow=c(2,3), mar=c(2,5,3,1.1))
transp<-0.6
ylab=expression("V"[s]~(g~m^-2~s^-1))

plot(wcr.master.day[,12]~wcr.master.day$SWC_1,
     xlim=c(15,45), ylim=c(0,100), col='white',main="Soil moisture (%)",ylab=ylab, xlab="");grid()
for(i in (1+which(wcr.tree$SPP=="OSVI"| wcr.tree$SPP=="ACSA"))){
  if(wcr.tree$SPP[i-1]=="ACSA"){col='orange'}else(col<-"dark red")
  points(wcr.master.day[,i+10]~wcr.master.day$SWC_1, col=alpha(col, transp))
}

plot(wcr.master.day[,12]~wcr.master.day$VPD_PI_1,
     xlim=c(0,25), ylim=c(0,100), col='white',main="VPD (hPa)",ylab=ylab, xlab="");grid()
for(i in (1+which(wcr.tree$SPP=="OSVI"| wcr.tree$SPP=="ACSA"))){
  if(wcr.tree$SPP[i-1]=="ACSA"){col='orange'}else(col<-"dark red")
  points(wcr.master.day[,i+10]~wcr.master.day$VPD_PI_1, col=alpha(col, transp))
}

plot(wcr.master.day[,12]~wcr.master.day$NETRAD_1,
     xlim=c(0,450), ylim=c(0,100), col='white',main=expression(bold("Net Rad."~(Wm^-2))),ylab=ylab, xlab="");grid()
for(i in (1+which(wcr.tree$SPP=="OSVI"| wcr.tree$SPP=="ACSA"))){
  if(wcr.tree$SPP[i-1]=="ACSA"){col='orange'}else(col<-"dark red")
  points(wcr.master.day[,i+10]~wcr.master.day$NETRAD_1, col=alpha(col, transp))
}



plot(syv.master.day[,12]~syv.master.day$SWC_1, xlim=c(15,45), ylim=c(0,100), col='white',ylab=ylab, xlab="");grid()
for(i in (1+which(syv.tree$SPP=="OSVI"| syv.tree$SPP=="ACSA"))){
  if(syv.tree$SPP[i-1]=="ACSA"){col='orange'}else(col<-"dark red")
  points(syv.master.day[,i+10]~syv.master.day$SWC_1, col=alpha(col, transp))
}

plot(syv.master.day[,12]~syv.master.day$VPD_PI_1, xlim=c(0,25), ylim=c(0,100), col='white',ylab=ylab, xlab="");grid()
for(i in (1+which(syv.tree$SPP=="OSVI"| syv.tree$SPP=="ACSA"))){
  if(syv.tree$SPP[i-1]=="ACSA"){col='orange'}else(col<-"dark red")
  points(syv.master.day[,i+10]~syv.master.day$VPD_PI_1, col=alpha(col, transp))
}

plot(syv.master.day[,12]~syv.master.day$NETRAD_1, xlim=c(0,450), ylim=c(0,100), col='white',ylab=ylab, xlab="");grid()
for(i in (1+which(syv.tree$SPP=="OSVI"| syv.tree$SPP=="ACSA"))){
  if(syv.tree$SPP[i-1]=="ACSA"){col='orange'}else(col<-"dark red")
  points(syv.master.day[,i+10]~syv.master.day$NETRAD_1, col=alpha(col, transp))
}

sp.wcr<-as.character(wcr.tree$SPP); sp.syv<-as.character(syv.tree$SPP)

coefholder.syv<-matrix(nrow=length(which(syv.tree$SPP=="OSVI"| syv.tree$SPP=="ACSA"))+12, ncol=4)
for(i in (which(syv.tree$SPP=="OSVI"| syv.tree$SPP=="ACSA"))){
  #print(summary(lm(syv.master.day[,i+10]~syv.master.day$SWC_1)))
  coefholder.syv[i,c(1:2)]<-coefficients(lm(syv.master.day[,i+11]~syv.master.day$SWC_1))
  coefholder.syv[i,3]<-summary(lm(syv.master.day[,i+11]~syv.master.day$SWC_1))$coefficients[,4][2]
  coefholder.syv[i,4]<-sp.syv[i]
}

coefholder.wcr<-matrix(nrow=length(which(wcr.tree$SPP=="OSVI"| wcr.tree$SPP=="ACSA"))+12, ncol=4)
for(i in (which(wcr.tree$SPP=="OSVI"| wcr.tree$SPP=="ACSA"))){
  #print(summary(lm(wcr.master.day[,i+10]~wcr.master.day$SWC_1)))
  coefholder.wcr[i,c(1:2)]<-coefficients(lm(wcr.master.day[,i+11]~wcr.master.day$SWC_1))
  coefholder.wcr[i,3]<-summary(lm(wcr.master.day[,i+11]~wcr.master.day$SWC_1))$coefficients[,4][2]
  coefholder.wcr[i,4]<-sp.wcr[i]
}


coefholder.syv<-coefholder.syv[!is.na(coefholder.syv[,1]),]
coefholder.syv.print<-coefholder.syv[,2:4]; nsigs<-which(as.numeric(coefholder.syv.print[,2])>0.01)
coefholder.syv.print[nsigs]<-"n.s"

coefholder.wcr<-coefholder.wcr[!is.na(coefholder.wcr[,1]),]; 
coefholder.wcr.print<-coefholder.wcr[,2:4]; nsigs<-which(as.numeric(coefholder.wcr.print[,2])>0.01)
coefholder.wcr.print[nsigs]<-"n.s"

#Again with VPD/rad
sp.wcr<-as.character(wcr.tree$SPP); sp.syv<-as.character(syv.tree$SPP)

coefholder.syv.a<-matrix(nrow=length(which(syv.tree$SPP=="OSVI"| syv.tree$SPP=="ACSA"))+12, ncol=6)
for(i in (which(syv.tree$SPP=="OSVI"| syv.tree$SPP=="ACSA"))){
  #print(summary(lm(syv.master.day[,i+10]~syv.master.day$SWC_1)))
  coefholder.syv.a[i,c(1:4)]<-coefficients(lm(syv.master.day[,i+11]~syv.master.day$SWC_1+syv.master.day$VPD_PI_1+syv.master.day$NETRAD_1))
  coefholder.syv.a[i,5]<-summary(lm(syv.master.day[,i+11]~syv.master.day$SWC_1+syv.master.day$VPD_PI_1+syv.master.day$NETRAD_1))$coefficients[,4][2]
  coefholder.syv.a[i,6]<-sp.syv[i]
}


coefholder.syv.a<-coefholder.syv.a[!is.na(coefholder.syv.a[,1]),]
coefholder.syv.print.a<-coefholder.syv.a[,2:6]; nsigs<-which(as.numeric(coefholder.syv.print.a[,4])>0.01)
coefholder.syv.print.a[nsigs]<-"n.s"

coefholder.wcr.a<-matrix(nrow=length(which(wcr.tree$SPP=="OSVI"| wcr.tree$SPP=="ACSA"))+12, ncol=6)
for(i in (which(wcr.tree$SPP=="OSVI"| wcr.tree$SPP=="ACSA"))){
  #print(summary(lm(wcr.master.day[,i+10]~wcr.master.day$SWC_1)))
  coefholder.wcr.a[i,c(1:4)]<-coefficients(lm(wcr.master.day[,i+11]~wcr.master.day$SWC_1+wcr.master.day$VPD_PI_1+wcr.master.day$NETRAD_1))
  coefholder.wcr.a[i,5]<-summary(lm(wcr.master.day[,i+11]~wcr.master.day$SWC_1+wcr.master.day$VPD_PI_1+wcr.master.day$NETRAD_1))$coefficients[,4][2]
  coefholder.wcr.a[i,6]<-sp.wcr[i]
}


coefholder.wcr.a<-coefholder.wcr.a[!is.na(coefholder.wcr.a[,1]),]
coefholder.wcr.print.a<-coefholder.wcr.a[,2:6]; nsigs<-which(as.numeric(coefholder.wcr.print.a[,4])>0.01)
coefholder.wcr.print.a[nsigs]<-"n.s"


#stick those mofos together

soil.table.wcr<-data.frame(cbind(coefholder.wcr.print[,3], coefholder.wcr.print[,1], coefholder.wcr.print.a[,1])); colnames(soil.table.wcr)<-c("SPP", " T ~ Soil", "T ~ Soil + VPD + Rnet")
soil.table.syv<-data.frame(cbind(coefholder.syv.print[,3], coefholder.syv.print[,1], coefholder.syv.print.a[,1]));colnames(soil.table.syv)<-c("SPP", " T ~ Soil", "T ~ Soil + VPD + Rnet")

soil.table.wcr<-soil.table.wcr[order(soil.table.wcr$SPP),];soil.table.syv<-soil.table.syv[order(soil.table.syv$SPP),]
# ##All da trees
# 
# cols<-rainbow(25)
# par(mfrow=c(2,3), mar=c(2,5,3,1.1))
# 
# ylab=expression("J"[s]~(g~m^-2~s^-1))
# 
# plot(wcr.master.day[,12]~wcr.master.day$SWC_1,
#      xlim=c(15,45), ylim=c(0,100), col='white',main="Soil moisture (%)",ylab=ylab, xlab="");grid()
# for(i in c(1:14)){
#   points(wcr.master.day[,i+10]~wcr.master.day$SWC_1, col=cols[i])
# }
# 
# plot(wcr.master.day[,12]~wcr.master.day$VPD_PI_1,
#      xlim=c(0,25), ylim=c(0,100), col='white',main="VPD (hPa)",ylab=ylab, xlab="");grid()
# for(i in c(1:14)){
#   points(wcr.master.day[,i+10]~wcr.master.day$VPD_PI_1, col=cols[i])
# }
# 
# plot(wcr.master.day[,12]~wcr.master.day$NETRAD_1,
#      xlim=c(0,450), ylim=c(0,100), col='white',main=expression(bold("Net Rad."~(Wm^-2))),ylab=ylab, xlab="");grid()
# for(i in c(1:14)){
#   points(wcr.master.day[,i+10]~wcr.master.day$NETRAD_1, col=cols[i])
# }
# 
# 
# 
# plot(syv.master.day[,12]~syv.master.day$SWC_1, xlim=c(15,45), ylim=c(0,100), col='white',ylab=ylab, xlab="");grid()
# for(i in c(1:20)){
#   points(syv.master.day[,i+10]~syv.master.day$SWC_1, col=cols[i])
# }
# 
# plot(syv.master.day[,12]~syv.master.day$VPD_PI_1, xlim=c(0,25), ylim=c(0,100), col='white',ylab=ylab, xlab="");grid()
# for(i in c(1:20)){
#   points(syv.master.day[,i+10]~syv.master.day$VPD_PI_1, col=cols[i])
# }
# 
# plot(syv.master.day[,12]~syv.master.day$NETRAD_1, xlim=c(0,450), ylim=c(0,100), col='white',ylab=ylab, xlab="");grid()
# for(i in c(1:20)){
#   points(syv.master.day[,i+10]~syv.master.day$NETRAD_1, col=cols[i])
# }



