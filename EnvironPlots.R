
if(!exists('wcr.master')){
source('Prepare_Data.R')
source('Refine_TowerData.R')
source('Calc_Sapflow_Full.R')
}

peakgs<-which(wcr.master$DOY>167& wcr.master$DOY<244 & wcr.master$H%in%dayhr)

wcr.master.day<-aggregate(wcr.master[daygs,], by=list(wcr.master$DOY[daygs]), FUN='mean', na.rm=TRUE)
syv.master.day<-aggregate(syv.master[daygs,], by=list(syv.master$DOY[daygs]), FUN='mean', na.rm=TRUE)
gs<-c(150:250)
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

#FITS
par(mfrow=c(3,3))


syv.coefs<-matrix(nrow=length(which(syv.tree$SPP=="ACSA")), ncol=2); count=0
for (i in which(syv.tree$SPP=="ACSA")){

pair<-data.frame(cbind(syv.master.day[,i+11], syv.master.day$VPD_PI_1)); colnames(pair)<-c("SAP", "VPD")
print(colnames(syv.master.day)[i+11])
y<-pair$SAP
x<-pair$VPD/10
y1<-y[!is.na(y)&!is.na(x)]
x1<-x[!is.na(y)&!is.na(x)]

fit<-nls(y1~a*(1-exp(-b*x1)), start=list(a=12, b=0.7))

# testx<-seq(from=0.1, to=2.0, by=0.1)
# testy<-a*(1-exp(-b*testx))
# plot(testx, testy)

plot(x1,y1, ylim=c(0,80), main=paste("SYV;", "a =",round(coef(fit)[1],2), "b =",round(coef(fit)[2],2)), ylab="sap flux", xlab="VPD")
yfit<-predict(fit)
lines(sort(x1), sort(yfit),col="red", pch="-")
count=count+1
syv.coefs[count,]<-(coef(fit))
}

par(mfrow=c(3,3))
wcr.coefs<-matrix(nrow=length(which(wcr.tree$SPP=="ACSA")), ncol=2); count=0
for (i in which(wcr.tree$SPP=="ACSA")){
  
  pair<-data.frame(cbind(wcr.master.day[,i+11], wcr.master.day$VPD_PI_1)); colnames(pair)<-c("SAP", "VPD")
  print(colnames(wcr.master.day)[i+11])
 
   y<-pair$SAP
  x<-pair$VPD/10
  y1<-y[!is.na(y)&!is.na(x)]
  x1<-x[!is.na(y)&!is.na(x)]
  
  fit<-nls(y1~a*(1-exp(-b*x1)), start=list(a=40, b=2))
  
  # testx<-seq(from=0.1, to=2.0, by=0.1)
  # testy<-a*(1-exp(-b*testx))
  # plot(testx, testy)
  
  plot(x1,y1, ylim=c(0,80), main=paste("WCR;", "a =", round(coef(fit)[1],2), "b =",round(coef(fit)[2],2)), ylab="sap flux", xlab="VPD")
  yfit<-predict(fit)
  lines(sort(x1), sort(yfit),col="red")
  count=count+1
  wcr.coefs[count,]<-(coef(fit))
}



t.test(wcr.coefs[,1], syv.coefs[,1]); t.test(wcr.coefs[,2], syv.coefs[,2])


#Ewers chcks
#Ewers coefs
1.45/0.52; 1.4/0.36
#mine
mean(wcr.coefs[,1])/mean(syv.coefs[,1])

#Ewers Ec: 3.44mm/day wcr, 1.74 mm/day syv
#mie:
syv.rain.day<-aggregate(syv.twr$P_PI_F, by=list(syv.twr$DOY), FUN='sum', na.rm=TRUE)
wcr.rain.day<-aggregate(wcr.twr$P_PI_F, by=list(wcr.twr$DOY), FUN='sum', na.rm=TRUE)


gs.leaf<-c(170:250); #gs.leaf<-gs.leaf[syv.rain.day$x[gs.leaf]==0 & wcr.rain.day$x[gs.leaf]==0]

wcr.mega.day<-aggregate(wcr.mega, by=list(wcr.twr$DOY), FUN='sum')
wcr.mega.maples<-rowSums(wcr.mega.day[,wcr.forest$species=='acsa'])/6400 #L/m2, i.e. mm
syv.mega.day<-aggregate(syv.mega, by=list(syv.twr$DOY), FUN='sum')
syv.mega.maples<-rowSums(syv.mega.day[,syv.forest$species=='acsa'])/6400

plot(wcr.mega.maples); plot(syv.mega.maples) 
mean(wcr.mega.maples[gs.leaf], na.rm=TRUE);mean(syv.mega.maples[gs.leaf], na.rm=TRUE)
#low for both; both about 25% ewers estimate with rainy days in, 30% with rainy days out.

3.44/1.74; 3.88/1.47

wcr.mega.maples[is.na(syv.mega.maples)]<-NA;syv.mega.maples[is.na(wcr.mega.maples)]<-NA
mean(wcr.mega.maples[gs.leaf], na.rm=TRUE)/mean(syv.mega.maples[gs.leaf], na.rm=TRUE) #Ratio OK,not spectacular but difference is smaller than interannual variability

#EL
#Ewers WCR L is 3.7/5.4; syv L is 4.8/7.1 (7.1 is much higher than measured with Li-Cor)
wcr.pctleaves<-3.7/5.4; syv.pctleaves<-4.8/7.1; syv.pctleaves.lai<-(5.54-1.13)/5.54 #Much more with li-cor, but that includes birch
totleaves.wcr<-max(LAI.dat$WCR, na.rm=TRUE); totleaves.syv<-max(LAI.dat$SYV, na.rm=TRUE)

mapleleaf.wcr<-wcr.pctleaves*totleaves.wcr; mapleleaf.syv<-syv.pctleaves*totleaves.syv

wcr.mega.mapleleaf<-wcr.mega.maples/(wcr.pctleaves*totleaves.wcr); syv.mega.mapleleaf<-syv.mega.maples/(syv.pctleaves*totleaves.syv)
plot(syv.mega.mapleleaf); plot(wcr.mega.mapleleaf)
plot(syv.mega.mapleleaf[gs]*3.33~syv.master.day$VPD_PI_1, xlim=c(0,20))
plot(wcr.mega.mapleleaf[gs]*3.33~wcr.master.day$VPD_PI_1, xlim=c(0,20)) #looks good except for a relative lack of curvature.

###Soil moisture checks####
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

#Leaf area
syv.tree.leaf<-0.375+(0.286*syv.tree$DBH^2.62)


