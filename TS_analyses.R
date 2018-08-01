
library(Hmisc) #sometimes you must run this manually (???)
library(zoo)

source('Calc_Sapflow_Full.R')
source('Refine_TowerData.R')

#Will hang for a bit after 'ST data ready' Tower data takes a while

# 
# syv.sap.all.1<-syv.sap.all
# wcr.sap.all.1<-wcr.sap.all
# 
# syv.sap.all[syv.sap.all==0]<-NA
# wcr.sap.all[wcr.sap.all==0]<-NA
# 
# wcr.td[wcr.td>8]<-NA
# syv.td[syv.td>8]<-NA


#####ANALYSES#####

#####Explore height thing; get rid of this eventually.####

par(mfrow=c(1,2))
hist(syv.forest$height, xlim=c(0,40))
hist(wcr.forest$height,xlim=c(0,40))

syv.hw.ratio<-syv.forest$height/syv.forest$ba
wcr.hw.ratio<-wcr.forest$height/wcr.forest$ba

hist(syv.hw.ratio[syv.forest$species=='acsa' & syv.forest$cc=='C'], xlim=c(0,0.2), breaks=seq(from=0,to=0.3,length.out=15))
hist(wcr.hw.ratio[wcr.forest$species=='acsa'& wcr.forest$cc=='C'], xlim=c(0,0.2), breaks=seq(from=0,to=0.3,length.out=15))

plot(syv.forest$height~syv.forest$dbh, xlim=c(0,100), col=syv.forest$col, pch=18)
plot(wcr.forest$height~wcr.forest$dbh, xlim=c(0,100), col=wcr.forest$col, pch=18)
legend(41,19,legend=c('sugar maple', 'hemlock','birch','hophornbeam','basswood','green ash'), 
       col=c('orange','forest green','blue','dark red','yellow','yellow green'), cex=0.7,pch=18)

#WCR has much higher height:dbh ratio
#Species cluseter in height/dbh space, with different slopes
#If allocating to height, less sapwood area, same leaf area = faster flux
#Try fitting lines through these to quantify, both at site level and at species level


#####Explore ST relationships alone####

syv.temp<-rowMeans(syv.master[7:10])
wcr.temp<-Lag(rowMeans(wcr.master[7:9]),-2) 

#NAN areas with implausible jumps in temperature

wcr.twr$TA_1[wcr.twr$TA_1<(-32)]<-NA
jumps<-unique(c(which(abs(diff(wcr.twr$TA_1))>10), which(abs(diff(wcr.twr$TA_1))>10)+1))
wcr.twr$TA_1[jumps]<-NA

jump.a<-which(abs(diff(wcr.master$TA_1,1))>8) 
wcr.master$TA_1[jump.a]<-NA

jump.s<-which(abs(diff(wcr.temp))>8) 
wcr.temp[jump.s]<-NA

syv.temp[is.na(wcr.temp)]<-NA
wcr.temp[is.na(syv.temp)]<-NA

#Ts - Ta; negative: surface cooler. Positive: Atm cooler
syv.td<-syv.temp-syv.master$TA_1
wcr.td<-wcr.temp-wcr.master$TA_1

syv.td.gs<-syv.td[syv.master$DOY%in%gs]
wcr.td.gs<-wcr.td[wcr.master$DOY%in%gs]

syv.td[is.na(wcr.td)]<-NA
wcr.td[is.na(syv.td)]<-NA

#WCR difference - SYV difference; negative: more cooling wcr. positive: more cooling syv
site.cool<-wcr.td.gs - syv.td.gs

######Various aggregations####
par(mfrow=c(1,1))
##Separate lines
#Diel, growing season
syv.td.gs.d<-aggregate(syv.td.gs,by=list(wcr.master$H[wcr.master$DOY%in%gs]), FUN="mean", na.rm=TRUE)
wcr.td.gs.d<-aggregate(wcr.td.gs,by=list(wcr.master$H[wcr.master$DOY%in%gs]), FUN="mean", na.rm=TRUE)
plot(syv.td.gs.d, type='l', main='gs diel difference', ylim=c(-3,2), lwd=2, xlab="hour", ylab="Ts - Ta")
lines(wcr.td.gs.d, col='blue', lwd=2)
abline(h=0, col='red')
legend(0,2,legend=c("Ts - Ta WCR", "Ts - Ta SYV"), col=c('blue', 'black'), lwd=2, cex=0.8)
#SYV warms up faster in the morning; both are similar for most of rest of day
#trysmoothScatter(syv.td.gs~syv.ts$H[syv.ts$DOY%in%gs])

#Diel, whole year
syv.td.d<-aggregate(syv.td,by=list(wcr.master$H), FUN="mean", na.rm=TRUE)
wcr.td.d<-aggregate(wcr.td,by=list(wcr.master$H), FUN="mean", na.rm=TRUE)
plot(syv.td.d, type='l', main='year diel difference', ylim=c(-3,2), lwd=2)
lines(wcr.td.d, col='blue', lwd=2)
abline(h=0, col='red')
#Same thing persists in the year-round but wcr gets much hotter at midday with other seasons included

#Monthly, whole day
syv.td.m<-aggregate(syv.td,by=list(wcr.twr.2016$MONTH), FUN="mean", na.rm=TRUE)
wcr.td.m<-aggregate(wcr.td,by=list(wcr.twr.2016$MONTH), FUN="mean", na.rm=TRUE)
plot(syv.td.m, type='l', main='year monthly difference (note scale)', ylim=c(-1,2), lwd=2, xlab="month",ylab="Ts-Ta")
lines(wcr.td.m, col='blue', lwd=2)
abline(h=0, col='red')
legend(6,1,legend=c("Ts - Ta WCR", "Ts - Ta SYV"), col=c('blue', 'black'), lwd=2, cex=0.8)

#Monthly, daytime
dayhr=c(6:20)
dayind<-which(wcr.twr.2016$HOUR%in%dayhr)
syv.td.m<-aggregate(syv.td[dayind],by=list(wcr.twr.2016$MONTH[dayind]), FUN="mean", na.rm=TRUE)
wcr.td.m<-aggregate(wcr.td[dayind],by=list(wcr.twr.2016$MONTH[dayind]), FUN="mean", na.rm=TRUE)
plot(syv.td.m, type='l', main='year monthly difference, day only (note scale)', ylim=c(-1,2), lwd=2, xlab="month",ylab="Ts-Ta")
lines(wcr.td.m, col='blue', lwd=2)
abline(h=0, col='red')
legend(6,2,legend=c("Ts - Ta WCR", "Ts - Ta SYV"), col=c('blue', 'black'), lwd=2, cex=0.8)
#in both, wcr warmer in spring (especially) and fall. WCR cooler in summer
#Could this be solar energy is high in spring but no leaves to dissipate?


##Difference in differences
#Diel, growing season
site.cool.d<-aggregate(site.cool,by=list(wcr.master$H[wcr.master$DOY%in%gs]), FUN="mean", na.rm=TRUE)
plot(site.cool.d, type='l', main='gs diel site difference', ylim=c(-1,1), xlab="hour")
abline(h=0,col='red')
text(4,0.5, "syv more cooling"); text(15,-0.5, "wcr more cooling")

#Diel, year
site.cool.dy<-aggregate(wcr.td-syv.td,by=list(wcr.master$H), FUN="mean", na.rm=TRUE)
plot(site.cool.dy, type='l', main='year diel site difference', ylim=c(-1,1), xlab="hour")
abline(h=0,col='red')
text(4,0.5, "syv more cooling"); text(15,-0.5, "wcr more cooling")
#Growing season and year are quite different; in gs, wcr does as much midday cooling as syv. Not so in whole year
#Always wcr does more morning cooling though.

#Monthly
site.cool.m<-aggregate(wcr.td-syv.td,by=list(wcr.twr.2016$MONTH), FUN="mean", na.rm=TRUE)
plot(site.cool.m, type='l', main='monthly site difference (note scale)', ylim=c(-1,1), xlab="month")
abline(h=0,col='red')
text(4,0.5, "syv more cooling"); text(9,-0.5, "wcr more cooling")

#Monthly, daytime
site.cool.m<-aggregate(wcr.td[dayind]-syv.td[dayind],by=list(wcr.twr.2016$MONTH[dayind]), FUN="mean", na.rm=TRUE)
plot(site.cool.m, type='l', main='monthly site difference, day (note scale)', ylim=c(-1,1), xlab="month")
abline(h=0,col='red')
text(4,0.75, "syv more cooling"); text(9,-0.5, "wcr more cooling")
#Daytime just enhances overall pattern; implies daytime differences are not much offset by opposite nighttime differences

#####Decompositions####
# #Strip names
# syv.td<-unname(syv.td)
# #Various subsetting params
# doy<-c(204:224)
# startind<-min(which(syv.master$DOY%in%doy))
# endind<-max(which(syv.master$DOY%in%doy))
# dayhr<-c(6:20)
# 
# #Make timeseries
# syv.td.ts<-ts((syv.td[startind:endind]), frequency=48)
# #Metadata for timeseries period
# syv.ref<-syv.master[startind: endind,]
# 
# #Decompose temp and sap
# syv.decomp<-decompose(syv.td.ts)
# syv.decomp.fancy<-stl(syv.td.ts,10, t.window=5)
# 
# syv.testsap<-ts(rowSums(syv.mega[startind:endind,], na.rm=TRUE), frequency=48)
# syv.saptrend<-decompose(syv.testsap)
# syv.saptrend.fancy<-stl(syv.testsap, 10, t.window=5)
# 
# #plot(syv.decomp)
# #plot(syv.saptrend)
# 
# plot(syv.decomp.fancy, main='SYV.temp')
# plot(syv.saptrend.fancy,main='SYV.sap')
# 
# 
# #Strip names wcr
# wcr.td<-unname(wcr.td)
# 
# #Subset wcr temp and metadata
# wcr.td.ts<-ts((wcr.td[startind:endind]), frequency=48)
# wcr.ref<-wcr.master[startind:endind,]
# 
# #Decompose wcr sap and temp
# wcr.decomp<-decompose(wcr.td.ts)
# wcr.decomp.fancy<-stl(wcr.td.ts,10, t.window=5)
# 
# wcr.testsap<-ts(rowSums(wcr.mega[startind:endind,], na.rm=TRUE), frequency=48)
# wcr.saptrend<-decompose(wcr.testsap)
# wcr.saptrend.fancy<-stl(wcr.testsap, 10, t.window=5)
# 
# #plot(wcr.decomp)
# #plot(wcr.saptrend)
# 
# plot(wcr.decomp.fancy, main='WCR.temp')
# plot(wcr.saptrend.fancy,main='WCR.sap')
# 
# #Decompose vpd (and subset trend)
# syv.vpd<-stl(ts(na.spline(syv.ref$VPD_PI_1), frequency=48),10,t.window=5)[[1]][,2]
# wcr.vpd<-stl(ts(na.spline(wcr.ref$VPD_PI_1), frequency=48),10,t.window=5)[[1]][,2]
# 
# #plot ts - sap trend relationships grouped by VPD trend (anomaly)
# plotvpd<-function(saptrend, temptrend, vpdtrend, ref, dayhr=c(10:17),vpd.co=4, int=3){
#   par(mfrow=c(2,3))
#   cols<-c("blue","green", "yellow", "red", 'purple')
#   plot(saptrend[[1]][which(ref$H%in%dayhr&vpdtrend>vpd.co&vpdtrend<vpd.co+(int-1)),2],temptrend[[1]][which(ref$H%in%dayhr&vpdtrend>vpd.co&vpdtrend<vpd.co+(int-1)),2], 
#        xlab='sap', ylab='temp', ylim=c(-2,1.5), xlim=c(-0.5,1), pch=18)
#   for(d in 1:5){
#     vpd.co<-vpd.co+int
#     plot(saptrend[[1]][which(ref$H%in%dayhr&vpdtrend>vpd.co-(int-1)&vpdtrend<vpd.co+(int-1)),2],temptrend[[1]][which(ref$H%in%dayhr&vpdtrend>vpd.co-(int-1)&vpdtrend<vpd.co+(int-1)),2], 
#          xlab='sap', ylab='temp', ylim=c(-2,1.5), xlim=c(-100,400), col=cols[d], pch=18, main=paste("vpd",vpd.co))
#     print(max(saptrend[[1]][which(ref$H%in%dayhr&vpdtrend>vpd.co-(int-1)&vpdtrend<vpd.co+(int-1)),2]))
#     }
#   par(mfrow=c(1,1))
# }
# 
# plotvpd(syv.saptrend.fancy,syv.decomp.fancy,syv.vpd,syv.ref)
# plotvpd(wcr.saptrend.fancy, wcr.decomp.fancy,wcr.vpd,wcr.ref)
# 
# 
# #Reset some parameters
# dayhr<-c(6:21)
# dayref<-which(syv.ref$H%in%dayhr)
# 
# #Subset sap, t, vpd trends
# syv.sap<-syv.saptrend.fancy[[1]][dayref,2]
# wcr.sap<-wcr.saptrend.fancy[[1]][dayref,2]
# 
# syv.t<-syv.decomp.fancy[[1]][dayref,2]
# wcr.t<-wcr.decomp.fancy[[1]][dayref,2]
# 
# syv.vpd<-syv.vpd[dayref]
# wcr.vpd<-wcr.vpd[dayref]
# 
# #get solar and wind trends
# syv.sun<-stl(ts(na.spline(syv.ref$NETRAD_1), frequency=48),10,t.window=5)[[1]][dayref,2]
# syv.wind<-ts(na.spline(syv.ref$WS_1), frequency=48)[dayref] #Not deseasoned becasue diel trend not expected
# #syv.wind<-stl(ts(na.spline(syv.ref$WS_1), frequency=48),10,t.window=5)[[1]][dayref,2]
# 
# 
# test<-lm(syv.t~syv.sun+syv.sap)
# summary(test)
#####
####SAPFLUX TRENDS####
#Site level sums of transp
syv.sap.all<-rowSums(syv.mega, na.rm=TRUE)
syv.sap.all[syv.sap.all==0]<-NA
wcr.sap.all<-rowSums(wcr.mega, na.rm=TRUE)
syv.sap.all[wcr.sap.all==0]<-NA

plot(wcr.sap.all~wcr.twr.2016$DTIME, col='orange', type='l', xlab='DOY', ylab='sap flow (L/30min)')
lines(syv.sap.all~wcr.twr.2016$DTIME, col='forest green', type='l')
#Nice plot to show sapflow differences if enhanced


#The month column has a dumb name left over from subsetting
colnames(syv.master)[2]<-'MONTH'
colnames(wcr.master)[2]<-'MONTH'

par(mfrow=c(1,1))

#Aggregate to monthly sums
syv.sap.month<-aggregate(syv.sap.all, by=list(syv.master$MONTH), FUN="sum", na.rm=TRUE)
wcr.sap.month<-aggregate(wcr.sap.all, by=list(wcr.master$MONTH), FUN="sum", na.rm=TRUE)

plot(wcr.sap.month,type='l', col='orange', xlab='Month', ylab='stand total sapflux (L/month)', main='annual sap flow')

#This loop gives proportions of missing data used in the next few lines
# for(i in 1:12){
#   print(length(which(!is.na(syv.sap.all[syv.master$MONTH==i])))/(length(which(syv.master$MONTH==i))))
# }

#Adjust sums for months with missing data; assume missing times transpire like times with data
syv.sap.month$x[7]<-syv.sap.month$x[7]*(1488/900)  #60% data avaialble for july; adjust sum for this
syv.sap.month$x[6]<-NA
#syv.sap.month$x[6]<-syv.sap.month$x[6]*(1440/362) #25% data available for june
syv.sap.month$x[3]<-syv.sap.month$x[3]*(1488/727) #49% data availble for march
syv.sap.month$x[4]<-NA #No april data at all

#Plot splined data as dotted line, 'real' data as solid line
lines(na.spline(syv.sap.month), col="forest green", lty=2)
lines(syv.sap.month, col="forest green")

#Create alternative version with all months with missing data NAN'd
syv.cons<-syv.sap.month$x
syv.cons[c(3:4,6:7)]<-NA

#Plot splined and real data for that subset
lines(na.spline(syv.cons), col='darkolivegreen3', lty=2)
lines(syv.cons, col='darkolivegreen3')

legend(1,200000,legend=c('wcr','syv, scaled months','syv, splined months'),col=c('orange','forest green','darkolivegreen3'),lwd=1, cex=0.8)

#Plot difference in sapflux, using less conservative adjusted sums
plot(wcr.sap.month$x-na.spline(syv.sap.month$x), type='l')
abline(h=0, col='red')

#Plot comparing differences in sapflux and differences in temperature. 
col.y=c('gray','red','orange','yellow','green','dark green','light blue','blue','purple','orange4','black','pink')
col.live=c('red','orange','yellow','green','blue','purple')
plot((wcr.sap.month$x-na.spline(syv.sap.month$x))~site.cool.m$x,col='gray', ylab='Dsapflow (wcr-syv)', xlab='Dcooling (wcr-syv)', xlim=c(-0.4,0.5))
lines((wcr.sap.month$x-na.spline(syv.sap.month$x))~site.cool.m$x,col='gray')
points((wcr.sap.month$x-na.spline(syv.sap.month$x))[5:10]~site.cool.m$x[5:10],col=col.live, pch=18, cex=1.2)
#lines((wcr.sap.month$x-na.spline(syv.sap.month$x))[5:10]~site.cool.m$x[5:10], col='gray')
abline(h=0)
abline(v=0)

text(c(-0.2,-0.2,0.28,0.28),c(50000,-20000,50000,-20000), c("wcr cooler+more transp", "wcr cooler+less transp", "wcr warmer+more transp","wcr warmer+less transp"), cex=0.8)
#Cool plot, need to work out how to clean up. 
#Check out how this corresponds to leaf area.


######Does sap lag T or vice-versa?#####
lagplot<-function(temptype='raw',sign,index){
  #lag<-abs(i)-1
  #if (sign=="negative"){lag<-(-lag)}
  if(temptype=='raw'){
    syv.temp<-syv.temp
    wcr.temp<-wcr.temp
    ylim=c(5,35)}else{
    syv.temp<-syv.td
    wcr.temp<-wcr.td
    ylim=c(-4,4)
    }
for(i in 1:6){
  lag<-i-1
  if (sign=="negative"){lag<-(-lag)}
  print(i-1)
  print(summary(lm(syv.temp[index]~Lag(syv.sap.all[index], lag)))$r.squared)
  plot(syv.temp[index]~Lag(syv.sap.all[index], -i), xlim=c(0,600), ylim=ylim, main=paste('syv',i))
  print(summary(lm(wcr.temp[index]~Lag(wcr.sap.all[index],lag)))$r.squared)
  plot(wcr.temp[index]~Lag(wcr.sap.all[index], -i), xlim=c(0,600), ylim=ylim, main=paste('wcr',i))
  print('######')
}
}

par(mfrow=c(1,2))
lagplot("raw","negative",daygs)
lagplot("diff","negative",daygs)
#Best matches with sap lagged 30 min -hours, i.e. temps are driving sapflux
#Indicate a feedback?

#####
####Bowen Ratio####
syv.bowen<-syv.twr$H_1/syv.twr$LE_1 ; syv.bowen[syv.bowen>20 | syv.bowen<(-2)]<-NA
wcr.bowen<-wcr.twr$H_1/wcr.twr$LE_1;  wcr.bowen[wcr.bowen>20 | wcr.bowen<(-2)]<-NA

par(mfrow=c(1,2))
plot(syv.td[daygs]~syv.bowen[daygs], xlim=c(-3,4), ylab='Ts-Ta', xlab="Bowen Ratio (H:LE)", main='SYV')
abline(h=0, col='red')
abline(v=0, col='red')
plot(wcr.td[daygs]~wcr.bowen[daygs], xlim=c(-3,4), ylab='Ts-Ta', xlab="Bowen Ratio (H:LE)", main='WCR')
abline(h=0, col='red')
abline(v=0, col='red')

smoothScatter(syv.twr$LE_1[daygs]~syv.sap.all[daygs])
smoothScatter(wcr.twr$LE_1[daygs]~wcr.sap.all[daygs])
#Sapflux is lower limit of LE
#Make a fit line
#Space above is evap contribution probably; does SYV have more evap?

smoothScatter(syv.twr$LE_1[midgs]~syv.sap.all[midgs])
smoothScatter(wcr.twr$LE_1[midgs]~wcr.sap.all[midgs])

par(mfrow=c(2,2))
smoothScatter(syv.twr$VPD_PI_1[daygs]~syv.sap.all[daygs], ylim=c(0,25), xlim=c(0,650))
smoothScatter(wcr.twr$VPD_PI_1[daygs]~wcr.sap.all[daygs], ylim=c(0,25), xlim=c(0,650))
#Sapflux almost linearly related to VPD
#Relationship between LE and VPD is tighter at WCR
smoothScatter(syv.twr$VPD_PI_1[daygs]~syv.twr$LE_1[daygs], ylim=c(0,25), xlim=c(0,450))
smoothScatter(wcr.twr$VPD_PI_1[daygs]~wcr.twr$LE_1[daygs], ylim=c(0,25), xlim=c(0,450))
par(mfrow=c(1,2))


#####
####T:ET####
LE.sap.syv<-(syv.sap.all*2260*1000*1.37)/(6400*1800) #2260: spec. heat water KJ/kg (1kg = 1L); 1000: KJ to Joules; 1.37: surveyed area to total area. 6400: plot (80*80) to m2;  1800: 3o min to s
plot(syv.twr$LE_1, col='gray', type='l', ylim=c(-100,600), ylab='LH', xlab='obs', main='SYV'); lines(LE.sap.syv)
t.et.gs.syv<-LE.sap.syv[gsind]/syv.twr$LE_1[gsind]
t.et.gs.syv[t.et.gs.syv>1 | t.et.gs.syv<0]<-NA
mean(t.et.gs.syv, na.rm=TRUE) #T:ET of 32% in gs

LE.sap.wcr<-(wcr.sap.all*2260*1000*1.37)/(6400*1800)
plot(wcr.twr$LE_1, col='gray', type='l', ylim=c(-100,600), ylab='LH', xlab='obs', main='WCR'); lines(LE.sap.wcr)
LE.sap.wcr[is.na(LE.sap.syv)]<-NA
t.et.gs.wcr<-LE.sap.wcr[gsind]/wcr.twr$LE_1[gsind]
t.et.gs.wcr[t.et.gs.wcr>1 | t.et.gs.wcr<0]<-NA
mean(t.et.gs.wcr, na.rm=TRUE) #T:ET of 39% in gs
#Definitely use these
#Combined with vpd, this might mean eco controls LE more tightly at WCR

#####
####Explore radiation differences####
rad.diff<-wcr.twr$NETRAD_1-syv.twr$NETRAD_1
par(mfrow=c(1,3))

#HOUR (diel)
plot(aggregate(wcr.twr$NETRAD_1[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE), type='l', ylim=c(-100,700), col='blue', main='Rnet')
lines(aggregate(syv.twr$NETRAD_1[gsind], by=list(syv.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE))

plot(aggregate(wcr.twr$SW_IN[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE), type='l', ylim=c(-100,700),col='blue', main='SWin')
lines(aggregate(syv.twr$SW_IN[gsind], by=list(syv.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE))

plot(aggregate(wcr.twr$SW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE), type='l', ylim=c(-100,700), col='blue', main='SWout')
lines(aggregate(syv.twr$SW_OUT[gsind], by=list(syv.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE))

#MONTH (annual)
plot(aggregate(wcr.twr$NETRAD_1, by=list(wcr.twr$MONTH), FUN='mean', na.rm=TRUE), type='l', ylim=c(0,400), col='blue', main='Rnet')
lines(aggregate(syv.twr$NETRAD_1, by=list(syv.twr$MONTH), FUN='mean', na.rm=TRUE))

plot(aggregate(wcr.twr$SW_IN, by=list(wcr.twr$MONTH), FUN='mean', na.rm=TRUE), type='l', ylim=c(0,400),col='blue', main='SWin')
lines(aggregate(syv.twr$SW_IN, by=list(syv.twr$MONTH), FUN='mean', na.rm=TRUE))

plot(aggregate(wcr.twr$SW_OUT, by=list(wcr.twr$MONTH), FUN='mean', na.rm=TRUE), type='l', ylim=c(0,400), col='blue', main='SWout')
lines(aggregate(syv.twr$SW_OUT, by=list(syv.twr$MONTH), FUN='mean', na.rm=TRUE))

#DIFF, HOUR (diel)
plot(aggregate(wcr.twr$SW_IN[gsind]-wcr.twr$SW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$SW_IN[gsind]-syv.twr$SW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', ylim=c(-75,30), main='SW')
lines(aggregate(wcr.twr$SW_IN[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$SW_IN[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', ylim=c(-75,30), main='SW', col='orange')
lines(aggregate(wcr.twr$SW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$SW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', ylim=c(-75,30), col='yellow')
plot(aggregate(wcr.twr$LW_IN[gsind]-wcr.twr$LW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$LW_IN[gsind]-syv.twr$LW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', ylim=c(-75,30), main='LW')
lines(aggregate(wcr.twr$LW_IN[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$LW_IN[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', ylim=c(-75,30), main='LW', col='orange')
lines(aggregate(wcr.twr$LW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$LW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', ylim=c(-75,30), col='yellow')
plot(aggregate(wcr.twr$NETRAD_1[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$NETRAD_1[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', ylim=c(-75,30), main='netrad')

#DIFF, MONTH (annual)
plot(aggregate(wcr.twr$SW_IN-wcr.twr$SW_OUT, by=list(wcr.twr$MONTH), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$SW_IN-syv.twr$SW_OUT, by=list(wcr.twr$MONTH), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', main='SWnet', ylim=c(-40,40))
lines(aggregate(wcr.twr$SW_IN, by=list(wcr.twr$MONTH), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$SW_IN, by=list(wcr.twr$MONTH), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', main='SWin (note scale)', ylim=c(-40,40), col='orange')
lines(aggregate(wcr.twr$SW_OUT, by=list(wcr.twr$MONTH), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$SW_OUT, by=list(wcr.twr$MONTH), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', main="SWout", ylim=c(-40,40), col='yellow')
plot(aggregate(wcr.twr$LW_IN-wcr.twr$LW_OUT, by=list(wcr.twr$MONTH), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$LW_IN - syv.twr$LW_OUT, by=list(syv.twr$MONTH), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', main="LWnet", ylim=c(-40,40))
lines(aggregate(wcr.twr$LW_IN, by=list(wcr.twr$MONTH), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$LW_IN, by=list(wcr.twr$MONTH), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', main="LWin", col='orange')
lines(aggregate(wcr.twr$LW_OUT, by=list(wcr.twr$MONTH), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$LW_OUT, by=list(wcr.twr$MONTH), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', main='LWout', col='yellow')
plot(aggregate(wcr.twr$NETRAD_1, by=list(wcr.twr$MONTH), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$NETRAD_1, by=list(wcr.twr$MONTH), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', main='net', ylim=c(-40,40))

legend(2,40,legend=c("in", "out", 'net'), col=c('orange','yellow','black'), lwd=2)


#Calculate albedo
wcr.alb<-(wcr.twr$SW_OUT)/(wcr.twr$SW_IN)
wcr.alb[which(wcr.twr$SW_IN<wcr.twr$SW_OUT)]<-NA

syv.alb<-(syv.twr$SW_OUT)/(syv.twr$SW_IN)
syv.alb[which(syv.twr$SW_IN<syv.twr$SW_OUT)]<-NA

par(mfrow=c(1,2))
smoothScatter(wcr.alb[dayind]~wcr.twr$DTIME[dayind], ylim=c(0,1), main='wcr albedo', ylab='albedo (SWout / SWin)', xlab='doy')
abline(h=c(0,0.1,0.2,0.3,0.4), lty=3)
smoothScatter(syv.alb[dayind]~syv.twr$DTIME[dayind], ylim=c(0,1), main='syv albedo', ylab='albedo (SWout / SWin)', xlab='doy')
abline(h=c(0,0.1,0.2,0.3,0.4), lty=3)

smoothScatter((wcr.alb[dayind]-syv.alb[dayind])~wcr.twr$DTIME[dayind], ylim=c(-1,1))
abline(h=0)
#This will be useful for showing mesophication, if that's the way I go
#It supplements sap flux in summer, agreeng with decreased temps
#But it counters higher temps in winter.
#####

####EB balancing####
plot(wcr.twr$NETRAD_1~wcr.twr$DOY, type='l', main='wcr')
lines(wcr.twr$H_1+wcr.le~wcr.twr$DOY, col='red')
lines(wcr.twr$LE_1~wcr.twr$DOY, col='blue')
#lines(wcr.twr$H_1~wcr.twr$DOY, col='red')

plot(syv.twr$NETRAD_1~syv.twr$DOY, type='l', main='syv')
lines(syv.twr$H_1+syv.twr$LE_1~syv.twr$DOY, col='red')
lines(syv.twr$LE_1~syv.twr$DOY, col='blue')
#lines(syv.twr$H_1~syv.twr$DOY, col='red')
#Try getting these into bars

wcr.hlenorm<-(wcr.twr$H_1+wcr.le)/wcr.twr$NETRAD_1
wcr.hnorm<-wcr.twr$H_1/wcr.twr$NETRAD_1
wcr.lenorm<-wcr.twr$LE_1/wcr.twr$NETRAD_1

syv.hlenorm<-(syv.twr$H_1+syv.twr$LE_1)/syv.twr$NETRAD_1
syv.hnorm<-syv.twr$H_1/syv.twr$NETRAD_1
syv.lenorm<-syv.twr$LE_1/syv.twr$NETRAD_1

mean(wcr.hlenorm[(wcr.hlenorm)<1 & (wcr.hlenorm)>0], na.rm=TRUE)
mean(syv.hlenorm[(syv.hlenorm)<1 & (syv.hlenorm)>0], na.rm=TRUE)

mean(wcr.twr$H_1/(wcr.twr$H_1+wcr.twr$LE_1), na.rm=TRUE)
mean(wcr.le/(wcr.twr$H_1+wcr.twr$LE_1), na.rm=TRUE)

mean(syv.twr$H_1/(syv.twr$H_1+syv.twr$LE_1), na.rm=TRUE)
mean(syv.twr$LE_1/(syv.twr$H_1+syv.twr$LE_1), na.rm=TRUE)
#LE as portion of HLE much higher at SYV

mean(wcr.twr$WS_1[daygs], na.rm=TRUE)
mean(syv.twr$WS_1[daygs], na.rm=TRUE)
#Slightly windier at SYV; supports roughness/atmospheric transport

#Indices for avoiding crazy ratios
mdgs<-midgs
brtdrygs<-which(wcr.twr$VPD_PI_1>5 & wcr.twr$SW_IN>200 & wcr.twr$DOY%in%gs)
allpos<-which(wcr.twr$H_1>0 & syv.twr$H_1>0 & wcr.twr$LE_1>0 & syv.twr$LE_1>0)

mean(wcr.twr$LE_1[allpos], na.rm=TRUE)
mean(syv.twr$LE_1[allpos], na.rm=TRUE)

mean(wcr.twr$H_1[allpos], na.rm=TRUE)
mean(syv.twr$H_1[allpos], na.rm=TRUE)

mean(wcr.twr$H_1[allpos]/(wcr.twr.clip$H_1+wcr.twr.clip$LE_1)[allpos], na.rm=TRUE)
mean(syv.twr$H_1[allpos]/(syv.twr.clip$H_1+syv.twr.clip$LE_1)[allpos], na.rm=TRUE)
#Sensible heat a bigger part of H+LE at WCR

#Check SW vs PPFD and EB balance
plot(syv.twr$SW_IN~syv.twr$PPFD_IN_PI_F_1, pch=18,cex=0.5)
syv.cal<-lm(syv.twr$SW_IN~syv.twr$PPFD_IN_PI_F_1)
summary(syv.cal)
abline(coef(syv.cal), col='red')
abline(coef(wcr.cal), col='blue')

plot(wcr.twr$SW_IN~wcr.twr$PPFD_IN_PI_F_1, pch=18,cex=0.5)
wcr.cal<-lm(wcr.twr$SW_IN~wcr.twr$PPFD_IN_PI_F_1)
summary(wcr.cal)
abline(coef(wcr.cal), col='red')
abline(coef(syv.cal, col='blue'))

par(mfrow=c(1,2))

#Shortwave alone?
smoothScatter(syv.twr$SW_IN~(wcr.twr$SW_IN));abline(0,1, col='red')
abline(lm(wcr.twr$SW_IN~0+syv.twr$SW_IN))
plot(syv.twr$SW_IN~(wcr.twr$SW_IN));abline(0,1, col='red')
abline(lm(wcr.twr$SW_IN~0+syv.twr$SW_IN))



####Smoothed plots####

plotsmooth<-function(dat1, dat2, ndays,func='mean', varset, allhr=TRUE){
  
  par(mfrow=c(1,2))
  
  for(v in 1:length(varset)){
    
    varcol1<-which(colnames(dat1)==varset[v])
    varcol2<-which(colnames(dat2)==varset[v])
    
    if(allhr==FALSE){
      hrmult<-length(unique(dat1$HOUR))/24
      ndays<-ndays*hrmult}
    
    sm1<-rollapply(dat1[,varcol1], 48*ndays, FUN='mean', na.rm=TRUE)
    sm2<-rollapply(dat2[,varcol2], 48*ndays, FUN='mean', na.rm=TRUE)
    
    date<-rollapply(dat1$DTIME, 48*ndays, FUN='mean', na.rm=TRUE)
    
    #spike1<-which(abs(diff(sm1))>4*sd(diff(sm1), na.rm=TRUE))
    #spike2<-which(abs(diff(sm2))>4*sd(diff(sm2), na.rm=TRUE))
    
    #sm1[spike1]<-NA
    #sm2[spike2]<-NA
    
    ylim=c(min(c(sm1,sm2), na.rm=TRUE), max(c(sm1,sm2), na.rm=TRUE))
    
    plot(sm1~date, type='l', col='blue', main=colnames(dat1)[varcol1], ylab='Wm-2', ylim=ylim)
    lines(sm2~date, type='l', ylab='Wm-2')
    
    legend(x=min(dat1$DOY), y=quantile(ylim,0.18), legend=c('WCR','SYV'), col=c('blue', 'black'), lwd=2, cex=0.7)
    plot((sm1-sm2)~date, type='l', ylab='Wm-2', main="Difference")
    abline(h=0, col='red')
    
  }
}

plotsmooth(dat1=wcr.twr,dat2=syv.twr, ndays=7,varset=c("H_1", "LE_1","NETRAD_1", 'SW_IN', "LW_OUT"))
plotsmooth(dat1=wcr.twr[gsind,],dat2=syv.twr[gsind,], ndays=7,varset=c("H_1", "LE_1","NETRAD_1", 'SW_IN', "LW_OUT"))
plotsmooth(dat1=wcr.twr[daygs,],dat2=syv.twr[daygs,], ndays=7,varset=c("H_1", "LE_1","NETRAD_1", 'SW_IN', "LW_OUT"), allhr=FALSE)
plotsmooth(dat1=wcr.twr[dayind,],dat2=syv.twr[dayind,], ndays=7,varset=c("H_1", "LE_1","NETRAD_1", 'SW_IN', "LW_OUT"), allhr=FALSE)


wcr.master$TS<-wcr.temp; colnames(wcr.master)[3]<-'DTIME'; wcr.master$TD<-wcr.td
syv.master$TS<-syv.temp; colnames(syv.master)[3]<-'DTIME'; syv.master$TD<-syv.td

plotsmooth(dat1=wcr.master,dat2=syv.master, ndays=7,varset=c("TA_1", "TS", "TD", "H_1","NETRAD_1"))
plotsmooth(dat1=wcr.master[gsind,],dat2=syv.master[gsind,], ndays=7,varset=c("TA_1", "TS", "TD", "NETRAD_1"))
plotsmooth(dat1=wcr.master[daygs,],dat2=syv.master[daygs,], ndays=7,varset=c("TA_1", "TS", "TD", "NETRAD_1"))
plotsmooth(dat1=wcr.master[dayind,],dat2=syv.master[dayind,], ndays=7,varset=c("TA_1", "TS", "TD", "NETRAD_1"))

allneg<-which(wcr.twr$H_1<0 | syv.twr$H_1<0 | wcr.twr$LE_1<0 | syv.twr$LE_1<0)
wcr.sub<-wcr.twr
wcr.sub[allneg,]<-NA
syv.sub<-syv.twr
syv.sub[allneg,]<-NA
plotsmooth(dat1=wcr.sub[daygs,],dat2=syv.sub[daygs,], ndays=7,varset=c("H_1", "LE_1","NETRAD_1", 'SW_IN', "LW_OUT"))

#Some linear modeling?


saps<-rowSums(wcr.mega)
flux<-rowMeans(wcr.gap)
vpd<-wcr.twr$VPD_PI_1
vpd[which(vpd==Inf)]<-NA

#what am I doing I need to focus on temp..

midday<-which(wcr.ts$HOUR==14& wcr.ts$MIN== 0 & wcr.ts$DOY%in%gs) #2:00 every day; peakish

plot(wcr.td[midday]~saps[midday])
saponly<-lm(wcr.td[midday]~saps[midday])
summary(saponly)
#plot(saponly)

plot(wcr.td[midday]~wcr.twr$NETRAD[midday])
radonly<-lm(wcr.td[midday]~wcr.twr$NETRAD[midday])
summary(radonly)
#plot(radonly)

plot(wcr.td[midday]~wcr.twr$USTAR[midday])
roughonly<-lm(wcr.td[midday]~wcr.twr$USTAR[midday])
summary(roughonly)
#plot(roughonly)

plot(wcr.td[midday]~wcr.twr$WS[midday])
windonly<-lm(wcr.td[midday]~wcr.twr$WS[midday])
summary(windonly)

#Combined
combo1<-lm(wcr.td[midday]~saps[midday]+wcr.twr$NETRAD[midday])
summary(combo1)

combo2<-lm(wcr.td[midday]~wcr.twr$NETRAD[midday]+saps[midday])
summary(combo2)

#What about LE, VPD?
plot(wcr.td[midday]~wcr.twr$LE[midday])
leonly<-lm(wcr.td[midday]~wcr.twr$LE[midday])
summary(leonly)

plot(wcr.td[midday]~wcr.twr$VPD_PI_1[midday])
vpdonly<-lm(wcr.td[midday]~wcr.twr$VPD_PI_1[midday])
summary(vpdonly)



#fuckit,differences.

difftwr<-wcr.twr-syv.twr
diffsap<-rowSums(wcr.mega)-rowSums(syv.mega)
td<-wcr.master$TD-syv.master$TD
tsd<-wcr.temp-syv.temp


eb.interest<-c(10:11,15:16,18:20,24:25,27:30)


cortab.gsd<-cor(cbind(difftwr[daygs,eb.interest],diffsap[daygs],td[daygs], tsd[daygs]), use="pairwise.complete.obs")
corrplot(cortab.gsd)
#TD not really correlated to anything but TS



summary(lm(tsd[daygs]~difftwr$NETRAD_1[daygs]+difftwr$H_1[daygs]+difftwr$LE_1[daygs]))

summary(lm(wcr.master$TS[daygs]~wcr.twr$NETRAD_1[daygs]+wcr.twr$H_1[daygs]+wcr.twr$LE_1[daygs]+wcr.twr$TA_1[daygs]))
summary(lm(syv.master$TS[daygs]~syv.twr$NETRAD_1[daygs]+syv.twr$H_1[daygs]+syv.twr$LE_1[daygs]+syv.twr$TA_1[daygs]))

summary(lm(tsd[daygs]~difftwr$NETRAD_1[daygs]+difftwr$H_1[daygs]+difftwr$LE_1[daygs]+difftwr$TA_1[daygs]))
summary(lm(tsd[daygs]~difftwr$NETRAD_1[daygs]+difftwr$H_1[daygs]+diffsap[daygs]+difftwr$TA_1[daygs]))
#strong effect of TA on TS; explains another 40%+ of variability

summary(lm(tsd[daygs]~difftwr$NETRAD_1[daygs]+difftwr$H_1[daygs]+difftwr$LE_1[daygs]))
summary(lm(tsd[daygs]~difftwr$NETRAD_1[daygs]+difftwr$H_1[daygs]+diffsap[daygs]))
#TS 14-20% ecosystem determined

summary(lm(td[daygs]~difftwr$NETRAD_1[daygs]+difftwr$H_1[daygs]+difftwr$LE_1[daygs]+difftwr$TA_1[daygs]))
summary(lm(td[daygs]~difftwr$NETRAD_1[daygs]+difftwr$H_1[daygs]+diffsap[daygs]+difftwr$TA_1[daygs]))

summary(lm(td[daygs]~tsd[daygs]))#39% explined by surface diffs
summary(lm(td[daygs]~difftwr$TA_1[daygs]))#basically not at all explained by atm temp diffs

summary(lm(difftwr$TA_1[daygs]~difftwr$NETRAD_1[daygs]+difftwr$H_1[daygs]+difftwr$LE_1[daygs]))
summary(lm(difftwr$TA_1[daygs]~difftwr$NETRAD_1[daygs]+difftwr$H_1[daygs]+diffsap[daygs]))
#TA not ecosystem determined largely; only 14% variability explained

bigmodel<-lm(td[daygs]~difftwr$H_1[daygs]+difftwr$LE_1[daygs]+difftwr$NETRAD[daygs]+difftwr$WS[daygs]+difftwr$USTAR_1[daygs]+diffsap[daygs])
summary(bigmodel)
#Sap significant, but still only explains a tiny fraction of variation

#Does aggregating by day affect anything?
daydiff<-aggregate(difftwr[dayind,], by=list(wcr.twr$DOY[dayind]), FUN='mean', na.rm=TRUE)
daysap<-aggregate(diffsap[dayind], by=list(wcr.twr$DOY[dayind]), FUN='sum' )$x
daytd<-aggregate(td[dayind], by=list(wcr.twr$DOY[dayind]), FUN='mean' )$x
dayts<-aggregate(tsd[dayind], by=list(wcr.twr$DOY[dayind]), FUN='mean' )$x

daydf<-cbind(daydiff, daysap, daytd, dayts)
daycor<-cor(daydf[gs,c((eb.interest+1),37:39)], use="pairwise.complete.obs")
corrplot(daycor)
