
source('Calc_Sapflow_Full.R')

library(Hmisc)
#Will hang for a bit after 'ST data ready' Tower data takes a while

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

gs<-c(150:250)

#plot(syv.master[sample(which(syv.master$DOY%in%gs),1000), c(2,sample(10:30, 3), sample(c(1:9, 30:55), 5))])

#####Explore ST relationships alone####

syv.temp<-rowMeans(syv.master[7:10])
wcr.temp<-Lag(rowMeans(wcr.master[7:9]),-2) 

#NAN areas with implausible jumps in temperature
jump.a<-which(abs(diff(wcr.master$TA_1,1))>8) 
wcr.master$TA_1[jump.a]<-NA

jump.s<-which(abs(diff(wcr.temp))>8) 
wcr.temp[jump.s]<-NA

#Raw ST and TA
plot((syv.temp), type='l')
lines(syv.master$TA_1, col='red')
plot((wcr.temp), type='l')
lines(wcr.master$TA_1, col="red")

#Ts - Ta; negative: surface cooler. Positive: Atm cooler
syv.td<-syv.temp-syv.master$TA_1
wcr.td<-wcr.temp-wcr.master$TA_1

#Subset to gs
gs<-c(150:250)

syv.td.gs<-syv.td[syv.master$DOY%in%gs]
wcr.td.gs<-wcr.td[wcr.master$DOY%in%gs]

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

#Diel, whole year
syv.td.d<-aggregate(syv.td,by=list(wcr.master$H), FUN="mean", na.rm=TRUE)
wcr.td.d<-aggregate(wcr.td,by=list(wcr.master$H), FUN="mean", na.rm=TRUE)
plot(syv.td.d, type='l', main='year diel difference', ylim=c(-3,2), lwd=2)
lines(wcr.td.d, col='blue', lwd=2)
abline(h=0, col='red')

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


#####
#Strip names
syv.td<-unname(syv.td)
#Various subsetting params
doy<-c(204:224)
startind<-min(which(syv.master$DOY%in%doy))
endind<-max(which(syv.master$DOY%in%doy))
dayhr<-c(6:20)

#Make timeseries
syv.td.ts<-ts((syv.td[startind:endind]), frequency=48)
#Metadata for timeseries period
syv.ref<-syv.master[startind: endind,]

#Decompose temp and sap
syv.decomp<-decompose(syv.td.ts)
syv.decomp.fancy<-stl(syv.td.ts,10, t.window=5)

syv.testsap<-ts(rowSums(syv.mega[startind:endind,], na.rm=TRUE), frequency=48)
syv.saptrend<-decompose(syv.testsap)
syv.saptrend.fancy<-stl(syv.testsap, 10, t.window=5)

#plot(syv.decomp)
#plot(syv.saptrend)

plot(syv.decomp.fancy, main='SYV.temp')
plot(syv.saptrend.fancy,main='SYV.sap')


#Strip names wcr
wcr.td<-unname(wcr.td)

#Subset wcr temp and metadata
wcr.td.ts<-ts((wcr.td[startind:endind]), frequency=48)
wcr.ref<-wcr.master[startind:endind,]

#Decompose wcr sap and temp
wcr.decomp<-decompose(wcr.td.ts)
wcr.decomp.fancy<-stl(wcr.td.ts,10, t.window=5)

wcr.testsap<-ts(rowSums(wcr.mega[startind:endind,], na.rm=TRUE), frequency=48)
wcr.saptrend<-decompose(wcr.testsap)
wcr.saptrend.fancy<-stl(wcr.testsap, 10, t.window=5)

#plot(wcr.decomp)
#plot(wcr.saptrend)

plot(wcr.decomp.fancy, main='WCR.temp')
plot(wcr.saptrend.fancy,main='WCR.sap')

#Decompose vpd (and subset trend)
syv.vpd<-stl(ts(na.spline(syv.ref$VPD_PI_1), frequency=48),10,t.window=5)[[1]][,2]
wcr.vpd<-stl(ts(na.spline(wcr.ref$VPD_PI_1), frequency=48),10,t.window=5)[[1]][,2]

#plot ts - sap trend relationships grouped by VPD trend (anomaly)
plotvpd<-function(saptrend, temptrend, vpdtrend, ref, dayhr=c(10:17),vpd.co=4, int=3){
  par(mfrow=c(2,3))
  cols<-c("blue","green", "yellow", "red", 'purple')
  plot(saptrend[[1]][which(ref$H%in%dayhr&vpdtrend>vpd.co&vpdtrend<vpd.co+(int-1)),2],temptrend[[1]][which(ref$H%in%dayhr&vpdtrend>vpd.co&vpdtrend<vpd.co+(int-1)),2], 
       xlab='sap', ylab='temp', ylim=c(-2,1.5), xlim=c(-0.5,1), pch=18)
  for(d in 1:5){
    vpd.co<-vpd.co+int
    plot(saptrend[[1]][which(ref$H%in%dayhr&vpdtrend>vpd.co-(int-1)&vpdtrend<vpd.co+(int-1)),2],temptrend[[1]][which(ref$H%in%dayhr&vpdtrend>vpd.co-(int-1)&vpdtrend<vpd.co+(int-1)),2], 
         xlab='sap', ylab='temp', ylim=c(-2,1.5), xlim=c(-100,400), col=cols[d], pch=18, main=paste("vpd",vpd.co))
    print(max(saptrend[[1]][which(ref$H%in%dayhr&vpdtrend>vpd.co-(int-1)&vpdtrend<vpd.co+(int-1)),2]))
    }
  par(mfrow=c(1,1))
}

plotvpd(syv.saptrend.fancy,syv.decomp.fancy,syv.vpd,syv.ref)
plotvpd(wcr.saptrend.fancy, wcr.decomp.fancy,wcr.vpd,wcr.ref)


#Reset some parameters
dayhr<-c(6:21)
dayref<-which(syv.ref$H%in%dayhr)

#Subset sap, t, vpd trends
syv.sap<-syv.saptrend.fancy[[1]][dayref,2]
wcr.sap<-wcr.saptrend.fancy[[1]][dayref,2]

syv.t<-syv.decomp.fancy[[1]][dayref,2]
wcr.t<-wcr.decomp.fancy[[1]][dayref,2]

syv.vpd<-syv.vpd[dayref]
wcr.vpd<-wcr.vpd[dayref]

#get solar and wind trends
syv.sun<-stl(ts(na.spline(syv.ref$NETRAD_1), frequency=48),10,t.window=5)[[1]][dayref,2]
syv.wind<-ts(na.spline(syv.ref$WS_1), frequency=48)[dayref] #Not deseasoned becasue diel trend not expected
#syv.wind<-stl(ts(na.spline(syv.ref$WS_1), frequency=48),10,t.window=5)[[1]][dayref,2]


test<-lm(syv.t~syv.sun+syv.sap)
summary(test)

##going to sapflux trends
#Site level sums of transp
syv.sap.all<-rowSums(syv.mega, na.rm=TRUE)
wcr.sap.all<-rowSums(wcr.mega, na.rm=TRUE)

plot(wcr.sap.all~wcr.twr.2016$DTIME, col='orange', type='l', xlab='DOY', ylab='sap flow (L/30min)')
lines(syv.sap.all~wcr.twr.2016$DTIME, col='forest green', type='l')

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

# #giant linear model for funsies
# library(party)
# syv.voi<-cbind(syv.master[,31:56], syv.sap.all)
# syv.voi.q<-syv.voi[,which(colMeans(is.na(syv.voi))<0.05)]
# syv.voi.spline<-na.spline(syv.voi)
# #origtest<-cforest(na.spline(syv.td)~., data=syv.voi, control=cforest_unbiased(mtry=2,ntree=50))
# vars<-varimp(origtest)

#####At Adrian's suggestion, more basic plotting

wcr.td[wcr.td>8]<-NA
syv.td[syv.td>8]<-NA

wcr.twr<-wcr.twr.2016
syv.twr<-syv.twr.2016

syv.sap.all.1<-syv.sap.all
wcr.sap.all.1<-wcr.sap.all

syv.sap.all[syv.sap.all==0]<-NA
wcr.sap.all[wcr.sap.all==0]<-NA

dayhr<-c(6:20)
dayind<-which(syv.twr$HOUR%in%dayhr) #made earlier; daytime hours
gs<-gs #made earlier, growing season days
gsind<-which(syv.twr$DOY%in%gs)

daygs<-which(syv.twr$HOUR%in%dayhr & syv.twr$DOY%in%gs)



par(mfrow=c(1,2))
plot(syv.td[gsind]~syv.sap.all[gsind], xlim=c(0,600), ylim=c(-5,5))
plot(wcr.td[gsind]~wcr.sap.all[gsind], xlim=c(0,600), ylim=c(-5,5))

plot(syv.td[gsind]~syv.twr$LE_1[gsind], xlim=c(0,600), ylim=c(-5,5))
plot(wcr.td[gsind]~wcr.twr$LE_1[gsind], xlim=c(0,600), ylim=c(-5,5))

plot(syv.td[gsind]~syv.twr$H_1[gsind], xlim=c(0,600), ylim=c(-5,5))
plot(wcr.td[gsind]~wcr.twr$H_1[gsind], xlim=c(0,600), ylim=c(-5,5))

syv.bowen<-syv.twr$H_1/syv.twr$LE_1 ; syv.bowen[syv.bowen>20 | syv.bowen<(-2)]<-NA
wcr.bowen<-wcr.twr$H_1/wcr.twr$LE_1;  wcr.bowen[wcr.bowen>20 | wcr.bowen<(-2)]<-NA
plot(syv.td[daygs]~syv.bowen[daygs], xlim=c(-3,3))
abline(h=0, col='red')
abline(v=0, col='red')
plot(wcr.td[daygs]~wcr.bowen[daygs], xlim=c(-3,3))
abline(h=0, col='red')
abline(v=0, col='red')

par(mfrow=c(1,1))

plot(syv.twr$LE_1[daygs]~syv.sap.all[daygs])
plot(wcr.twr$LE_1[daygs]~wcr.sap.all[daygs])

LE.sap.syv<-(syv.sap.all*2260*1000*1.37)/(6400*1800)
plot(syv.twr$LE_1, col='gray', type='l', ylim=c(-100,600)); lines(LE.sap)

LE.sap.wcr<-(wcr.sap.all*2260*1000*1.37)/(6400*1800)
plot(wcr.twr$LE_1, col='gray', type='l', ylim=c(-100,600)); lines(LE.sap.wcr)

#testing radiation differences
#NAN all unpaired obs
wcr.twr$SW_IN[is.na(syv.twr$SW_IN)]<-NA
syv.twr$SW_IN[is.na(wcr.twr$SW_IN)]<-NA
wcr.twr$SW_OUT[is.na(syv.twr$SW_OUT)]<-NA
syv.twr$SW_OUT[is.na(wcr.twr$SW_OUT)]<-NA
wcr.twr$LW_IN[is.na(syv.twr$LW_IN)]<-NA
syv.twr$LW_IN[is.na(wcr.twr$LW_IN)]<-NA
wcr.twr$LW_OUT[is.na(syv.twr$LW_OUT)]<-NA
syv.twr$LW_OUT[is.na(wcr.twr$LW_OUT)]<-NA

plot(wcr.twr$NETRAD_1~wcr.twr$DTIME, type='l')
lines(syv.twr$NETRAD_1~wcr.twr$DTIME,col='red')

rad.diff<-wcr.twr$NETRAD_1-syv.twr$NETRAD_1

par(mfrow=c(1,3))
plot(aggregate(wcr.twr$NETRAD_1[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE), type='l', ylim=c(-100,700), col='blue', main='Rnet')
lines(aggregate(syv.twr$NETRAD_1[gsind], by=list(syv.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE))

plot(aggregate(wcr.twr$SW_IN[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE), type='l', ylim=c(-100,700),col='blue', main='SWin')
lines(aggregate(syv.twr$SW_IN[gsind], by=list(syv.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE))

plot(aggregate(wcr.twr$SW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE), type='l', ylim=c(-100,700), col='blue', main='SWout')
lines(aggregate(syv.twr$SW_OUT[gsind], by=list(syv.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE))

par(mfrow=c(1,3))
plot(aggregate(wcr.twr$SW_IN[gsind]-wcr.twr$SW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$SW_IN[gsind]-syv.twr$SW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', ylim=c(-75,30), main='SW')
lines(aggregate(wcr.twr$SW_IN[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$SW_IN[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', ylim=c(-75,30), main='SW', col='orange')
lines(aggregate(wcr.twr$SW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$SW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', ylim=c(-75,30), col='yellow')
plot(aggregate(wcr.twr$LW_IN[gsind]-wcr.twr$LW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$LW_IN[gsind]-syv.twr$LW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', ylim=c(-75,30), main='LW')
lines(aggregate(wcr.twr$LW_IN[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$LW_IN[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', ylim=c(-75,30), main='LW', col='orange')
lines(aggregate(wcr.twr$LW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$LW_OUT[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', ylim=c(-75,30), col='yellow')
plot(aggregate(wcr.twr$NETRAD_1[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x-aggregate(syv.twr$NETRAD_1[gsind], by=list(wcr.twr$HOUR[gsind]), FUN='mean', na.rm=TRUE)$x, ylab='WCR - SYV', type='l', ylim=c(-75,30), main='netrad')

par(mfrow=c(1,3))
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



