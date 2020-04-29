#Multi year LE with preharvest

source("Prepare_TowerData_v2.R")

syv.twr<-syv.twr.std[syv.twr.std$YEAR%in%c(2010:2019),]
wcr.twr<-wcr.twr.std[wcr.twr.std$YEAR%in%c(2010:2019),]

syv.twr[syv.twr==-9999]<-NA;wcr.twr[wcr.twr==-9999]<-NA

gpp.syv.c<-umolCO2.to.gC(syv.twr$GPP_PI_F)/48
gpp.wcr.c<-umolCO2.to.gC(wcr.twr$GPP_PI_F)/48

syv.gpp.month<-aggregate(gpp.syv.c, by=list(syv.twr$MONTH), FUN='sum', na.rm=TRUE)
wcr.gpp.month<-aggregate(gpp.wcr.c, by=list(wcr.twr$MONTH), FUN='sum', na.rm=TRUE)
#plot(syv.gpp.month,pch=2); points(wcr.gpp.month, pch=1)

# #Interannual variability?
# wcr.twr.max<-read.csv('WCR_Tower_2015_2017.csv')
# syv.twr.max<-read.csv('SYV_Tower_2015_2017.csv')

extract.prof<-function(dat, year){
  
  yeardat<-dat[dat$YEAR==year,]
  
  yeargpp<-umolCO2.to.gC(yeardat$GPP_PI_F)/48
  y.m.gpp<-aggregate(yeargpp, by=list(yeardat$MONTH), FUN='sum')
  
  y.m.le<-aggregate(yeardat$LE_1, by=list(yeardat$MONTH), FUN='mean', na.rm=TRUE)
  
  profs<-data.frame(cbind(y.m.gpp$x, y.m.le$x));colnames(profs)<-c('gpp','le')
  return(profs)
}


wcr.gpps<-data.frame(matrix())
for(i in 2012:2018){
  
  
}
wcr.2011<-extract.prof(wcr.twr,2011);syv.2011<-extract.prof(syv.twr,2011)
wcr.2012<-extract.prof(wcr.twr,2012);syv.2012<-extract.prof(syv.twr,2012)
wcr.2013<-extract.prof(wcr.twr,2013);syv.2013<-extract.prof(syv.twr,2013)
wcr.2014<-extract.prof(wcr.twr,2014);syv.2014<-extract.prof(syv.twr,2014)
wcr.2015<-extract.prof(wcr.twr,2015);syv.2015<-extract.prof(syv.twr,2015)
wcr.2016<-extract.prof(wcr.twr,2016);syv.2016<-extract.prof(syv.twr,2016)
wcr.2017<-extract.prof(wcr.twr,2017);syv.2017<-extract.prof(syv.twr,2017)
wcr.2018<-extract.prof(wcr.twr,2018);syv.2018<-extract.prof(syv.twr,2018)
wcr.2019<-extract.prof(wcr.twr,2019);syv.2019<-extract.prof(syv.twr,2019)


par(mfrow=c(1,2))

#LE
plot(wcr.2011$le, ylim=c(0,150), col='gray')
points(wcr.2012$le, ylim=c(0,150), col='brown')
points(wcr.2013$le,col='orange')
points(wcr.2014$le,col='yellow')
points(wcr.2015$le,col='red')
points(wcr.2016$le, col='black')
points(wcr.2017$le, col='blue')
points(wcr.2018$le, col='purple')

#LE

points(syv.2013$le,col='orange', pch=2)
points(syv.2014$le,col='yellow', pch=2)
points(syv.2015$le,col='red', pch=2)
points(syv.2016$le, col='black', pch=2)
points(syv.2017$le, col='blue', pch=2)
points(syv.2018$le, col='purple', pch=2)



syv.twr.2015<-syv.twr.max[syv.twr.max$YEAR==2015,]; syv.twr.2015[syv.twr.2015==-9999]<-NA
wcr.twr.2015<-wcr.twr.max[wcr.twr.max$YEAR==2015,]; wcr.twr.2015[wcr.twr.2015==-9999]<-NA

gpp.syv.c<-umolCO2.to.gC(syv.twr.2015$GPP_PI_F)/48
gpp.wcr.c<-umolCO2.to.gC(wcr.twr.2015$GPP_PI_F)/48

syv.gpp.month.15<-aggregate(gpp.syv.c, by=list(syv.twr.2015$MONTH), FUN='sum')
wcr.gpp.month.15<-aggregate(gpp.wcr.c, by=list(wcr.twr.2015$MONTH), FUN='sum')

syv.twr.2017<-syv.twr.max[syv.twr.max$YEAR==2017,];syv.twr.2017[syv.twr.2017==-9999]<-NA
wcr.twr.2017<-wcr.twr.max[wcr.twr.max$YEAR==2017,];wcr.twr.2017[wcr.twr.2017==-9999]<-NA
gpp.syv.c<-umolCO2.to.gC(syv.twr.2017$GPP_PI_F)/48; 
gpp.wcr.c<-umolCO2.to.gC(wcr.twr.2017$GPP_PI_F)/48

syv.gpp.month.17<-aggregate(gpp.syv.c, by=list(syv.twr.2017$MONTH), FUN='sum', na.rm=TRUE)
wcr.gpp.month.17<-aggregate(gpp.wcr.c, by=list(wcr.twr.2017$MONTH), FUN='sum', na.rm=TRUE)


syv.LE.15<-aggregate(syv.twr.2015$LE_1_1_1, by=list(syv.twr.2015$MONTH), FUN=mean, na.rm=TRUE)
wcr.LE.15<-aggregate(wcr.twr.2015$LE_1_1_1, by=list(wcr.twr.2015$MONTH), FUN=mean, na.rm=TRUE)

wcr.LE.16<-aggregate(wcr.twr$LE_1, by=list(wcr.twr$MONTH), FUN=mean, na.rm=TRUE)
syv.LE.16<-aggregate(syv.twr$LE_1, by=list(syv.twr$MONTH), FUN=mean, na.rm=TRUE)
wcr.ET.16<-aggregate(LE.to.ET(wcr.twr$LE_1, Tair=wcr.twr$TA_1)*1800,by=list(wcr.twr$MONTH), FUN=sum, na.rm=TRUE)
syv.ET.16<-aggregate(LE.to.ET(syv.twr$LE_1, Tair=syv.twr$TA_1)*1800,by=list(syv.twr$MONTH), FUN=sum, na.rm=TRUE)

syv.LE.17<-aggregate(syv.twr.2017$LE_1_1_1, by=list(syv.twr.2017$MONTH), FUN=mean, na.rm=TRUE)
wcr.LE.17<-aggregate(wcr.twr.2017$LE_1_1_1, by=list(wcr.twr.2017$MONTH), FUN=mean, na.rm=TRUE)
wcr.ET.17<-aggregate(LE.to.ET(wcr.twr.2017$LE_1_1_1, Tair=wcr.twr.2017$TA_1_1_1)*1800,by=list(wcr.twr.2017$MONTH), FUN=sum, na.rm=TRUE)
syv.ET.17<-aggregate(LE.to.ET(syv.twr.2017$LE_1_1_1, Tair=syv.twr.2017$TA_1_1_1)*1800,by=list(syv.twr.2017$MONTH), FUN=sum, na.rm=TRUE)

par(mfrow=c(2,1), mar=c(4,4,1,1))

plot(syv.gpp.month,pch=17, ylab='GPP', xlab='month'); points(wcr.gpp.month, pch=16)
points(syv.gpp.month.15,pch=2, col='red'); points(wcr.gpp.month.15, pch=1, col='red')
points(syv.gpp.month.17, pch=24, col='blue'); points(wcr.gpp.month.17, pch=21, col='blue')

legend(1,350, legend=c("SYV", "WCR"), pch=c(2,1), cex=0.5, bty='n')
legend (0.95,275, legend=c('2015','2016','2017'), fill=c('red','black','blue'), cex=0.5, bty='n')


leafadjust<-0

plot(syv.LE.16, pch=17, ylim=c(10,200), ylab='LE', xlab='month');points(wcr.LE.16$x, pch=16) #IMPORTANT: /0.7 adjusts for leaf area from harvest
points(syv.LE.15, pch=2, col='red');points(wcr.LE.15$x, pch=1, col='red')
points(syv.LE.17, pch=2, col='blue');points(wcr.LE.17$x, pch=1, col='blue')


