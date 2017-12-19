wcr.flux<-wcr.dat
wcr.ts<-wcr.ts
WCR.twr.2016<-read.csv('WCR.tower.csv')

daycol<-rep('black', 48)
hours<-wcr.ts$HOUR[wcr.ts$DOY==194] #any day works
daycol[hours>=5 & hours <=13]<-'orange'
daycol[hours>13& hours < 22]<-'blue'

#All sensors one day
for(i in 1:14){
  plot(wcr.flux[wcr.ts$DOY==199,i]~WCR.twr.2016$vpd[wcr.ts$DOY==199], main=i, col=daycol)
}

#One sensor several days
par(mfrow=c(3,2), mar=c(4,4,2,2))

sensa<-5
sensb<-3
for(i in 200:220){
  plot(wcr.flux[wcr.ts$DOY==i,sensa]~WCR.twr.2016$vpd[wcr.ts$DOY==i], main=paste ('sensa, DOY',i), col=daycol, ylab='flux(g/m^2s)', xlab='vpd')
  plot(wcr.flux[wcr.ts$DOY==i,sensb]~WCR.twr.2016$vpd[wcr.ts$DOY==i], main=paste ('sensb, DOY',i), col=daycol,ylab='flux(g/m^2s)', xlab='vpd')
  
  plot(wcr.flux[wcr.ts$DOY==i,sensa]~WCR.twr.2016$hour[wcr.ts$DOY==i], main=paste ('sensa, DOY',i), col=daycol, xlab='hour', ylab='flux(g/m^2s)')
  plot(wcr.flux[wcr.ts$DOY==i,sensb]~WCR.twr.2016$hour[wcr.ts$DOY==i], main=paste ('sensb, DOY',i), col=daycol, xlab='hour', ylab='flux(g/m^2s)')
  
  plot(WCR.twr.2016$vpd[wcr.ts$DOY==i]~WCR.twr.2016$hour[wcr.ts$DOY==i], main=paste ('VPD, DOY',i), col=daycol, ylim=c(0,1500), xlab='hour', ylab='vpd')
  plot(WCR.twr.2016$vpd[wcr.ts$DOY==i]~WCR.twr.2016$hour[wcr.ts$DOY==i], main=paste ('VPD, DOY',i), col=daycol, ylim=c(0,1500), xlab='hour', ylab='vpd')
  
  }

par(mfrow=c(1,1))

fl.t<-wcr.flux[wcr.ts$DOY==213,3]
vd.t<-WCR.twr.2016$vpd[wcr.ts$DOY==213]
plot(vd.t,fl.t,col=daycol)

lag.ez<-function(ts, lagnum){
  ts.lag<-c(rep(NA,lagnum), ts[1:(length(ts)-lagnum)])
  return(ts.lag)
}

for(i in 1:4){
  plot(lag.ez(vd.t,i-1),fl.t, main=paste('lag',i-1), col=lag.ez(daycol,i-1))
}


#fit an ellipse
library('hysteresis')

ch<-wcr.flux[wcr.ts$DOY==200,5]
d<-WCR.twr.2016$vpd[wcr.ts$DOY==200]

d.f<-d[d>400]
ch.f<-ch[d>400]

thing2<-fel(d.f,ch.f)
plot(thing2)


#try lming

for(t in 200:220){
  

}

datset.fl<-ts(wcr.flux[wcr.ts$DOY==t,sensa])
datset.vp<-ts(WCR.twr.2016$vpd[wcr.ts$DOY==t])
datset.par<-ts(WCR.twr.2016$par[wcr.ts$DOY==t])
datset.temp<-ts(WCR.twr.2016$tair[wcr.ts$DOY==t])

#all 3
tsmod1<-tslm(datset.fl~datset.par+datset.vp+datset.temp)

#2's
tsmod2.1<-tslm(datset.fl~datset.par+datset.vp)
tsmod2.2<-tslm(datset.fl~datset.par+datset.temp)
tsmod2.3<-tslm(datset.fl~datset.vp+datset.temp)

#singletons
tsmod3.1<-tslm(datset.fl~datset.par)
tsmod3.2<-tslm(datset.fl~datset.vp)
tsmod3.3<-tslm(datset.fl~datset.temp)

#interactions?
tsmod1I<-tslm(datset.fl~datset.par+datset.vp+datset.temp+
                datset.par*datset.vp + datset.par*datset.temp + datset.vp*datset.temp)

