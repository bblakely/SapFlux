source('Prepare_data.R')
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

#####Explore ST relationships####

syv.temp<-rowMeans(syv.master[7:10])
wcr.temp<-Lag(rowMeans(wcr.master[7:9]),-2) 
#rowMeans(wcr.master[6:8])
#Lag(rowMeans(wcr.master[6:8]),2) #currently leaves out T4. Also assumes time off by 1 hour!

jump.a<-which(abs(diff(wcr.master$TA_1,1))>8) 
wcr.master$TA_1[jump.a]<-NA

jump.s<-which(abs(diff(wcr.temp))>8) 
wcr.temp[jump.s]<-NA

plot((syv.temp), type='l')
lines(syv.master$TA_1, col='red')
plot((wcr.temp), type='l')
lines(wcr.master$TA_1, col="red")

syv.td<-syv.temp-syv.master$TA_1  #negative: surface cooler. Positive: Atm cooler
wcr.td<-wcr.temp-wcr.master$TA_1

#wcr.td.lag<-Lag(wcr.temp,-2)-wcr.master$TA_1

#Subste to gs
gs<-c(150:250)

syv.td.gs<-syv.td[syv.master$DOY%in%gs]
wcr.td.gs<-wcr.td[wcr.master$DOY%in%gs]

#wcr.td.gs.lag<-wcr.td.lag[wcr.master$DOY%in%gs]

site.cool<-wcr.td.gs - syv.td.gs #negative: more cooling wcr. positive: more cooling syv

##Various aggregations
par(mfrow=c(1,1))
#Separate lines
syv.td.gs.d<-aggregate(syv.td.gs,by=list(wcr.master$H[wcr.master$DOY%in%gs]), FUN="mean", na.rm=TRUE)
wcr.td.gs.d<-aggregate(wcr.td.gs,by=list(wcr.master$H[wcr.master$DOY%in%gs]), FUN="mean", na.rm=TRUE)
plot(syv.td.gs.d, type='l', main='gs diel difference', ylim=c(-3,2), lwd=2)
lines(wcr.td.gs.d, col='blue', lwd=2)
abline(h=0, col='red')

syv.td.d<-aggregate(syv.td,by=list(wcr.master$H), FUN="mean", na.rm=TRUE)
wcr.td.d<-aggregate(wcr.td,by=list(wcr.master$H), FUN="mean", na.rm=TRUE)
plot(syv.td.d, type='l', main='year diel difference', ylim=c(-3,2), lwd=2)
lines(wcr.td.d, col='blue', lwd=2)
abline(h=0, col='red')

syv.td.m<-aggregate(syv.td,by=list(wcr.twr.2016$MONTH), FUN="mean", na.rm=TRUE)
wcr.td.m<-aggregate(wcr.td,by=list(wcr.twr.2016$MONTH), FUN="mean", na.rm=TRUE)
plot(syv.td.m, type='l', main='year monthly difference (note scale)', ylim=c(-1,1), lwd=2)
lines(wcr.td.m, col='blue', lwd=2)
abline(h=0, col='red')

#Difference in differences
site.cool.d<-aggregate(site.cool,by=list(wcr.master$H[wcr.master$DOY%in%gs]), FUN="mean", na.rm=TRUE)
plot(site.cool.d, type='l', main='gs diel site difference', ylim=c(-1,1))
abline(h=0,col='red')
text(4,0.5, "syv more cooling"); text(15,-0.5, "wcr more cooling")

site.cool.dy<-aggregate(wcr.td-syv.td,by=list(wcr.master$H), FUN="mean", na.rm=TRUE)
plot(site.cool.dy, type='l', main='year diel site difference', ylim=c(-1,1))
abline(h=0,col='red')
text(4,0.5, "syv more cooling"); text(15,-0.5, "wcr more cooling")

site.cool.m<-aggregate(wcr.td-syv.td,by=list(wcr.twr.2016$MONTH), FUN="mean", na.rm=TRUE)
plot(site.cool.m, type='l', main='monthly site difference (note scale)', ylim=c(-1,1))
abline(h=0,col='red')
text(4,0.5, "syv more cooling"); text(9,-0.5, "wcr more cooling")
