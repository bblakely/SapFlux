wcr.2016<-read.csv('WCR_2016_SAPFLUX.csv')
wcr.2015<-read.csv('WCR_2015_SAPFLUX.csv')

syv.2016<-read.csv('SYV_2016_SAPFLUX.csv')
syv.2015<-read.csv('SYV_2015_SAPFLUX.csv')

wcr.2015.day<-aggregate(wcr.2015, by=list(wcr.2015$DOY), FUN=mean, na.action=na.omit)
wcr.2016.day<-aggregate(wcr.2016, by=list(wcr.2016$DOY), FUN=mean, na.action=na.omit)

syv.2015.day<-aggregate(syv.2015, by=list(syv.2015$DOY), FUN=mean, na.action=na.omit)
syv.2016.day<-aggregate(syv.2016, by=list(syv.2016$DOY), FUN=mean, na.action=na.omit)



######
wcr.2015.rg<-which(!is.na(rowMeans(wcr.2015.day[,6:19], na.rm=TRUE)))
wcr.2015.line<-rep(NA, 366)
wcr.2015.line[wcr.2015.rg]<-1

syv.2015.rg<-which(!is.na(rowMeans(syv.2015.day[,6:19], na.rm=TRUE)))
syv.2015.line<-rep(NA, 366)
syv.2015.line[syv.2015.rg]<-2

wcr.2016.rg<-which(!is.na(rowMeans(wcr.2016.day[,6:19], na.rm=TRUE)))
wcr.2016.line<-rep(NA, 366)
wcr.2016.line[wcr.2016.rg]<-3

syv.2016.rg<-which(!is.na(rowMeans(syv.2016.day[,6:19], na.rm=TRUE)))
syv.2016.line<-rep(NA, 366)
syv.2016.line[syv.2016.rg]<-4

plot(wcr.2015.line, type='l', ylim=c(0,5), col='orange4')
lines(syv.2015.line, col='forest green')
lines(wcr.2016.line, col='orange')
lines(syv.2016.line, col='green')

abline(v=105)
abline(v=135)
abline(v=166, lty=2)
abline(v=227, lty=2)
abline(v=274)
abline(v=304)
#####

wcr.flow.doy.info<-aggregate(wcr.flow, by=list(wcr.flow$DOY), FUN=mean, na.action=na.omit)
wcr.flow.doy<-as.matrix(wcr.flow.doy.info[,6:19]*1000000)
wcr.flow.doy[is.na(wcr.flow.doy)]<-NA

syv.flow.doy.info<-aggregate(syv.flow, by=list(syv.flow$DOY), FUN=mean, na.action=na.omit)
syv.flow.doy<-as.matrix(syv.flow.doy.info[,6:25]*1000000)
syv.flow.doy[is.na(syv.flow.doy)]<-NA

syv.tree$CC[c(1,5,11,14,17)]<-c('D','S','I','I','C')



syv.test<-syv.flow.doy[20,]
wcr.test<-wcr.flow.doy[20,]


library(nlme)
syv.dat<-cbind(syv.tree, syv.test)
colnames(syv.dat)<-c(names(syv.dat[,1:10]),"flow")
wcr.dat<-cbind(wcr.tree,wcr.test)
colnames(wcr.dat)<-c(names(wcr.dat[,1:10]),"flow")

comb.dat<-rbind(syv.dat, wcr.dat)

test<-(lm(flow~SPP+BA+CC+Site, data=comb.dat))
#+SPP*DBH+SPP*Site
test1<-(gls(flow~SPP+BA+CC+Site, data=comb.dat, na.action=na.omit))


e<-resid(test1)
f<-fitted(test1)
plot(e~f)
# cc.colvec<-rep('black', 20)
# cc.colvec[syv.tree$CC=='D']<-'light blue'
# cc.colvec[syv.tree$CC=='C']<-'orange'
# cc.colvec[syv.tree$CC=='I']<-'forest green'
# cc.colvec[syv.tree$CC=='S']<-'dark red'

cc.pchvec<-rep(0, 20)
cc.pchvec[syv.tree$CC=='D']<-1
cc.pchvec[syv.tree$CC=='C']<-2
cc.pchvec[syv.tree$CC=='I']<-5
cc.pchvec[syv.tree$CC=='S']<-6


plot(e~f, col=as.character(syv.plot.info$colvec),pch=cc.pchvec)

vf2<-varIdent(form = ~1 | CC)
test2<-gls(flow~SPP+BA+CC+Site, weights=vf2, data=comb.dat, method="REML", na.action=na.omit)

e2<-resid(test2)
f2<-fitted(test2)


test3<-gls(flow~SPP+BA+CC+Site, data=comb.dat, method="REML", na.action=na.omit)

##Fuck it let's see how this changes over the seasons
varholder<-data.frame(matrix(data=NA,nrow=nrow(syv.flow.doy), ncol=10))

for(i in 2:nrow(syv.flow.doy)){
  
  syv.test<-syv.flow.doy[i,]
  wcr.test<-wcr.flow.doy[i,]
  
  syv.dat<-cbind(syv.tree, syv.test)
  colnames(syv.dat)<-c(names(syv.dat[,1:10]),"flow")
  wcr.dat<-cbind(wcr.tree,wcr.test)
  colnames(wcr.dat)<-c(names(wcr.dat[,1:10]),"flow")
  
  comb.dat<-rbind(syv.dat, wcr.dat)
  #Redefine canopy vs noncanopy
  comb.dat$CC<-as.character(comb.dat$CC)
  comb.dat$CC[comb.dat$CC=='D' | comb.dat$CC=='C']<-'C'
  comb.dat$CC[comb.dat$CC=='I' | comb.dat$CC=='S']<-'NC'
  
  if(!is.na(mean(comb.dat$flow[comb.dat$Site=='WCR'], na.rm=TRUE)) & !is.na(mean(comb.dat$flow[comb.dat$Site=='SYV'], na.rm=TRUE))){
  mod<-gls(flow~SPP+CC+BA, data=comb.dat, na.action=na.omit)
  varholder[i,]<-as.numeric(unname(mod[[4]])[2:11])
  }else{varholder[i,]<-NA}
  
}

names<-names(mod[[4]])[2:11]
colnames(varholder)<-names

for (i in ncol(varholder)){
  if(!is.na(mean(varholder[,i]))){
  plot(varholder[,i], main=colnames(varholder)[i])
  }
}


##Let's try this with each day a data point



        