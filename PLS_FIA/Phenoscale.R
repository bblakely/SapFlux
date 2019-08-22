#Let's do 2016 only first
#wishlist: aggregate by hour within days first OR aggregate by hour within months'
wcr.15<-read.csv('WCR_2015_SAPFLUX.csv')[,5:18];syv.15<-read.csv('SYV_2015_SAPFLUX.csv')[,5:24]

#wcr.gap.bku<-wcr.gap
wcr.gap[is.na(wcr.gap)]<-wcr.15[is.na(wcr.gap)]

#syv.gap.bku<-syv.gap
syv.gap[is.na(syv.gap)]<-syv.15[is.na(syv.gap)]
length(which(!is.na(syv.15[is.na(syv.gap.bku)])))

#Get monthly transp rates
wcr.sap.day<-aggregate(wcr.gap, by=list(wcr.twr.2016$'DOY'), FUN='mean', na.rm=TRUE )
syv.sap.day<-aggregate(syv.gap, by=list(syv.twr.2016$'DOY'), FUN='mean', na.rm=TRUE )

monthvec<-c(rep(1, 31), rep(2, 29), rep(3, 31), 
            rep(4, 30), rep(5, 31), rep(6, 30),
            rep(7, 31), rep(8, 31), rep(9, 30),
            rep(10, 31), rep(11, 30), rep(12, 31))

wcr.monthagg<-cbind(wcr.sap.day[2:15], monthvec)
wcr.month<-aggregate(wcr.monthagg, by=list(monthvec), FUN='mean', na.rm=TRUE)

syv.monthagg<-cbind(syv.sap.day[2:21], monthvec)
syv.month<-aggregate(syv.monthagg, by=list(monthvec), FUN='mean', na.rm=TRUE)


summ.flux<-function(source,tree,sapcol){ 
  sapsum<-data.frame(matrix(nrow=nrow(source), ncol=length(unique(tree$SPP))))
  for (i in 1:length(unique(tree$SPP))){
    soi<-as.character(unique(tree$SPP)[i])
    #sp<-unname(rowMeans(syv.master[,sapstart+which(syv.tree$SPP==soi)], na.rm=TRUE))
    sapsum[,i]<-unname(rowMeans(source[,sapcol+which(tree$SPP==soi)], na.rm=TRUE))
  }
  colnames(sapsum)<-as.character(unique(tree$SPP))
  sapsum$UK<-rowMeans(sapsum)
  return(sapsum)
}

syv.trmonth<-summ.flux(syv.month, syv.tree, 1)
wcr.trmonth<-summ.flux(wcr.month, wcr.tree, 1)

#Get scalars for other SPP

wcr.norm<-data.frame(apply(wcr.trmonth, 2, function(x) x/max(x, na.rm=TRUE)))
syv.norm<-data.frame(apply(syv.trmonth, 2, function(x) x/max(x, na.rm=TRUE)))

par(mfrow=c(1,2))
colvec<-c('yellow', 'dark red','orange','green','gray')
plot(wcr.norm[,1], ylim=c(-0.1,1.1), col='white')
for(i in c(1:5)){
  lines(wcr.norm[,i], col=colvec[i])
}
colvec<-c('dark green', 'dark red','blue','orange','gray')
plot(syv.norm[,1], ylim=c(-0.1,1.1), col='white')
for(i in c(1:5)){
  lines(syv.norm[,i], col=colvec[i])
}

scalarvec<-rowMeans(cbind(wcr.norm$UK, syv.norm$UK), na.rm=TRUE)

#Build flux lut

#Probably a better way to do this...
acsamean<-rowMeans(cbind(wcr.trmonth$ACSA, syv.trmonth$ACSA), na.rm=TRUE)
osvimean<-rowMeans(cbind(wcr.trmonth$OSVI, syv.trmonth$OSVI), na.rm=TRUE)
ukmean<-rowMeans(cbind(wcr.trmonth$UK, syv.trmonth$UK), na.rm=TRUE)

trmonth<-cbind(acsamean, osvimean, wcr.trmonth$TIAM, wcr.trmonth$FRPE, syv.trmonth$TSCA, syv.trmonth$BEAL)
colnames(trmonth)<-c('ACSA', 'OSVI','TIAM','FRPE','TSCA','BEAL')

#interpolate
trmonth<-data.frame(na.approx(trmonth))

megalut<-array(dim=c(nrow(fluxrates.lut), ncol(fluxrates.lut), 12))
#for(i in c(1:12)){
  lut<-fluxrates.lut
  lut$FLUX<-round(lut$FLUX*scalarvec[i], digits=7) #All growing season numbers, scaled
  lut$FLUX[match(colnames(trmonth), fluxrates.lut$SPP)]<-round(as.numeric(trmonth[i,])/1000000, digits=7) #Add in measured numbers
  lut$FLUX[12]<-mean(lut$FLUX[c(1:4,7:9,11)]);lut$FLUX[13]<-mean(lut$FLUX[c(5:6,10)]) #Average for hardwoods, softwoods
#}

