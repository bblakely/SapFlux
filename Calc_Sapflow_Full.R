#Aggregate to species fluxrates
source('Prepare_data.R')

#Reads in gapfilled files for sapflux rates, in g/m2 s

sapstart<-10

#syv.tsca<-unname(rowMeans(syv.master[,sapstart+which(syv.tree$SPP=='TSCA')], na.rm=TRUE))
#syv.tsca<-unname(rowMeans(syv.master[,sapstart+which(syv.tree$SPP=='TSCA')], na.rm=TRUE))

#Takes mean flux for each species, every timepoint
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

syv.sapsum<-summ.flux(syv.master,syv.tree,sapstart)
wcr.sapsum<-summ.flux(wcr.master,wcr.tree,sapstart)
wcr.sapsum$QURU<-wcr.sapsum$UK #Not quite right; should find something more precise.

#Make unbelievably large matrix of forest for each.
#This version takes species averages; could split into overstory and understory to investigate sensitivity
modeltrees<-function(sapsum,tree){
mega<-matrix(nrow=nrow(sapsum), ncol=nrow(tree))
colnames(sapsum)<-tolower(colnames(sapsum))
for(r in 1:nrow(tree)){
mega[,r]<-(tree$SWA[r]/10000)*sapsum[,which(colnames(sapsum)==tree$species[r])] * 1800 / 1000 # L/30min period, i.e. total 
}
colnames(mega)<-paste((tree$cc), toupper(tree$species), tree$id)
return(mega)
}

wcr.mega<-modeltrees(wcr.sapsum,wcr.forest)
syv.mega<-modeltrees(syv.sapsum, syv.forest)


#Calculate for actual measured trees
wcr.site<-sweep(wcr.gap, 2,((wcr.tree$SWA_calc/10000)*wcr.tree$MULT_calc), FUN='*')*1800/1000
syv.site<-sweep(syv.gap, 2,((syv.tree$SWA_calc/10000)*syv.tree$MULT_calc), FUN='*')*1800/1000

#temporary
plot(aggregate(syv.site[,1], by=list(syv.master$H), FUN='mean', na.rm=TRUE), col='white', ylim=c(0,3))
for(i in 1:ncol(syv.site)){
    lines(aggregate(syv.site[,i], by=list(syv.master$H), FUN='mean', na.rm=TRUE), col=syv.tree$col[i])
}

plot(aggregate(wcr.site[,1], by=list(wcr.master$H), FUN='mean', na.rm=TRUE), col='white', ylim=c(0,3))
for(i in 1:ncol(wcr.site)){
  lines(aggregate(wcr.site[,i], by=list(wcr.master$H), FUN='mean', na.rm=TRUE), col=wcr.tree$col[i])
}
