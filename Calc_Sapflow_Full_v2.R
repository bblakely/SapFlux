
#Takes mean flux for each species, every timepoint
summ.flux<-function(source,tree,sapcol=0){ 
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

syv.sapsum<-summ.flux(syv.sap,syv.tree)
wcr.sapsum<-summ.flux(wcr.sap,wcr.tree)
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



