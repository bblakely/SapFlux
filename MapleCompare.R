syv<-read.csv("SYV_2016_K.csv")
wcr<-read.csv("WCR_2016_K.csv")
und<-read.csv("UND_2017_BLprocessed.csv")
map<-read.csv("MapleMono_manual.csv")


syv.dat<-syv[which(syv$DOY%in%c(184:224)),6:25]
wcr.dat<-wcr[which(wcr$X1.1%in%c(184:224)),6:19]
und.dat<-und[which(und$DOY%in%c(184:224)),6:19]
map.dat<-map[which(map$X183%in%c(184:224)),6:18]

#boxplot(syv.dat);boxplot(und.dat); boxplot(wcr.dat)


syv.vs<-(119e-6)*(syv.dat^1.231)*1000000
wcr.vs<-(119e-6)*(wcr.dat^1.231)*1000000
und.vs<-(119e-6)*(und.dat^1.231)*1000000
map.vs<-(119e-6)*(map.dat^1.231)*1000000

gaps<-which(is.na(syv.vs$S1)|is.na(wcr.vs[,1])|is.na(map.vs[,1])|is.na(und.vs[,1]))

syv.vs[gaps,]<-NA;wcr.vs[gaps,]<-NA;und.vs[gaps,]<-NA;map.vs[gaps,]<-NA;

syv.vs.prof<-aggregate(syv.vs, by=list(syv$H[syv$DOY%in%c(184:224)]), FUN='mean', na.rm=TRUE)
wcr.vs.prof<-aggregate(wcr.vs, by=list(syv$H[syv$DOY%in%c(184:224)]), FUN='mean', na.rm=TRUE)
und.vs.prof<-aggregate(und.vs, by=list(syv$H[syv$DOY%in%c(184:224)]), FUN='mean', na.rm=TRUE)
map.vs.prof<-aggregate(map.vs, by=list(syv$H[syv$DOY%in%c(184:224)]), FUN='mean', na.rm=TRUE)

syv.tree<-read.csv('SYV_trees.csv');wcr.tree<-read.csv('WCR_trees.csv');
und.tree<-read.csv('UND_tree.csv');map.tree<-read.csv('MM_tree.csv')

map.tree$species<-rep('acsa', nrow(und.tree))



assign.color<-function(data, spcol=which(colnames(data)=="species")){
  colvec<-rep("gray", nrow(data))
  spp<-data[,spcol]
  colvec[spp=='acsa'|spp=='ACSA']<-"orange"
  colvec[spp=='tiam'|spp=='TIAM']<-"yellow"
  colvec[spp=='quru'|spp=='QURU']<-"orange4"
  colvec[spp=='frpe'|spp=='FRPE']<-"yellow green"
  colvec[spp=='osvi'|spp=='OSVI']<-"dark red"
  colvec[spp=='beal'|spp=='BEAL']<-"blue"
  colvec[spp=='tsca'|spp=='TSCA']<-"dark green"
  colvec[spp=='potr'|spp=='POTR']<-"light blue"
  colvec[spp=='acru'|spp=='ACRU']<-"red"
  colvec[spp=='bepa'|spp=='BEPA']<-"gray"
  data$col<-colvec
  return(data)
}


syv.tree<-assign.color(syv.tree, spcol=3)
wcr.tree<-assign.color(wcr.tree, spcol=3)
und.tree<-assign.color(und.tree, spcol=3)
map.tree<-assign.color(map.tree, spcol=5)

par(mfrow=c(1,4))
plot(syv.vs.prof[,1+1]~syv.vs.prof$Group.1, type='l', ylim=c(0,90), xlab="SYV", col='white')
for(i in 1:20){
  lines(syv.vs.prof[,i+1]~syv.vs.prof$Group.1, col=syv.tree$col[i])
}

plot(wcr.vs.prof[,1+1]~wcr.vs.prof$Group.1, type='l', ylim=c(0,90),xlab="WCR", col='white')
for(i in 1:14){
  lines(wcr.vs.prof[,i+1]~wcr.vs.prof$Group.1,col=wcr.tree$col[i])
}

plot(und.vs.prof[,1+1]~und.vs.prof$Group.1, type='l', ylim=c(0,90),xlab="UND", col='white')

for(i in 1:14){
  lines(und.vs.prof[,i+1]~und.vs.prof$Group.1,col=und.tree$col[i])
}

plot(map.vs.prof[,1+1]~map.vs.prof$Group.1, type='l', ylim=c(0,90),xlab="MONO", col='white')

for(i in 1:13){
  lines(map.vs.prof[,i+1]~map.vs.prof$Group.1,col=map.tree$col[i])
}

par(mfrow=c(1,1))
plot(colMeans(map.vs.prof[2:14])~map.tree$approxage[1:13],xlim=c(0,200), ylim=c(0,30))
points(colMeans(wcr.vs.prof[2:14])~wcr.tree$approxage[1:13], col='red')
points(colMeans(syv.vs.prof[2:21])~syv.tree$approxage[1:20], col='blue')



