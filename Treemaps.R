source('Prepare_forestdata.R')
library(treemap)
syv.tm.count<-data.frame(table(syv.forest$cc, syv.forest$species))

#try by basal area
syv.tm<-aggregate(syv.forest$ba, by=list(syv.forest$species,syv.forest$cc), FUN=sum)

# #A couple data typos
# syv.tm.clip<-syv.tm.count[c(1:3,5,7:30),]
# syv.tm.clip[3,3]<-syv.tm.count[3,3]+1
# syv.tm.clip[3,4]<-syv.tm.count[3,4]+1
# 
# syv.tm<-syv.tm.clip

syv.tm.clip<-syv.tm[c(1:13,15:18),]
#skips a couple small ACSA, should fix
syv.tm<-syv.tm.clip

myset<-t(col2rgb(c('orange','blue','dark red','forest green','gray')))

setvec<-rep(0,nrow(myset))
for(i in 1:nrow(myset)){
  col<-myset[i,]
  r<-col[1]
  g<-col[2]
  b<-col[3]
  setvec[i]<-rgb(r,g,b,maxColorValue=255)
}

#by count
#treemap(syv.tm.count, index=c('Var2', 'Var1'), vSize='Freq', palette=setvec)
#by ba:
treemap(syv.tm, index=c('Group.1','Group.2'), vSize='x', palette=setvec,bg.labels=0, 
        title='SYV',position.legend="bottom",fontsize.labels=c(0,18),fontface.labels='bold',fontsize.legend=22)


syv.tm1<-syv.tm
syv.tm1$splab<-'Other'
syv.tm1$splab[syv.tm1$Group.1=='acsa']<-'Sugar Maple'
syv.tm1$splab[syv.tm1$Group.1=='beal']<-'Yellow Birch'
syv.tm1$splab[syv.tm1$Group.1=='osvi']<-'Hophornbeam'
syv.tm1$splab[syv.tm1$Group.1=='tsca']<-'Eastern Hemlock'
treemap(syv.tm1, index=c('Group.1', 'splab'), vSize='x', palette=setvec,bg.labels=0, 
        title='Old Growth',fontsize.labels=c(0,18),fontface.labels='bold',fontsize.legend=22)



#Now WCR
wcr.tm.count<-data.frame(table(wcr.forest$cc, wcr.forest$species))
#try by basal area
wcr.tm<-aggregate(wcr.forest$ba, by=list(wcr.forest$species,wcr.forest$cc), FUN=sum)

myset<-t(col2rgb(c('orange','darkolivegreen3','dark red','navajowhite4','yellow','gray')))

setvec<-rep(0,nrow(myset))
for(i in 1:nrow(myset)){
  col<-myset[i,]
  r<-col[1]
  g<-col[2]
  b<-col[3]
  setvec[i]<-rgb(r,g,b,maxColorValue=255)
}


#treemap(wcr.tm.count, index=c('Var2', 'Var1'), vSize='Freq', palette=setvec)
treemap(wcr.tm, index=c('Group.1', 'Group.2'), vSize='x', palette=setvec, bg.labels=0, 
        title='WCR', position.legend="bottom", fontsize.labels=c(0,18), fontface.labels='bold', fontsize.legend=22)

wcr.tm1<-wcr.tm
wcr.tm1$splab<-'Other'
wcr.tm1$splab[wcr.tm1$Group.1=='acsa']<-'Sugar Maple'
wcr.tm1$splab[wcr.tm1$Group.1=='tiam']<-'Basswood'
wcr.tm1$splab[wcr.tm1$Group.1=='osvi']<-'Hophornbeam'
wcr.tm1$splab[wcr.tm1$Group.1=='frpe']<-'Green Ash'
wcr.tm1$splab[wcr.tm1$Group.1=='quru']<-'Red Oak'
treemap(wcr.tm1, index=c('Group.1', 'splab'), vSize='x', palette=setvec, bg.labels=0, 
        title='WCR', fontsize.labels=c(0,18), fontface.labels='bold', fontsize.legend=22)


#####barplots of other stuff

par(mfrow=c(1,3), mar=c(3,5,4,1))

barplot(c(sum(syv.forest$ba)/10000,sum(wcr.forest$ba)/10000), names.arg=c('SYV', 'WCR'), 
        col=c('dark gray','light gray'), main='Total Basal Area', ylab='Basal area (m2)',
        font.lab=2, font=2,font.main=2, cex.axis=2, cex.names=2, cex.main=2, cex.lab=2)
 
syv.lai<-4
wcr.lai<-3.7
#these are esimated visually from MC
barplot(c(syv.lai, wcr.lai),names.arg=c('SYV', 'WCR'), col=c('dark gray','light gray'), main='Leaf Area Index', ylab='LAI (unitless)', font.lab=2, font=2,font.main=2, cex.axis=2, cex.names=2, cex.main=2, cex.lab=2)

syv.sph<-(nrow(syv.forest)/5024)*10000
wcr.sph<-(nrow(wcr.forest)/4307)*10000  #4307 accounts for the areas un-surveyable becasue of the road

barplot(c(syv.sph, wcr.sph),names.arg=c('SYV', 'WCR'), col=c('dark gray','light gray'), main='Stem Density', ylab='Density (stems/ha)', font.lab=2, font=2,font.main=2, cex.axis=2, cex.names=2, cex.main=2, cex.lab=2)


par(mfrow=c(2,1), mar=c(4,4.5,2.1,1))
hist(syv.forest$dbh, breaks=seq(from=5,to=85,length.out=17), col='dark gray', ylim=c(0,100), 
     axes=FALSE, ylab='# Trees', xlab='', main='SYV', cex.lab=1.5, font.lab=2, cex.main=1.2)
axis(1,at=seq(from=5, to=85,by=10), labels=seq(from=5, to=85,by=10), cex.axis=1.5, font=2)
axis(2,at=seq(0,100,20), labels=seq(0,100,20), cex.axis=1.5, font=2)

hist(wcr.forest$dbh,breaks=seq(from=5,to=85,length.out=17), col='light gray',ylim=c(0,100), 
     axes=FALSE, ylab='# Trees', xlab='DBH (cm)', main='WCR', cex.lab=1.5, font.lab=2, cex.main=1.2)
axis(1,at=seq(from=5, to=85,by=10), labels=seq(from=5, to=85,by=10), cex.axis=1.5, font=2)
axis(2,at=seq(0,100,20), labels=seq(0,100,20), cex.axis=1.5, font=2)

#reset graphical params
par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))



