#Read in matching data
fia.raw<-read.csv('species_plot_parameters_v0.1.csv')
convert<-read.csv('fia_conversion_v02-sgd.csv')
spatial<-read.csv('fia_paleongrid_albers.csv')

compare<-read.csv('plss_composition_alb_v0.9-10.csv')

#Combine datasets
sp.align<-merge(fia.raw,spatial, by.x='plt_cn', by.y='CN')
master<-merge(sp.align, convert, by.x='spcd', by.y='spcd')

vars.want<-c('plt_cn','dbh','basal area', 'n','density','x','y','cell','PalEON')
des.col<-which(names(master) %in% vars.want)
dat<-master[,des.col]

dat.order<-dat[order(dat$cell),]
rm('dat', 'sp.align','des.col','spatial','convert')

#Dumb for loop. Seriously so dumb. It takes like 8 minutes. 

companalog<-matrix(nrow=nrow(dat.order), ncol=ncol(compare)-3)
colnames(companalog)<-colnames(compare)[4:ncol(compare)]

diamanalog<-matrix(nrow=nrow(dat.order), ncol=ncol(compare)-3)
colnames(diamanalog)<-colnames(compare)[4:ncol(compare)]

densanalog<-matrix(nrow=nrow(dat.order), ncol=ncol(compare)-3)
colnames(densanalog)<-colnames(compare)[4:ncol(compare)]

library(beepr)
for (i in 1:nrow(dat.order)){ 
  taxn<-as.character(dat.order$PalEON[i])
  col<-which(colnames(companalog)==taxn)
  
  companalog[i,col]<-dat.order$n[i]
  diamanalog[i,col]<-dat.order$dbh[i]
  densanalog[i,col]<-dat.order$density[i]

  if (i%%5000==0){
    beep(2)
    Sys.sleep(1)
    print(i)
    if (i%%90000==0){beep(4)
      Sys.sleep(2)}
    }
}


coords<-aggregate(cbind(dat.order$x, dat.order$y),by=list(dat.order$cell),FUN=mean, na.rm=TRUE)


companalog.fin<-aggregate(companalog, by=list(dat.order$cell), FUN=sum, na.rm=TRUE)
fia.comp<-cbind(coords[2:3],companalog.fin)
colnames(fia.comp)[1:3]<-c('x','y','cell')

diamanalog.fin<-aggregate(diamanalog, by=list(dat.order$cell), FUN=mean, na.rm=TRUE)
fia.diam<-cbind(coords[2:3],diamanalog.fin)
colnames(fia.diam)[1:3]<-c('x','y','cell')

densanalog.fin<-aggregate(densanalog, by=list(dat.order$cell), FUN=mean, na.rm=TRUE)
fia.dens<-cbind(coords[2:3],densanalog.fin)
colnames(fia.dens)[1:3]<-c('x','y','cell')

rm('companalog.fin','diamanalog.fin','densanalog.fin', 'compare')

analogs<-list(companalog,diamanalog, densanalog)
rm('companalog','diamanalog','densanalog')



