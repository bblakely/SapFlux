
source('SapFlow_Calc.R')
source('Sapflow_plotscale.R')

#barplot(height=(aggregate(syv.stand$flow.mean, by=list(syv.stand$SPP), FUN=mean, na.rm=TRUE)[,2]), names.arg=(aggregate(syv.stand$flow.mean, by=list(syv.stand$SPP), FUN=mean, na.rm=TRUE)[,1]), ylim=c(0,50))
#barplot(height=(aggregate(wcr.stand$flow.mean, by=list(wcr.stand$SPP), FUN=mean, na.rm=TRUE)[,2]), names.arg=(aggregate(wcr.stand$flow.mean, by=list(wcr.stand$SPP), FUN=mean, na.rm=TRUE)[,1]), ylim=c(0,50))

#flux
#barplot(syv.flux.sp[,2], names.arg=syv.flux.sp[,1], ylim=c(0, 1.3e-05))
#barplot(wcr.flux.sp[,2], names.arg=wcr.flux.sp[,1], ylim=c(0, 1.3e-05))

t.test(syv.stand$flow.mean, wcr.stand$flow.mean)

#sum
barplot(height=(aggregate(syv.stand$flow.mean, by=list(syv.stand$SPP), FUN=sum, na.rm=TRUE)[,2]), 
        names.arg=(aggregate(syv.stand$flow.mean, by=list(syv.stand$SPP), FUN=mean, na.rm=TRUE)[,1]),
        ylim=c(0,3000), col=c('orange','blue','dark red','forest green','gray'), ylab='L / day')
barplot(height=(aggregate(wcr.stand$flow.mean, by=list(wcr.stand$SPP), FUN=sum, na.rm=TRUE)[,2]), 
        names.arg=(aggregate(wcr.stand$flow.mean, by=list(wcr.stand$SPP), FUN=mean, na.rm=TRUE)[,1]),
        ylim=c(0,3000),col=c('orange','darkolivegreen3','dark red','brown4','yellow','gray'),ylab='L / day')

#total
barplot(height=c(sum(syv.stand$flow.mean),sum(wcr.stand$flow.mean)), names.arg=c('SYV','WCR'), col=c('dark gray', 'light gray'))


#boxplots of flow
boxplot(syv.stand$flow.mean~syv.stand$SPP, ylim=c(-5, 100), main='Tree Sap Flow by Species: SYV', 
        col=c('orange', 'blue','dark red','forest green','gray'), ylab='L day-1',
        cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
text(c(1:4), c(80,90,26,38), c('ACD','B','AC','AD'), cex=0.8, font=2)

boxplot(wcr.stand$flow.mean~wcr.stand$SPP, ylim=c(-5, 100), main='Tree Sap Flow by Species: WCR', 
        col=c('orange','darkolivegreen3','dark red','brown4','yellow', 'gray'),ylab='L day-1',
        cex.axis=1.5,cex.lab=1.5,cex.main=1.5,font.axis=2,font.main=2,font.lab=2)
text(c(1:5),c(60,15,15,12,60), c('A', rep('BCD',3), 'E'), cex=0.8, font=2)

#barplot of flux rates

par(mfrow=c(1,1), mar=c(4,5,3,2))
wcr.fluxspp<-c(wcr.flux.sp$x[1:3],wcr.flux.sp$x[2],wcr.flux.sp$x[4], mean(wcr.flux.sp$x))
centers<-barplot(wcr.fluxspp, col=c("orange","darkolivegreen3","dark red","navajowhite4","yellow", "gray"))

barplot(wcr.fluxspp, col=c("orange","darkolivegreen3","dark red","navajowhite4","yellow", "gray"), 
        ylim=c(0,2e-05), names.arg=c("ACSA","FRPE","OSVI","(QURU)","TIAM","UNKNOWN"), ylab="Flux rate (ms-1)", main='WCR',
        font.lab=2,font.axis=2,cex.lab=1.5,cex.axis=1.5, cex.names=1.5, cex.main=1.5)


# The horror
segments(centers[1],wcr.flux.sp$x[1]-sd(wcr.plot.info$flux.mean[wcr.plot.info$SPP=="ACSA"]),centers[1],wcr.flux.sp$x[1]+sd(wcr.plot.info$flux.mean[wcr.plot.info$SPP=="ACSA"]), lwd=2)
segments(centers[2],wcr.flux.sp$x[2]-sd(wcr.plot.info$flux.mean[wcr.plot.info$SPP=="FRPE"]),centers[2],wcr.flux.sp$x[2]+sd(wcr.plot.info$flux.mean[wcr.plot.info$SPP=="FRPE"]), lwd=2)
segments(centers[3],wcr.flux.sp$x[3]-sd(wcr.plot.info$flux.mean[wcr.plot.info$SPP=="OSVI"]),centers[3],wcr.flux.sp$x[3]+sd(wcr.plot.info$flux.mean[wcr.plot.info$SPP=="OSVI"]), lwd=2)
segments(centers[4],wcr.flux.sp$x[2]-sd(wcr.plot.info$flux.mean[wcr.plot.info$SPP=="FRPE"]),centers[4],wcr.flux.sp$x[2]+sd(wcr.plot.info$flux.mean[wcr.plot.info$SPP=="FRPE"]), lwd=2)
segments(centers[5],wcr.flux.sp$x[4]-sd(wcr.plot.info$flux.mean[wcr.plot.info$SPP=="TIAM"]),centers[5],wcr.flux.sp$x[4]+sd(wcr.plot.info$flux.mean[wcr.plot.info$SPP=="TIAM"]), lwd=2)

###SYV
syv.fluxspp<-c(syv.flux.sp$x,mean(syv.flux.sp$x[1:3]))
centers<-barplot(syv.fluxspp, col=c("orange","blue","dark red","forest green","gray"))
barplot(syv.fluxspp, col=c("orange","blue","dark red","forest green","gray"), ylim=c(0,2e-05), 
        names.arg=c("ACSA","BEAL","OSVI","TSCA", "UNKNOWN"), ylab="Flux rate (ms-1)", main='SYV',
        font.lab=2,font.axis=2,cex.lab=1.5,cex.axis=1.5, cex.names=1.5, cex.main=1.5)
# The horror
segments(centers[1],syv.flux.sp$x[1]-sd(syv.plot.info$flux.mean[syv.plot.info$SPP=="ACSA"]),centers[1],syv.flux.sp$x[1]+sd(syv.plot.info$flux.mean[syv.plot.info$SPP=="ACSA"]), lwd=2)
segments(centers[2],syv.flux.sp$x[2]-sd(syv.plot.info$flux.mean[syv.plot.info$SPP=="BEAL"]),centers[2],syv.flux.sp$x[2]+sd(syv.plot.info$flux.mean[syv.plot.info$SPP=="BEAL"]), lwd=2)
segments(centers[3],syv.flux.sp$x[3]-sd(syv.plot.info$flux.mean[syv.plot.info$SPP=="OSVI"]),centers[3],syv.flux.sp$x[3]+sd(syv.plot.info$flux.mean[syv.plot.info$SPP=="OSVI"]), lwd=2)
segments(centers[4],syv.flux.sp$x[4]-sd(syv.plot.info$flux.mean[syv.plot.info$SPP=="TSCA"]),centers[4],syv.flux.sp$x[4]+sd(syv.plot.info$flux.mean[syv.plot.info$SPP=="TSCA"]), lwd=2)



#text(c(1:5),c(60,15,15,12,60), c('A', rep('BCD',3), 'E'), cex=0.8)

#Try vioplots?

# no.nan.st<-lapply(wcr.stand$flow.mean,na.omit)
# # vioplot(unlist(no.nan[1]),unlist(no.nan[2]),unlist(no.nan[3]),unlist(no.nan[4]),unlist(no.nan[5]),unlist(no.nan[6]),unlist(no.nan[7]),unlist(no.nan[8]),unlist(no.nan[9]),
# #         unlist(no.nan[10]),unlist(no.nan[11]),unlist(no.nan[12]),unlist(no.nan[13]),unlist(no.nan[14]), col='white')
# 
# plot(0:4,xlim=c(0,5), ylim=c(-2,100), axes=FALSE, ylab="Flow", xlab="Species", col='white')
# spplist<-c("acsa","tiam","osvi","frpe")
# cols<-c("orange","yellow","dark red","darkolivegreen3")
# for (i in 1:4) { 
#   vioplot(unlist(no.nan.st[wcr.stand$SPP==spplist[i]]), at = i, add = T, col = cols[i])
# }
# axis(side=1,at=1:4,labels=spplist)
# axis(side=2,at=seq(from=0,to=100,length.out=5),labels=seq(from=0,to=100,length.out=5))
# 
# 
# ##SYV
# no.nan.st<-lapply(syv.stand$flow.mean,na.omit)
# # vioplot(unlist(no.nan[1]),unlist(no.nan[2]),unlist(no.nan[3]),unlist(no.nan[4]),unlist(no.nan[5]),unlist(no.nan[6]),unlist(no.nan[7]),unlist(no.nan[8]),unlist(no.nan[9]),
# #         unlist(no.nan[10]),unlist(no.nan[11]),unlist(no.nan[12]),unlist(no.nan[13]),unlist(no.nan[14]), col='white')
# 
# plot(0:4,xlim=c(0,5), ylim=c(-2,100), axes=FALSE, ylab="Flow", xlab="Species", col='white')
# spplist<-c("acsa","beal","osvi","tsca")
# cols<-c("orange","blue","dark red","forest green")
# for (i in 1:4) { 
#   vioplot(unlist(no.nan.st[syv.stand$SPP==spplist[i]]), at = i, add = T, col = cols[i])
# }
# axis(side=1,at=1:4,labels=spplist)
# axis(side=2,at=seq(from=0,to=100,length.out=5),labels=seq(from=0,to=100,length.out=5))
# 
# 



# #boxplots of DBH
# boxplot(syv.stand$DBH~syv.stand$SPP, main='SYV DBH by Species', ylim=c(-2,100),col=c('orange', 'blue','dark red','forest green','gray'))
# #text(c(1:4), c(80,90,26,38), c('ACD','B','AC','AD'), cex=0.8)
# 
# boxplot(wcr.stand$DBH~wcr.stand$SPP, main='WCR DBH by Species', ylim=c(-2,100), col=c('orange','darkolivegreen3','dark red','brown4','yellow', 'gray'))
# #text(c(1:5),c(60,15,15,12,60), c('A', rep('BCD',3), 'E'), cex=0.8)
# 

#boxplots of SWA
boxplot(syv.stand$SWA~syv.stand$SPP, main='SYV Sapwood Area by Spp.', ylim=c(-2,2000),col=c('orange', 'blue','dark red','forest green','gray'))
#text(c(1:4), c(80,90,26,38), c('ACD','B','AC','AD'), cex=0.8)

boxplot(wcr.stand$SWA~wcr.stand$SPP, main='WCR Sapwood by Spp.', ylim=c(-2,2000), col=c('orange','darkolivegreen3','dark red','brown4','yellow', 'gray'))
#text(c(1:5),c(60,15,15,12,60), c('A', rep('BCD',3), 'E'), cex=0.8)



#Plot level
boxplot(syv.plot.sort$flux.mean, wcr.plot.sort$flux.mean, main='plot flux', names=c('SYV', 'WCR'), ylim=c(-1e-05,3e-05))
boxplot(syv.plot.sort$flow.mean, wcr.plot.sort$flow.mean, main='plot flow', names=c('SYV', 'WCR'), ylim=c(-5,80))
#This tells me I samples unusually large trees at SYV

boxplot(syv.stand$flow.mean, wcr.stand$flow.mean, main='stand flow', names=c('SYV', 'WCR'), ylim=c(-5,100))

#Stand level BA, SWA
barplot(height=c(sum(syv.stand$BA), sum(wcr.stand$BA)*1.17), names.arg=c('SYV', 'WCR'), col=c('dark gray', 'light gray'), main='Basal area (total, cm2)', ylim=c(0,175000))

barplot(height=c(sum(syv.stand$SWA), sum(wcr.stand$SWA)*1.17), names.arg=c('SYV', 'WCR'), col=c('dark gray', 'light gray'), main='Sapwood area (total, cm2)', ylim=c(0,175000))


#Can I get stand flux?
#####
syv.stand.flux<-syv.stand
syv.stand.flux$flux.mean<-NA
syv.stand.flux$flux.mean[syv.stand.flux$SPP=='acsa']<-syv.flux.sp$x[1]
syv.stand.flux$flux.mean[syv.stand.flux$SPP=='beal']<-syv.flux.sp$x[2]
syv.stand.flux$flux.mean[syv.stand.flux$SPP=='osvi']<-syv.flux.sp$x[3]
syv.stand.flux$flux.mean[syv.stand.flux$SPP=='tsca']<-syv.flux.sp$x[4]
syv.stand.flux$flux.mean[syv.stand.flux$SPP=='uk']<-mean(syv.flux.sp$x)

wcr.stand.flux<-wcr.stand
wcr.stand.flux$flux.mean<-NA
wcr.stand.flux$flux.mean[wcr.stand.flux$SPP=='acsa']<-wcr.flux.sp$x[1]
wcr.stand.flux$flux.mean[wcr.stand.flux$SPP=='frpe']<-wcr.flux.sp$x[2]
wcr.stand.flux$flux.mean[wcr.stand.flux$SPP=='osvi']<-wcr.flux.sp$x[3]
wcr.stand.flux$flux.mean[wcr.stand.flux$SPP=='tiam']<-wcr.flux.sp$x[4]
wcr.stand.flux$flux.mean[wcr.stand.flux$SPP=='quru']<-wcr.flux.sp$x[2]
wcr.stand.flux$flux.mean[wcr.stand.flux$SPP=='uk']<-mean(wcr.flux.sp$x)
#####
##Doesn't really work becasue all repeats.
#boxplot(syv.stand.flux$flux.mean, wcr.stand.flux$flux.mean, ylim=c(2e-06,2e-05))


###stacked bars by spp
syv.sp.flow<-aggregate(syv.stand$flow.mean, by=list(syv.stand$SPP),FUN=sum)
wcr.sp.flow<-aggregate(wcr.stand$flow.mean, by=list(wcr.stand$SPP),FUN=sum)

#make dataframes friends. is pain in ass.

combo<-merge(x=syv.sp.flow,y=wcr.sp.flow, by='Group.1', all=TRUE)
combo<-combo[,2:3]
combo[2,2]<-0
combo[4,2]<-0
combo[6:8,1]<-0
#put unknown on top every time
combo<-rbind(combo[1:4,],combo[6:8,],combo[5,])

barplot(as.matrix(combo), col=c('orange','blue','dark red', 'forest green','darkolivegreen3','navajowhite4','yellow', 'gray'),
        main='Total Sap Flow',names.arg=c('SYV','WCR'),ylab='Sap Flow (L day-1)', ylim=c(0,7000), 
        cex.axis=2, cex.lab=2,cex.main=2.5, cex.names=2, font.axis=2,font.lab=2,font.main=2,font.names=2)
legend(x=0.2,y=7000, legend=c('ACSA','BEAL','OSVI','TSCA','FRPE','TIAM','UNKNOWN'), ncol=4, cex=0.93, 
       fill=c('orange','blue','dark red', 'forest green','darkolivegreen3','yellow', 'gray'),
       x.intersp=0.5)

#Make giant legend
par(mfrow=c(1,1))
plot(seq(1,50,length.out=200),1:200, col='white')

legend(x=0.2,y=200, legend=c('ACSA','BEAL','OSVI','TSCA','FRPE','TIAM','UNKNOWN'), ncol=1, cex=2, 
       fill=c('orange','blue','dark red', 'forest green','darkolivegreen3','yellow', 'gray'),
       x.intersp=0.5)



#33%
       
# ##AGAIN BUT FOR PER BASAL AREA/SAPWOOD AREA
# par(mfrow=c(1,2))
# 
# total.sap<-colSums(combo)
# ba.site<-c(sum(syv.stand$BA), sum(wcr.stand$BA))
# sw.site<-c(sum(syv.stand$SWA), sum(wcr.stand$SWA))
# 
# sap.ba<-total.sap/(ba.site/10000)
# sap.sw<-total.sap/(sw.site/10000)
#   
#barplot(sap.ba, col=c('dark gray','light gray'), main='Sap Flow per Basal Area',names.arg=c('SYV','WCR'),ylab='Sap Flow (L/day*m2)', ylim=c(0,400))
# #63%
# 
# #barplot(sap.sw, col=c('dark gray','light gray'), main='Sap Flow per Sapwood Area',names.arg=c('SYV','WCR'),ylab='Sap Flow (L/day*m2)', ylim=c(0,800))
# #68%




t.test(all.plot.info$flow.mean[all.plot.info$SPP=="TSCA" & all.plot.info$CC=='C'|all.plot.info$CC=='D'],
       all.plot.info$flow.mean[all.plot.info$SPP!="TSCA" & all.plot.info$CC=='C'|all.plot.info$CC=='D'], "less" )




