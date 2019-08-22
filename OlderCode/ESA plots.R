#ESA figures reorganization


#Make sure the plot window is large. 
#TS_analyses may error out and/or some plots will not plot if the window is too small

####Prep for plots####

#TD difference, SW differences
source('TS_analyses.R', verbose=FALSE)

#Tree- and plot-level barplots
source('Figures_v1.R', verbose=FALSE)

#Sapwood area change regionwide
sapwoodplot<-FALSE 
#Decides whether you run the whole fia/pls prep scripts needed for regional sapwood change
#FALSE by default because (1) it takes forever due to a dumb for loop, and (2) it won't run on some machines (data too large)
if(sapwoodplot==TRUE){
source('SapfluxScale_pls_fia', verbose=FALSE) 
#will take forever due to a horrendously inefficient for loop.
#If it errors out, you must do parts of the FIA_PROCESS script piecewise. Will revise someday...
}

#Seasonal profiles (polygons), transp difference, 
source('SeasonProcess.R', verbose=FALSE)


#####


####Actual plotting####
dev.off()
###Sapwood area plot

par(mfrow=c(1,1))
barplot(height=all$X2[order(all$X2)], col=all$col[order(all$X2)],names.arg=all$Taxon[order(all$X2)], ylim=c(-2e11,2.5e11), ylab='Regional Sapwood Change')
abline(h=0, lwd=3)
box(lwd=3)
text(8,1.8e11, "Gained", font=2)
text(3,-1.4e11, "Lost", font=2)



###Stand-level barplots of transp
if(sapwoodplot==TRUE){
par(mfrow=c(1,2))
#Zoomed out
boxplot(colMeans(wcr.forest.day[gs,], na.rm=TRUE)~wcr.forest$species, col=c('orange', 'yellow green','dark red','orange4','yellow','gray'), ylim=c(0,300))
boxplot(colMeans(syv.forest.day[gs,], na.rm=TRUE)~syv.forest$species, col=c('orange','blue','dark red','forest green','gray'), ylim=c(0,300))
#Zoomed in
boxplot(colMeans(wcr.forest.day[gs,], na.rm=TRUE)~wcr.forest$species, col=c('orange', 'yellow green','dark red','orange4','yellow','gray'), ylim=c(0,100))
boxplot(colMeans(syv.forest.day[gs,], na.rm=TRUE)~syv.forest$species, col=c('orange','blue','dark red','forest green','gray'), ylim=c(0,100))
}

###Transpiration seasonal profile

par(mfrow=c(2,1))
#SYV
plot(syv.sm.tot[gs], col='white', ylim=c(0,6000), xlim=c(min(gs),max(gs)), main='SYV', ylab="Sap flow (L day-1)", xlab='DOY', font=2, font.lab=2)
polygon(y=c(syv.sm.tot[gs]+syv.hl.tot[gs]+syv.yb.tot[gs]+syv.hb.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='dark red')
polygon(y=c(syv.sm.tot[gs]+syv.hl.tot[gs]+syv.yb.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='blue')
polygon(y=c(syv.sm.tot[gs]+syv.hl.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='forest green')
polygon(y=c(syv.sm.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)), col='orange')
#mask out weird smoothing
polygon(x=c(156,156,177,177),y=c(0,5000,5000,0), col='white', border=NA)
polygon(x=c(75,75,110,110),y=c(0,5000,5000,0), col='white', border=NA)
#WCR
plot(wcr.sm.tot[gs], col='white', ylim=c(0,6000), xlim=c(min(gs),max(gs)),main='WCR', ylab="Sap flow (L day-1)", xlab='DOY', font=2, font.lab=2)
polygon(y=c(wcr.sm.tot[gs]+wcr.ab.tot[gs]+wcr.ga.tot[gs]+wcr.hb.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='dark red')
polygon(y=c(wcr.sm.tot[gs]+wcr.ab.tot[gs]+wcr.ga.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='yellow green')
polygon(y=c(wcr.sm.tot[gs]+wcr.ab.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='yellow')
polygon(y=c(wcr.sm.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)), col='orange')
#mask out weird smoothing
polygon(y=c(0,6000,6000,0), x=c(247,247,252,252), col='white',border=NA)
polygon(y=c(0,6000,6000,0), x=c(39,39,46,46), col='white',border=NA)
polygon(y=c(0,6000,6000,0), x=c(330,330,365,365), col='white',border=NA)



###Transpiration difference silhouettes
par(mfrow=c(1,1))
plot(wcr.sm.tot[gs], col='white', ylim=c(0,6000), xlim=c(min(gs),max(gs)), ylab="Sap flow (L day-1)", xlab='DOY', font=2, font.lab=2)
polygon(y=c(wcr.sm.tot[gs]+wcr.ab.tot[gs]+wcr.ga.tot[gs]+wcr.hb.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='dark graY')
polygon(y=c(syv.sm.tot[gs]+syv.hl.tot[gs]+syv.yb.tot[gs]+syv.hb.tot[gs],rep(0,length(gs))),x=c(gs,rev(gs)),col='black')

polygon(x=c(156,156,177,177),y=c(0,3000,3000,0), col='dark gray', border=NA)
polygon(x=c(170,170,177,177),y=c(3000,4500,4500,3000), col='dark gray', border=NA)



###Difference in TD
par(mfrow=c(1,1))
plotsmooth(dat1=wcr.master,dat2=syv.master, ndays=7,varset=c("TD"), 
           allplot='DIF', set.par=FALSE) #Replicates ESA figure; actually should use just daytime, which results in basically the same trend but smoother



###Transpiration difference (color code)

par(mfrow=c(1,1))
plot(diffs.gf, type='l', main='WCR-SYV', ylab='Transp. difference (L day-1)', lwd=3, lty=2, font=2, font.lab=2, xlab='Day of Year', col='white')
lines(diffs.gf, col='dark red', lwd=2,lty=2);lines(diffs, lwd=3, col='dark red')
clip(0, length(diffs.gf),0, max(diffs.gf, na.rm=TRUE))
lines(diffs.gf, col='forest green', lwd=2,lty=2);lines(diffs, lwd=3, col='forest green')
abline(h=0, lwd=2)


###Correlation plot
#Does not quite replicate ESA plot. Working on it....
par(mfrow=c(1,1))
corrplot(compday,method="shade",diag=FALSE,type='upper', addCoef.col='black',number.cex=0.7,
         tl.col='black',tl.cex=1, mar=c(1,1,1,1), tl.srt=45, font=2, outline=TRUE, addgrid.col='black')



###Radiation plots

par(mfrow=c(1,1))
plotsmooth(dat1=wcr.twr[dayind,],dat2=syv.twr[dayind,], ndays=7,varset=c("NETRAD_1", 'SW_IN', 'SW_OUT'), allhr=FALSE, 
           allplot='DIF', set.par=FALSE)


###Albedo difference

plot(albdiff.sm, type='l', lwd=3, ylim=c(-0.05,0.15), col='forest green', 
     font=2, font.lab=2, ylab='Albedo change (unitless)', xlab='Day of Year')
abline(h=0)


