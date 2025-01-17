

#Data
if (!exists('fia.diam')){ source('fia_process.R')} #Read in and process FIA data, Script takes ~10min

fia.comp.raw<-fia.comp
fia.dens.raw<-fia.dens
fia.diam.raw<-fia.diam

rm(list=setdiff(ls(), c('fia.comp.raw','fia.dens.raw','fia.diam.raw', 'wcr.master')))

if(!exists('wcr.master')){source('Prepare_Data.R');source('Refine_TowerData.R')} #Read in and process tower and site data, Script takes a few min.

#Read in paleo data
pls.comp.raw<-read.csv('plss_composition_alb_v0.9-10.csv')
pls.dens.raw<-read.csv('plss_density_alb_v0.9-10.csv')
pls.diam.raw<-read.csv('plss_diam_alb_v0.9-10.csv')
pls.basal.raw<-read.csv('plss_basal_alb_v0.9-10.csv')

#('plss_diam_veil_alb_v0.9-10.csv')
#('plss_diam_alb_v0.9-10.csv')


#Thin FIA to paleo and vice-versa
fia.cells<-fia.comp.raw$cell
pls.cells<-pls.comp.raw$cell

#fia.cells<-fia.dens.raw$cell
#pls.cells<-pls.dens.raw$cell

fia.match<-which(fia.cells%in%pls.cells)
pls.match<-which(pls.cells%in%fia.cells)


#Matched datasets

fia.comp<-fia.comp.raw[fia.match,]
pls.comp<-pls.comp.raw[pls.match,]

fia.diam<-fia.diam.raw[fia.match,]
pls.diam<-pls.diam.raw[pls.match,]

fia.dens<-fia.dens.raw[fia.match,]
pls.dens<-pls.dens.raw[pls.match,]

#Plotting function for quick checks
quickplot<-function(var){
  rast<-rasterFromXYZ(cbind(pls.comp$x,pls.comp$y,var))
  plot(rast)
}


#Diameter weighting issue
pls.diam.orig<-pls.diam
pls.diam<-pls.diam/pls.comp #Check this! Makes reasonble values.



swa.calc<-function(comp,diam,dens){
  
  #clip off georeference info
  comp.dat<-comp[,4:ncol(comp)]
  diam.dat<-diam[,4:ncol(diam)]
  dens.dat<-dens[,4:ncol(dens)]
  
  #sapwood area space (do I need this?)
  swa.eachtree<-data.frame(matrix(dat=0,nrow(diam.dat),ncol(diam.dat))) #dat was NA
  colnames(swa.eachtree)<-colnames(fia.diam[4:ncol(fia.diam)]) #Is FIA diameter the same as PLS diameter?
  
  #Assign sapwood area
  #####
  #Filler value (maple)
  swa.eachtree<-0.312*diam.dat^2.07
 
  #Ash
  swa.eachtree[,2]<-0.21*diam.dat[,2]^2.18 #Not sure where this comes from...
  #Basswood
  swa.eachtree[,3]<-0.77*diam.dat[,3]^1.868 #Actually aspen
  #Beech
  swa.eachtree[,4]<-0.67*diam.dat[,4]^1.92 #American Beech. High.
  #Birch
  swa.eachtree[,5]<-1.17*diam.dat[,5]^1.79 #Yellow Birch
  #Cedar/Juniper
  swa.eachtree[,8]<-(3.14*(0.5*diam.dat[,8])^2)-(3.14*(0.5*(diam.dat[,8]-1.5))^2) #White Cedar; back-calculated from Ewers. Low.
  #Ironwood(same as maple for now)
  swa.eachtree[,16]<-0.312*diam.dat[,16]^2.07  #Sugar/Red maple combined
  #Maple
  swa.eachtree[,17]<-0.312*diam.dat[,17]^2.07  #Sugar/Red maple combined
  #Oak
  swa.eachtree[,19]<-3.24*diam.dat[,19]-10.24 #Red oak
  #Pine
  swa.eachtree[,21]<-10^(-0.221+2.145*log10(diam.dat[,21])-(0.004*35)) #Jack Pine
  #Poplar
  swa.eachtree[,22]<-0.77*diam.dat[,22]^1.868  #Bigtooth Aspen

  #Evergreen Fillers
  EG.sub<-c(12,14,23,25) #fir,hemlock,spruce,tamarack
  swa.eachtree[,EG.sub]<-4.2568*diam.dat[,EG.sub]^1.37 #Eastern Hemlock
  #####
  
  #Multiply out across all stems to get total sapwood area
  dens.stem<-dens.dat*6400  #assumes PLS is in stems per hectare (check!)
  swa.tot<-swa.eachtree*dens.stem*0.73 #I think the .75 is for radial profile
  
  #clip extreme value for plotting
  ext<-quantile(swa.tot,0.999, na.rm=TRUE)
  swa.tot[swa.tot>ext]<-NA
  
  return(swa.tot)
}

fia.swa<-swa.calc(fia.comp,fia.diam,fia.dens)
pls.swa.orig<-swa.calc(pls.comp,pls.diam.orig,pls.dens)
pls.swa<-swa.calc(pls.comp,pls.diam,pls.dens)

quickplot(rowSums(fia.swa, na.rm=TRUE))
quickplot(rowSums(pls.swa, na.rm=TRUE))
#quickplot(rowSums(pls.swa.orig, na.rm=TRUE))

georef<-fia.comp[,1:2]

fluxrates.lut<-read.csv('FLUX_LUT.csv')

#Now transpiration
transp.calc<-function(swa, dens, fluxdf=fluxrates.lut){  #swa: sapwood calculated above. 
  
  #make general references
  georef<-dens[,1:2]
  dimref<-dens[,4:ncol(dens)]
  
  #Read in lit/measured transpiration rates
  fluxrates.lut<-fluxdf
  
  #make space and name it
  flow.tot<-data.frame(matrix(0, nrow(dimref),ncol(dimref))) #was NA
  colnames(flow.tot)<-colnames(dimref)
  
  #fill with generalizations
  flow.tot[,]<-swa*fluxrates.lut$FLUX[fluxrates.lut$Taxon=='Hardwood']*86400  #Baseline: all things mean of hardwoods. The 86400 is for seconds -> days
  
  EG.sub<-c(12,14,23,25) #fir,hemlock,spruce,tamarack
  flow.tot[,EG.sub]<-swa[,EG.sub]*fluxrates.lut$FLUX[fluxrates.lut$Taxon=='Evergreen']*86400 #Step 2, evergreens mean of evergreens
  
  
  #assigning
  for(i in 1:(nrow(fluxrates.lut)-2)){
    flow.tot[,fluxrates.lut$COL[i]]<-swa[,fluxrates.lut$COL[i]]*fluxrates.lut$FLUX[i]*86400
  }
  
  #remove places with no trees
  flow.cell<-cbind(georef, rowSums(flow.tot, na.rm=TRUE))
  flow.tax<-cbind(georef, flow.tot)
  flow.cell[rowSums(dens, na.rm=TRUE)<10,3]<-0 #was NA
  colnames(flow.cell)[3]<-'transp'
  
  #clip crazy values
  #ext<-quantile(flow.cell$transp,c(0.03,0.97))
  #flow.cell$transp[flow.cell$transp>ext[2] | flow.cell$transp<ext[1]]<-NA
  
  #return(flow.cell)
  return(cbind(georef, flow.tot))
}

fia.transp<-transp.calc(fia.swa,fia.dens)
pls.transp<-transp.calc(pls.swa,pls.dens)

fia.transp.tot<-cbind(georef,rowSums(fia.transp[3:31], na.rm=TRUE)); colnames(fia.transp.tot)[3]<-"Transpiration"
pls.transp.tot<-cbind(georef,rowSums(pls.transp[3:31], na.rm=TRUE)); colnames(pls.transp.tot)[3]<-"Transpiration"

####plotting####
library(maps)
library(raster)
library(RColorBrewer)
library(zoo)

breakset.abs<-c(0,2e8,4e8,6e8,8e8,1e9,1.2e9)
breakset.diff<-c(-1.2e9,-8e8,-4e8,-2e8,2e8,4e8,8e8,1.2e9)
plotnice<-function(toplot, breakset=breakset.abs, colset='BrBG', title='sapflux'){
  
  quants<-quantile(toplot[,3],c(0.03, 0.97), na.rm=TRUE)

  flow.cell.vis<-toplot
  bignums<-which(flow.cell.vis[,3]>quants[2])
  flow.cell.vis[bignums,3]<-quants[2] #Clip hemlock mountain to max

  rast.vis<-rasterFromXYZ(flow.cell.vis)

#breaks<-seq(from=quants[1], to=quants[2],length.out=8)
  pal<-brewer.pal(7,colset) #RdYlGn
  plot(rast.vis, breaks=breakset, col=pal, main=title)
}

par(mar=c(4,4,4,1))
plotnice(pls.transp.tot/(24*64*10), breakset.abs/(24*64*10), title='Historic')
plotnice(fia.transp.tot/(24*64*10), breakset.abs/(24*64*10),title='Modern')

#real messy conversion. 24 converts days - > hours, 64 converts 8km2 -> 1km2
#10 is a weird one; original numbers multiply m/s * cm2.
# m | cm2 |  m2     ->   m3
# s  |     |10000cm2 -> s * 10000
#But now we might as well do L  (also kg) so
#   m3     | 1000 L ->   L
# s *10000 |   m3   -> s * 10
#Current plots (pre-1/16) don't have the 10 and are in dL / km2h
#Plots now in L/km2h

fia.transp.write<-fia.transp.tot$Transpiration/(24*6400*10);pls.transp.write<-pls.transp.tot$Transpiration/(24*6400*10)
fia.transp.wrfile<-fia.transp.tot; fia.transp.wrfile$Transpiration<-fia.transp.write
pls.transp.wrfile<-pls.transp.tot; pls.transp.wrfile$Transpiration<-pls.transp.write

write.csv(fia.transp.wrfile, "Modern_transpiration.csv");write.csv(pls.transp.wrfile, "Historical_transpiration.csv")

#diff.transp<-cbind(georef,fia.transp$transp-pls.transp$transp)
diff.transp.pr<-cbind(georef,fia.transp.tot[,3]-pls.transp.tot[,3])

#plotnice(diff.transp/(24*64*10),breakset.diff/(24*64*10), colset="RdYlGn", title='Difference (Modern-Historic)')
plotnice(diff.transp.pr/(24*64*10),breakset.diff/(24*64*10), colset="RdYlGn", title='Difference (Modern-Historic)')


colnames(diff.transp.pr)[3]<-"tdiff"
write.csv(diff.transp.pr, "pls.fia.transp.diff.csv")

sp.plot='no'  #Do you want regressions by species plots?

#####Individual taxa converstion chart####
if(sp.plot=='yes'){

pls.pctcomp<-pls.comp[4:32]/rowSums(pls.comp[4:32])
fia.pctcomp<-fia.comp[4:32]/rowSums(fia.comp[4:32])
pctcomp.diff<-fia.pctcomp-pls.pctcomp

for(i in 1:29){
  chg<-which(abs(pctcomp.diff[,i])!=0 )
  fit<-lm(diff.transp[chg,3]~pctcomp.diff[chg,i])
  if(length(chg)<5){
    sig<-999
    r2<-999
  }else{
    sig<-summary(fit)$coefficients[2,4]
    r2<-summary(fit)$r.squared}
  if(sig<0.01& r2>0.02){ #& r2>0.02
    print(paste(colnames(pctcomp.diff)[i], "is significant with r2", r2))
    plot(diff.transp[chg,3]~pctcomp.diff[chg,i], main=colnames(pctcomp.diff)[i])
    int<-unname(fit[[1]][1])
    sl<-unname(fit[[1]][2])
    abline(int,sl,col='red')
  }
}

}


pls.sums<-colSums(pls.transp.cols, na.rm=TRUE)
fia.sums<-colSums(fia.transp.cols, na.rm=TRUE)

ordered.taxa<-sort((fia.sums-pls.sums)[3:31])
barplot(ordered.taxa[which(abs(ordered.taxa)>1e10)])
abline(h=0, lwd=3)
text(5, 3e11, "Greater Modern")
text(12, -1.5e11, "Greater Historic")

par(mfrow=c(2,1))

fluxes<-read.csv('FLUX_LUT.csv')
fluxes$color<-c('yellow green','yellow','purple4','blue','tan','forest green','gray','orange','orange4','red','light blue','gray','gray')

fluxes.chgtaxa<-fluxes[fluxes$Taxon%in%swa.names,]
#barplot(sort(fluxes$FLUX), names.arg=fluxes$Taxon[order(fluxes$FLUX)], col=fluxes$color[order(fluxes$FLUX)])
#plot(sort(fluxes$FLUX), names.arg=fluxes$Taxon[order(fluxes$FLUX)], col=fluxes$color[order(fluxes$FLUX)], pch=17)
plot(sort(fluxes.chgtaxa$FLUX), names.arg=fluxes$Taxon[order(fluxes.chgtaxa$FLUX)], col=fluxes$color[order(fluxes.chgtaxa$FLUX)], pch=17)



swa.chg<-sort(colSums(fia.swa, na.rm=TRUE)-colSums(pls.swa, na.rm=TRUE))
swa.names<-names(swa.chg[which(abs(swa.chg)>2.7e10)])

fluxes.chgtaxa<-fluxes[fluxes$Taxon%in%swa.names,]

col<-c('forest green','red','gray','blue','purple4','tan','gray','orange4','yellow green','yellow','orange','light blue')
barplot(swa.chg[which(abs(swa.chg)>2e10)], col=col)
abline(h=0, lwd=3)
swa.df<-data.frame(cbind(names(swa.chg), (unname(swa.chg))))
swa.df$X1<-as.character(swa.df$X1)
swa.df$X2<-as.numeric(as.character((swa.df$X2)))

all<-merge(fluxes.chgtaxa,swa.df, by.x="Taxon", by.y="X1")
all$Taxon<-as.character(all$Taxon)

barplot(height=all$X2[order(all$X2)], col=all$col[order(all$X2)],names.arg=all$Taxon[order(all$X2)], ylim=c(-2e11,2.5e11), ylab='Regional Sapwood Change')
abline(h=0, lwd=3)
box(lwd=3)
text(8,1.8e11, "Gained", font=2)
text(3,-1.4e11, "Lost", font=2)


plot(all$FLUX[order(all$X2)], pch=17,cex=3, ylim=c(0,7e-5), col=all$col[order(all$X2)], xaxt='n', ylab='Flux rate', xlab='')
box(lwd=3)
#####

#Now everything again for phenology

wcr.15<-read.csv('WCR_2015_SAPFLUX.csv')[,5:18];syv.15<-read.csv('SYV_2015_SAPFLUX.csv')[,5:24]
wcr.gap[is.na(wcr.gap)]<-wcr.15[is.na(wcr.gap)]
syv.gap[is.na(syv.gap)]<-syv.15[is.na(syv.gap)]

syv.gap<-0.000119*((syv.gap/10)^1.21)
wcr.gap<-0.000119*((wcr.gap/10)^1.21)

####Get monthly transp rates####
wcr.sap.day<-aggregate(wcr.gap, by=list(wcr.twr.2016$'DOY'), FUN='mean', na.rm=TRUE )
syv.sap.day<-aggregate(syv.gap, by=list(syv.twr.2016$'DOY'), FUN='mean', na.rm=TRUE )

monthvec<-c(rep(1, 31), rep(2, 29), rep(3, 31), 
            rep(4, 30), rep(5, 31), rep(6, 30),
            rep(7, 31), rep(8, 31), rep(9, 30),
            rep(10, 31), rep(11, 30), rep(12, 31))

wcr.monthagg<-cbind(wcr.sap.day[2:15], monthvec); wcr.month<-aggregate(wcr.monthagg, by=list(monthvec), FUN='mean', na.rm=TRUE)
syv.monthagg<-cbind(syv.sap.day[2:21], monthvec); syv.month<-aggregate(syv.monthagg, by=list(monthvec), FUN='mean', na.rm=TRUE)
#####

#Function from calc sapflow full
summ.flux<-function(source,tree,sapcol){ 
  sapsum<-data.frame(matrix(nrow=nrow(source), ncol=length(unique(tree$SPP))))
  for (i in 1:length(unique(tree$SPP))){
    soi<-as.character(unique(tree$SPP)[i])
    #sp<-unname(rowMeans(syv.master[,sapstart+which(syv.tree$SPP==soi)], na.rm=TRUE))
    sapsum[,i]<-unname(rowMeans(source[,sapcol+which(tree$SPP==soi)], na.rm=TRUE))
  }
  colnames(sapsum)<-as.character(unique(tree$SPP))
  sapsum$UK<-rowMeans(sapsum)
  
  sapsum<-na.approx(sapsum) #Fills missing april data at SYV
  
  return(sapsum)
}

syv.trmonth<-summ.flux(syv.month, syv.tree, 1)
wcr.trmonth<-summ.flux(wcr.month, wcr.tree, 1)

#Get scalars for other SPP

wcr.norm<-data.frame(apply(wcr.trmonth, 2, function(x) x/max(x, na.rm=TRUE)))
syv.norm<-data.frame(apply(syv.trmonth, 2, function(x) x/max(x, na.rm=TRUE)))

scalarvec<-rowMeans(cbind(wcr.norm$UK, syv.norm$UK), na.rm=TRUE)

####Build flux lut####
#Probably a better way to do this...
acsamean<-rowMeans(cbind(wcr.trmonth$ACSA, syv.trmonth$ACSA), na.rm=TRUE)
osvimean<-rowMeans(cbind(wcr.trmonth$OSVI, syv.trmonth$OSVI), na.rm=TRUE)
ukmean<-rowMeans(cbind(wcr.trmonth$UK, syv.trmonth$UK), na.rm=TRUE)

trmonth<-cbind(acsamean, osvimean, wcr.trmonth$TIAM, wcr.trmonth$FRPE, 
               syv.trmonth$TSCA, syv.trmonth$BEAL)
colnames(trmonth)<-c('ACSA', 'OSVI','TIAM','FRPE','TSCA','BEAL')

rm('acsamean','osvimean','ukmean')
#interpolate
trmonth<-data.frame(na.approx(trmonth))
#####

#Months
fia.month.transp<-matrix(data=NA,nrow=nrow(fia.transp.tot), ncol=12) #Months on columns, locations on rows
pls.month.transp<-matrix(data=NA,nrow=nrow(fia.transp.tot), ncol=12) #Months on columns, locations on rows
par(mfrow=c(1,2))
for(i in c(1:12)){
  
lut<-fluxrates.lut

lut$FLUX<-round(lut$FLUX*scalarvec[i], digits=7) #All growing season numbers, scaled
lut$FLUX[match(colnames(trmonth), fluxrates.lut$SPP)]<-round(as.numeric(trmonth[i,]), digits=7) #Add in measured numbers
lut$FLUX[12]<-mean(lut$FLUX[c(1:4,7:9,11)]);lut$FLUX[13]<-mean(lut$FLUX[c(5:6,10)]) #Average for hardwoods, softwoods

lut$FLUX[is.na(lut$FLUX)]<-round(fluxrates.lut$FLUX*scalarvec[i], digits=7)[is.na(lut$FLUX)]

fia.mo<-transp.calc(fia.swa/10000, fia.dens, lut) # The  /10000 converts sapwood area in cm2 to m2.
pls.mo<-transp.calc(pls.swa/10000, pls.dens, lut) # When m/s of sap flow is multiplied by m2 sapwood area, the output is m3/s. There's also a s>day conversion in original function so,
#Output is in m3/day for the whole grid cell.

fia.month.transp[,i]<-rowSums(fia.mo[,3:31], na.rm=TRUE)
pls.month.transp[,i]<-rowSums(pls.mo[,3:31], na.rm=TRUE)

plotguy.fia<-cbind(georef, fia.month.transp[,i])
plotguy.pls<-cbind(georef, pls.month.transp[,i])

plotnice(plotguy.pls, breakset=breakset.abs*8e-4, title='Historical')
plotnice(plotguy.fia, breakset=breakset.abs*8e-4,title='Modern')
#COOL
}
par(mfrow=c(1,1))
transpdiff<-(fia.month.transp-pls.month.transp) 

convfactor<-1000/(86400*64000000) #start: m3/day 8km ; 1000 L/m3, 86400 seconds/day ; 64000000 m2/8km grid cell
transpdiff.unit<-transpdiff*convfactor

write.csv(transpdiff.unit,"/Users/bethanyblakely/Desktop/Analysis/MIP/TranspChg_Sap.csv")

#Subsetting to transitions

egtaxa<-c(8,12,14,21,23)
hwtaxa<-c(1:7,9:10,13,15:17,19:20,22,24:26,28:29)

#comp based

fia.dat<-fia.comp[,4:32]
fia.eg<-rowSums(fia.dat[egtaxa]);fia.dc<-rowSums(fia.dat[hwtaxa])
fia.total<-rowSums(fia.dat[,c(1:17,18:26,28:29)])

#fia.egdom<-which((fia.eg/fia.total)>0.6)
fia.dcdom<-which((fia.dc/fia.total)>0.6)

pls.dat<-pls.comp[,4:32]
pls.eg<-rowSums(pls.dat[egtaxa]);pls.dc<-rowSums(pls.dat[hwtaxa])
pls.total<-rowSums(pls.dat[,c(1:17,18:26,28:29)])

#pls.egdom<-which((pls.eg/pls.total)>0.6)
#pls.dcdom<-which((pls.dc/pls.total)>0.6)

transitions<-which(((pls.eg/pls.total)>=0.6 & (fia.dc/fia.total)>=0.6) | #EG to DC
                     ((pls.eg/pls.total>=0.4 & (pls.eg/pls.total<0.6)) & (fia.dc/fia.total)>=0.6) | #MX to DC
                      pls.eg/pls.total>=0.6 & ((fia.dc/fia.total>=0.4 & fia.dc/fia.total<0.6))) #EG to MX

evelse<-which(((pls.eg/pls.total)<0.4 | (fia.dc/fia.total)<0.4) | #doesn't start or end mixed / dominant
                ((pls.eg/pls.total)>=0.4 & (fia.dc/fia.total)<0.6)) #starts mixed but doesnt go deciduous
              
length(transitions)

write.csv(transitions,"/Users/bethanyblakely/Desktop/Analysis/MIP/EGtoDC_ind.csv")


transp.dcify<-transpdiff.unit[transitions,]

transp.dcify.plot<-transpdiff.unit; transp.dcify.plot[evelse,]<-NA


plot(colMeans(transp.dcify), type='l', lwd=3, ylim=c(-1e-5,2e-5))
for(i in 1:nrow(transp.dcify)){
  lines(transp.dcify[i,], col='gray', lwd=0.5)
  lines(colMeans(transp.dcify), lwd=2)
}

#A map
par(mfrow=c(1,1), mar=c(1,1,1,1))


background<-rasterFromXYZ(cbind(pls.comp.raw[1:2], rowSums(pls.comp.raw[4:32])))
plot(background, col='black', legend=FALSE, main='Transpiration Change',box=FALSE, axes=FALSE)

default<-rasterFromXYZ(pls.transp.tot)
rasterFromXYZ(cbind(pls.comp.raw[1:2], rowSums(pls.comp.raw[,4:32])))
#plot(default, add=TRUE, col='black')

breaknum=11
col<-colorRampPalette(c('dark red','red','light steel blue', 'blue','dark blue'))
breaks<-seq(from=-0.5, to=0.5, length.out=breaknum)

transprast<-rasterFromXYZ(cbind(georef,rowMeans(transp.dcify.plot*3600, na.rm=TRUE)))
plot(transprast, add=TRUE, breaks=breaks, col=col(breaknum))

dev.copy(png, filename='/Users/bethanyblakely/Desktop/Analysis/Albedo/Figures/Maps/TranspChangeMap.png', width=500, height=410); dev.off()


#mask<-default
#mask[transitions]<-NA
#plot(mask, add=T, col='black')

plot(colMeans(transpdiff.unit[transitions,]), type='l')
