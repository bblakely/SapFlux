#Read in pls products
comp.raw<-read.csv('plss_composition_alb_v0.9-10.csv')
dens.raw<-read.csv('plss_density_alb_v0.9-10.csv')
diam.raw<-read.csv('plss_diam_alb_v0.9-10.csv')

#dens from stems/hectare to stems/cell
#assumes 8km / side cells
dens.dat<-dens.raw[,4:32]
dens.stem<-dens.dat*6400 #cells are 64km2. 100ha per km2 * 64. 
dens.stem<-cbind(dens.raw[,1:3], dens.stem)

# #Let's just assume everything is a maple, and all trees have the same avg diameter.
# swa.eachtree<-0.312*diam.raw[4:32]^2.07  #allometry for sugar maple, 0.312*DBH^2.07
# swa.tot<-swa.eachtree*dens.stem[4:32]*0.85 #0.85 is a rough avg of the radial corrections observed
# maple_fluxrate<-((7/12)*8.64e-6)+((5/12)*1.18e-5)  #flux in m/s
# 
# flow.tot<-swa.tot*maple_fluxrate*86400
# flow.cell<-cbind(dens.stem[,1:2], rowSums(flow.tot))
# 
# flow.cell.base<-flow.cell

#Okay now let's assign genus specific flux rates so far as I measured
diam.dat<-diam.raw[4:32]/comp.raw[4:32] #Check this! Makes reasonble values.
swa.eachtree<-data.frame(matrix(dat=NA,nrow(diam.dat),ncol(diam.dat)))
colnames(swa.eachtree)<-colnames(diam.dat)

#Fill in all not-measured species (measured species will replace in following lines)

swa.eachtree<-0.312*diam.dat^2.07 #This is maple for now

#Ash 
RP.sub<-2
swa.eachtree[,RP.sub]<-0.21*diam.dat[,RP.sub]^2.18 #Not sure where this comes from...

#Basswood
swa.eachtree[,3]<-0.77*diam.dat[,22]^1.868 #Actually aspen

#Beech
swa.eachtree[,4]<-0.67*diam.dat[,4]^1.92 #American Beech

#Birch
swa.eachtree[,5]<-1.17*diam.dat[,5]^1.79 #Yellow Birch

#Cedar/Juniper
swa.eachtree[,8]<-(3.14*(0.5*diam.dat[,8])^2)-(3.14*(0.5*(diam.dat[,8]-1.5))^2) #White Cedar

#ALL EVERGREENS ARE HEMLOCK, RIGHT? (Fir, Hemlock, Spruce, Tamarack)
EG.sub<-c(12,14,23,25)
swa.eachtree[,EG.sub]<-4.2568*diam.dat[,EG.sub]^1.37 #Eastern Hemlock

#Maple
SG.sub<-c(16,17)
swa.eachtree[,SG.sub]<-0.312*diam.dat[,SG.sub]^2.07  #Sugar/Red maple combined

#Oak
swa.eachtree[,19]<-3.24*diam.dat[,19]-10.24 #Red oak

#Pine
swa.eachtree[,21]<-10^(-0.221+2.145*log10(diam.dat[,21])-(0.004*35)) #Jack Pine
  
#Poplar
swa.eachtree[,22]<-0.77*diam.dat[,22]^1.868  #Bigtooth Aspen


#Multiply out to all stems in grid cell
swa.tot<-swa.eachtree*dens.stem[4:32]*0.75 #I think the .75 is for radial profile

#Make LUT to make this faster
# fluxrates.lut.old<-data.frame(matrix(NA,7,3))
# colnames(fluxrates.lut.old)<-c('SPP', 'FLUX', 'COL')
# #old
# fluxrates.lut.old$SPP<-c('ACSA','OSVI','BEAL','TSCA','TIAM','FRPE','OTH') 
# fluxrates.lut.old$FLUX<-c(1.05e-5,9.87e-6,9.81e-6,5.7e-6,1.19e-5,1.39e-6,9.81e-6)
# fluxrates.lut.old$COL<-c(17,16,5,14,3,2,NA)

fluxrates.lut<-read.csv('FLUX_LUT.csv')

fluxrates.blakely<-c(1,2,4,6:8,10:13)
fluxrates.use<-fluxrates.lut[fluxrates.blakely,]
fluxrates.lut<-fluxrates.use


flow.tot<-data.frame(matrix(NA, nrow(diam.dat),ncol(diam.dat)))
colnames(flow.tot)<-colnames(diam.dat)

#All maple
flow.tot.basic<-swa.tot*1.05e-05*86400
flow.basic<-cbind(dens.stem[,1:2], rowSums(flow.tot.basic, na.rm=TRUE))
flow.basic[rowSums(dens.dat)<10,3]<-NA

flow.tot[,]<-swa.tot*fluxrates.lut$FLUX[fluxrates.lut$Taxon=='Hardwood']*86400  #Baseline: all things mean of hardwoods
flow.tot[,EG.sub]<-swa.tot[,EG.sub]*fluxrates.lut$FLUX[fluxrates.lut$Taxon=='Evergreen']*86400 #Step 2, evergreens mean of evergreens


# leave.out<-c(4,8)
# fluxrates.lut.test<-fluxrates.lut[fluxrates.lut$COL!=leave.out,]
# fluxrates.lut<-fluxrates.lut.test

for(i in 1:(nrow(fluxrates.lut)-2)){
  flow.tot[,fluxrates.lut$COL[i]]<-swa.tot[,fluxrates.lut$COL[i]]*fluxrates.lut$FLUX[i]*86400
}


flow.cell<-cbind(dens.stem[,1:2], rowSums(flow.tot, na.rm=TRUE))
flow.cell[rowSums(dens.dat)<10,3]<-NA


library(maps)
library(raster)
library(RColorBrewer)

toplot<-flow.cell

quants<-quantile(toplot[,3],c(0.03, 0.97), na.rm=TRUE)

flow.cell.vis<-toplot
bignums<-which(flow.cell.vis[,3]>quants[2])
flow.cell.vis[bignums,3]<-quants[2] #Clip hemlock mountain to max


rast<-rasterFromXYZ(flow.cell)
rast.vis<-rasterFromXYZ(flow.cell.vis)


breaks<-seq(from=quants[1], to=quants[2],length.out=8)
pal<-brewer.pal(6,'RdYlGn')


#Percent actually based on data

goodcols<-sort(c(fluxrates.lut$COL[1:(nrow(fluxrates.lut)-2)],18,20,27))
#(2,3,14,5,16,17,21,22,19,18,20,27)


comp.dat<-comp.raw[4:32]

alltree<-rowSums(dens.stem[4:32])
goodtree<-rowSums(dens.stem[goodcols+3])

#alltree<-rowSums(comp.dat)
#goodtree<-rowSums(comp.dat[goodcols])

knowtree<-goodtree/alltree

knowtree.cell<-cbind(dens.stem[1:2], knowtree)

knowtree.rast<-rasterFromXYZ(knowtree.cell)
pal.know<-brewer.pal(5, 'Greys')


par(mfrow=c(1,2))
plot(knowtree.rast, main='% known', col=pal.know)
plot(rast.vis, breaks=c(0,2e8,4e8,6e8,8e8,1e9,1.2e9), col=pal, main='sapflux')

 for(i in 1:29){
    chg<-which(abs(pctcomp.diff[,i])!=0 )
    fit<-lm(diff.transp[chg,3]~pctcomp.diff[chg,i])
    if(length(chg)<5){
      sig<-999
      r2<-999
    }else{
      sig<-summary(fit)$coefficients[2,4]
      r2<-summary(fit)$r.squared}
  if(sig<0.01& r2>0.01){ #& r2>0.02
    print(paste(colnames(pctcomp.diff)[i], "is significant with r2", r2))
    plot(diff.transp[chg,3]~pctcomp.diff[chg,i], main=colnames(pctcomp.diff)[i])
    int<-unname(fit[[1]][1])
    sl<-unname(fit[[1]][2])
    abline(int,sl,col='red')
    }
  }

#classify trees as early/mid/late, riparian, understory

early<-c(3,5,9,12,22) #5 is birch, could be both early and late
mid<-c(2,15,17,20,23,26,28)
late<-c(4,8,14) #8 is cedar, could be riparian
riparian<-c(1,11,24,25,29)
small<-c(6,7,10,13,16)
dry<-c(19,21)

ear.tree<-rowSums(pctcomp.diff[early])
mid.tree<-rowSums(pctcomp.diff[mid])
late.tree<-rowSums(pctcomp.diff[late])
riparian.tree<-rowSums(pctcomp.diff[riparian])
small.tree<-rowSums(pctcomp.diff[small])
dry.tree<-rowSums(pctcomp.diff[dry])


succ<-cbind(ear.tree,mid.tree,late.tree,riparian.tree,small.tree, dry.tree)
colnames(succ)<-c('early','mid','late','riparian','small','dry' )

for(i in 1:6){
  chg<-which(abs(succ[,i])!=0 )
  fit<-lm(diff.transp[chg,3]~succ[chg,i])
  plot(diff.transp[chg,3]~succ[chg,i], main=colnames(succ)[i], xlim=c(-1,1))
  if(length(chg)<5){
    sig<-999
    r2<-999
  }else{
    sig<-summary(fit)$coefficients[2,4]
    r2<-summary(fit)$r.squared}
  if(sig<0.01&r2>0.01){ #& r2>0.02
    print(paste(colnames(succ)[i], "is significant with r2", r2))
    int<-unname(fit[[1]][1])
    sl<-unname(fit[[1]][2])
    abline(int,sl,col='red')
  }
}


swa.diff<-rowSums(fia.swa, na.rm=TRUE)-rowSums(pls.swa, na.rm=TRUE)
plot(density(swa.diff))
abline(v=0)

mean(swa.diff)/(8000^2)

