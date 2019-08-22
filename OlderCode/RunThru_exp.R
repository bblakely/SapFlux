#Questions in think-through
source("Refine_TowerData.R")


#Do WCR maples have more sapwood? More sapwood rel. to basal?

mean(syv.tree$SWA_meas);mean(wcr.tree$SWA_meas) #straight mean
mean(syv.tree$SWA_meas*syv.tree$MULT_meas);mean(wcr.tree$SWA_meas*wcr.tree$MULT_meas) #mean with multiplier; 'effective' SWA
mean((syv.tree$SWA_meas*syv.tree$MULT_meas)/syv.tree$BA);mean(wcr.tree$SWA_meas*wcr.tree$MULT_meas/wcr.tree$BA) #'effective' per BA, probably most meaningful
mean((syv.tree$SWA_meas)/syv.tree$BA);mean(wcr.tree$SWA_meas/wcr.tree$BA) # total sapwood area per BA

#percent maples covered at each - need more @ syv
length(which(syv.tree$meas[syv.tree$SPP=="ACSA"]=='Y')) / length(which(syv.tree$SPP=="ACSA"))
length(which(wcr.tree$meas[wcr.tree$SPP=="ACSA"]=='Y')) / length(which(wcr.tree$SPP=="ACSA"))

#same calculations as above, but just for maples
syv.tree.maple<-syv.tree[syv.tree$SPP=='ACSA',]; wcr.tree.maple<-wcr.tree[wcr.tree$SPP=='ACSA',]
mean(syv.tree.maple$SWA_meas);mean(wcr.tree.maple$SWA_meas) #straight mean
mean(syv.tree.maple$SWA_meas*syv.tree.maple$MULT_meas);mean(wcr.tree.maple$SWA_meas*wcr.tree.maple$MULT_meas) #mean with multiplier; 'effective' SWA
mean((syv.tree.maple$SWA_meas*syv.tree.maple$MULT_meas)/syv.tree.maple$BA);mean(wcr.tree.maple$SWA_meas*wcr.tree.maple$MULT_meas/wcr.tree.maple$BA) #'effective' per BA, probably most meaningful
mean((syv.tree.maple$SWA_meas)/syv.tree.maple$BA);mean(wcr.tree.maple$SWA_meas/wcr.tree.maple$BA) # total sapwood area per BA

#mean multipliers, e.g. which uses its SWA best:
mean(syv.tree$MULT_meas); mean(wcr.tree$MULT_meas) # ~20% more efficient SW use at WCR

###Okay sapwood explore done

#At the forest level, NOT including age, growth effects
#As of 7/5 these have incorrect hemlock multipliers; however, hemlocks are <10% of the forest so shouldn't be a big deal. Correct eventually though
mean(syv.forest$SWA);mean(wcr.forest$SWA) #mean 'effective' SWA
mean(syv.forest$SWA/syv.forest$ba);mean(wcr.forest$SWA/wcr.forest$ba)

#interesting, much closer. What if we use measured trees but with allometric swa
mean(syv.tree$SWA_calc*syv.tree$MULT_calc);mean(wcr.tree$SWA_calc*wcr.tree$MULT_calc) #mean with multiplier; 'effective' SWA
mean((syv.tree$SWA_calc*syv.tree$MULT_calc)/syv.tree$BA);mean(wcr.tree$SWA_calc*wcr.tree$MULT_calc/wcr.tree$BA) #'effective' per BA, probably most meaningful

#Okay so farther apart when using measured values, mainly becasue of WCR changing

#(plotted flux and flow in calc sapflux)
#Plot again as 'effective' flux?
par(mfrow=c(1,2))
#Sylvania
plot(aggregate(syv.gap[,1], by=list(syv.master$H), FUN='mean', na.rm=TRUE), col='white', ylim=c(0,40))
for(i in 1:ncol(syv.gap)){
  lines(aggregate(syv.gap[,i]*syv.tree$MULT_meas[i], by=list(syv.master$H), FUN='mean', na.rm=TRUE), col=syv.tree$col[i])
}

#WCR
plot(aggregate(wcr.gap[,1], by=list(wcr.master$H), FUN='mean', na.rm=TRUE), col='white', ylim=c(0,40))
for(i in 1:ncol(wcr.gap)){
  lines(aggregate(wcr.gap[,i]*wcr.tree$MULT_meas[i], by=list(wcr.master$H), FUN='mean', na.rm=TRUE), col=wcr.tree$col[i])
}



#Okay, is it greater atmospheric demand?
plot(syv.twr$VPD_PI_1, type='l');plot(wcr.twr$VPD_PI_1,type='l') #looks similar

plot(syv.twr$VPD_PI_1-wcr.twr$VPD_PI_1, type='l')
hist(syv.twr$VPD_PI_1-wcr.twr$VPD_PI_1)

gs<-c(152:243) #Jun to Aug, peakish season
hist(syv.twr$VPD_PI_1[syv.twr$DOY%in%gs]-wcr.twr$VPD_PI_1[wcr.twr$DOY%in%gs], xlim=c(-20,20))
mean(syv.twr$VPD_PI_1[syv.twr$DOY%in%gs]-wcr.twr$VPD_PI_1[wcr.twr$DOY%in%gs], na.rm=TRUE)

#Nope, VPD is *higher* at SYV actually, even more so in GS

#Leaf temperature is higher at WCR, maybe that matters?
wcr.temps<-rowMeans(wcr.ap[,7:10]);syv.temps<-rowMeans(syv.ap[,7:10])

hist(syv.temps-wcr.temps);hist(syv.temps[syv.twr$DOY%in%gs]-wcr.temps[wcr.twr$DOY%in%gs])
mean(syv.temps-wcr.temps, na.rm=TRUE);mean(syv.temps[syv.twr$DOY%in%gs]-wcr.temps[wcr.twr$DOY%in%gs], na.rm=TRUE)

#half a degree difference year-round, quarter of a degree difference in GS

#plot flux diffs against met, codominant maples only for simplicity and clarity

plot(rowMeans(wcr.gap[,wcr.tree$SPP=='ACSA'&wcr.tree$CC=='C'], na.rm=TRUE)-rowMeans(syv.gap[,syv.tree$SPP=='ACSA'&syv.tree$CC=='C'], na.rm=TRUE), type='l')

maplediff<-(rowMeans(wcr.gap[,wcr.tree$SPP=='ACSA'&wcr.tree$CC=='C'])-rowMeans(syv.gap[,syv.tree$SPP=='ACSA'&syv.tree$CC=='C']))
tempdiff<-syv.temps-wcr.temps
vpddiff<-syv.twr$VPD_PI_1-wcr.twr$VPD_PI_1

plot(maplediff~tempdiff);plot(maplediff[syv.twr$DOY%in%gs]~tempdiff[syv.twr$DOY%in%gs])
smoothScatter(maplediff~tempdiff);smoothScatter(maplediff[syv.twr$DOY%in%gs]~tempdiff[syv.twr$DOY%in%gs])
#okay it really doesn't look like a temp thing...

plot(maplediff~vpddiff);plot(maplediff[syv.twr$DOY%in%gs]~vpddiff[syv.twr$DOY%in%gs])
smoothScatter(maplediff[syv.twr$HOUR %in% c(10:15)]~vpddiff[syv.twr$HOUR %in% c(10:15)]);smoothScatter(maplediff[syv.twr$DOY%in%gs & syv.twr$HOUR %in% c(10:15)]~vpddiff[syv.twr$DOY%in%gs & syv.twr$HOUR %in% c(10:15)])

lm((maplediff[syv.twr$DOY%in%gs & syv.twr$HOUR %in% c(10:15)]~vpddiff[syv.twr$DOY%in%gs & syv.twr$HOUR %in% c(10:15)]))$coefficients

#interestingly, the difference in maple transp (maybe) *increases* as syv vpd gets further above wcr vpd. Perhaps a covariate?
#either way, more proof that VPD is not driving differences, unless it's water-stress related

#Make a daytime gs vector
daygs<-which(syv.twr$DOY%in%gs & syv.twr$HOUR %in% c(10:15))
#Let's check on water stress
par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
for (i in c(1:14)){
smoothScatter(wcr.gap[daygs,i]~wcr.twr$VPD_PI_1[daygs], xlab='vpd',ylab='flux', main=colnames(wcr.gap)[i], ylim=c(0,100))
}

#codom maples only, wcr
for (i in which(wcr.tree$SPP=='ACSA'& wcr.tree$CC=="C")){
  smoothScatter(wcr.gap[daygs,i]~wcr.twr$VPD_PI_1[daygs], xlab='vpd',ylab='flux', main=paste('wcr',colnames(wcr.gap)[i]), ylim=c(0,100))
}

#codom maples only, syv
for (i in which(syv.tree$SPP=='ACSA'& syv.tree$CC=="C")){
  smoothScatter(syv.gap[daygs,i]~syv.twr$VPD_PI_1[daygs], xlab='vpd',ylab='flux', main=paste('syv',colnames(syv.gap)[i]), ylim=c(0,100))
}

#Check soil moist diffs
mean(syv.twr$SWC_1[daygs], na.rm=TRUE);mean(wcr.twr$SWC_1[daygs], na.rm=TRUE)
mean(syv.twr$SWC_1_2_1[daygs], na.rm=TRUE);mean(wcr.twr$SWC_1_2_1[daygs], na.rm=TRUE)

#Alright, syv is drier. So higher vpd and lower soil moisture. 
#More latent heat flux though - WCR trapping moisture better? T:ET is higher as well.

#This may explain lower Air T @ sylvania (more evap, consistent with greater LE) and greater Canopy T - Air T at WCR (less cooling to air, more cooling to surface)

#maybe it just rains more? **Need separate data for this as tower precip does not work


#Okay let's try for hysteresis. this will be complex. Make this a function when no in a hurrt
dry<-8 #vpd cutoff for dryish days
syv.maple<-syv.gap[syv.tree$SPP=='ACSA' & syv.tree$CC =='C']
syv.dryind<-which(syv.twr$VPD_PI_1>dry & syv.twr$DOY%in%gs)

wcr.maple<-wcr.gap[wcr.tree$SPP=='ACSA' & wcr.tree$CC =='C']
wcr.dryind<-which(wcr.twr$VPD_PI_1>dry & wcr.twr$DOY%in%gs)

#smoothScatter(rowMeans(syv.maple)[syv.dryind]~syv.ts$HOUR[syv.dryind])
#smoothScatter(rowMeans(wcr.maple)[wcr.dryind]~wcr.ts$HOUR[wcr.dryind])

syv.dryhour<-aggregate(rowMeans(syv.maple)[syv.dryind], by=list(syv.twr$HOUR[syv.dryind]), FUN='mean', na.rm=TRUE)
wcr.dryhour<-aggregate(rowMeans(wcr.maple)[wcr.dryind], by=list(wcr.twr$HOUR[wcr.dryind]), FUN='mean', na.rm=TRUE)

syv.dryvp<-aggregate(syv.twr$VPD_PI_1[syv.dryind], by=list(syv.twr$HOUR[syv.dryind]), FUN='mean', na.rm=TRUE)
wcr.dryvp<-aggregate(wcr.twr$VPD_PI_1[wcr.dryind], by=list(wcr.twr$HOUR[wcr.dryind]), FUN='mean', na.rm=TRUE)

colcode<-rep('black', 24)
colcode[7:13]<-"orange"
colcode[13:22]<-"blue"

par(mfrow=c(2,2))
par(mar=c(4,4,4,2))
daytime<-c(7:22)

plot(syv.dryhour$x~syv.dryvp$x, col=colcode[syv.dryhour$Group.1], ylim=c(0,75), xlim=c(8,16), xlab="VPD", ylab="Flux rate", main='SYV');lines(syv.dryhour$x~syv.dryvp$x);
plot(wcr.dryhour$x~wcr.dryvp$x, col=colcode[wcr.dryhour$Group.1], ylim=c(0,75), xlim=c(8,16), xlab="VPD", ylab="Flux rate", main='WCR');lines(wcr.dryhour$x~wcr.dryvp$x)

#normalized...
plot((syv.dryhour$x/max(syv.dryhour$x))~syv.dryvp$x, col=colcode[syv.dryhour$Group.1], ylim=c(0,1), xlim=c(5,16), xlab="VPD", ylab="Flux rate", main='SYV');lines(syv.dryhour$x/max(syv.dryhour$x)~syv.dryvp$x)
plot(wcr.dryhour$x/max(wcr.dryhour$x)~wcr.dryvp$x, col=colcode[wcr.dryhour$Group.1], ylim=c(0,1), xlim=c(5,16), xlab="VPD", ylab="Flux rate", main='WCR');lines(wcr.dryhour$x/max(wcr.dryhour$x)~wcr.dryvp$x)

#Both normalized (this does not work for no reason?)
syv.hrn<-(syv.dryhour$x/max(syv.dryhour$x))
syv.vpdn<-(syv.dryvp$x/max(syv.dryvp$x))
plot(syv.hrn~syv.vpdn, col=colcode[syv.dryhour$Group.1], ylim=c(0,1), xlim=c(0.5, 1), xlab="VPD", ylab="Flux rate", main='SYV');lines(syv.hrn~syv.vpdn)

wcr.hrn<-(wcr.dryhour$x/max(wcr.dryhour$x))
wcr.vpdn<-(wcr.dryvp$x/max(wcr.dryvp$x))
plot(wcr.hrn~wcr.vpdn, col=colcode[wcr.dryhour$Group.1], ylim=c(0,1), xlim=c(0.5, 1), xlab="VPD", ylab="Flux rate", main='WCR');lines(wcr.hrn~wcr.vpdn)


#Okay cool so strong case for some effect of water stress - larger differences at high VPD, greater hysteresis at SYV

#Let's actually get area...

#Only sap normalized
library(pracma)
polyarea(wcr.dryhour$x/max(wcr.dryhour$x),wcr.dryvp$x);polyarea((syv.dryhour$x/max(syv.dryhour$x))[syv.dryhour$Group.1%in%c(7:22)],syv.dryvp$x[syv.dryhour$Group.1%in%c(7:22)]) #eliminate nighttime values from syv polygon
polyarea(wcr.dryhour$x/max(wcr.dryhour$x),wcr.dryvp$x)/polyarea((syv.dryhour$x/max(syv.dryhour$x))[syv.dryhour$Group.1%in%c(7:22)],syv.dryvp$x[syv.dryhour$Group.1%in%c(7:22)]) #eliminate nighttime values from syv polygon

#vpd also normalized
polyarea(wcr.hrn,wcr.vpdn);polyarea(syv.hrn[daytime], syv.vpdn[daytime])
polyarea(wcr.hrn,wcr.vpdn)/polyarea(syv.hrn[daytime], syv.vpdn[daytime])

#similar story with both normalized, somewhat less difference



#How does humidity differ? Apparently we don't have this data. 
#But WCR VPD < SYV VPD despite WCR Temp > SYV Temp would indicate WCR humid > SYV humid

#recheck netrad/swin
mean(syv.twr$NETRAD_1[daygs], na.rm=TRUE)
mean(wcr.twr$NETRAD_1[daygs], na.rm=TRUE)

mean(syv.twr$SW_IN[daygs], na.rm=TRUE)
mean(wcr.twr$SW_IN[daygs], na.rm=TRUE)

t.test(syv.twr$SW_IN[daygs],wcr.twr$SW_IN[daygs]) #NOT significant
t.test(syv.twr$NETRAD_1[daygs],wcr.twr$NETRAD_1[daygs]) #Definitely significant
t.test(syv.twr$LW_IN[daygs],wcr.twr$LW_IN[daygs]) #Very Significant, but effect is small, <15 W/m^2
t.test(syv.twr$LW_IN[daygs]+syv.twr$SW_IN[daygs],wcr.twr$LW_IN[daygs]+wcr.twr$SW_IN[daygs]) #Barely Significant, 3 to 44 W/m^2

#All of this is consistent with slightly greater cloudiness at WCR

#With wcr zeros included
syv.twr.zeroes<-syv.twr
syv.twr.zeroes$SW_IN[syv.twr.zeroes$SW_IN<0]<-0

wcr.twr.zeroes<-wcr.twr
wcr.twr.zeroes$SW_IN[is.na(wcr.twr.zeroes$SW_IN) & (wcr.twr.zeroes$HOUR<10 | wcr.twr.zeroes$HOUR>17)]<-0

t.test(syv.twr.zeroes$SW_IN[daygs],wcr.twr.zeroes$SW_IN[daygs]) #NOT significant
t.test(syv.twr.zeroes$NETRAD_1[daygs],wcr.twr.zeroes$NETRAD_1[daygs]) #Definitely significant
t.test(syv.twr.zeroes$LW_IN[daygs],wcr.twr.zeroes$LW_IN[daygs]) #Very Significant, but effect is small, <15 W/m^2
t.test(syv.twr.zeroes$LW_IN[daygs]+syv.twr.zeroes$SW_IN[daygs],wcr.twr.zeroes$LW_IN[daygs]+wcr.twr.zeroes$SW_IN[daygs]) #somewhat Significant, 5 to 46 W/m^2

#Basically same story


