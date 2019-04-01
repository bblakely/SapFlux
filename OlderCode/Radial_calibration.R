#Calculates radial profiles from calibration experiment 
#in which a long needle measured sap flux for ~3 days at each of 4 depths.

#Data from calibration 
syv.c.raw<-read.csv('SYV_Calib.csv')
und.c.raw<-read.csv('UND_Calib.csv')
wcr.c.raw<-read.csv('WCR_Calib.csv')

#WCR has an extra day with messy data. Remove
wcr.c.raw<-wcr.c.raw[wcr.c.raw$DOY > 208,]


sc<-8 #First column with main data

#Cleanup
syv.c.dat<-syv.c.raw[,sc:ncol(syv.c.raw)]
syv.c.dat[syv.c.dat<0.001 | syv.c.dat> 1]<-NaN
und.c.dat<-und.c.raw[,sc:ncol(und.c.raw)]
und.c.dat[und.c.dat<0.001 | und.c.dat> 1]<-NaN
wcr.c.dat<-wcr.c.raw[,sc:ncol(wcr.c.raw)]
wcr.c.dat[wcr.c.dat<0.001 | wcr.c.dat> 1]<-NaN

#Days when sensor was moved between depths (from field journal)
und.switchdays<-c(211,215,220,223,226)
wcr.switchdays<-c(209,213,217,221,224)
syv.switchdays<-c(210,214,218,222,226)


#take out days where I switched the sensor
syv.c.dat[syv.c.raw$DOY %in% syv.switchdays,]<-NaN
#& syv.c.raw$H > 6 &syv.c.raw$H< 22,] <- NaN
und.c.dat[und.c.raw$DOY %in% und.switchdays,]<-NaN
#& und.c.raw$H > 6 &und.c.raw$H< 22,] <- NaN
wcr.c.dat[wcr.c.raw$DOY %in% wcr.switchdays,]<-NaN
#& wcr.c.raw$H > 6 &wcr.c.raw$H< 22,] <- NaN

#First look plots
for(i in 1:4){
  plot( syv.c.raw$DecDOY,syv.c.dat[,i], main=paste('syv', i), type='l')
}
for(i in 1:5){
  plot(und.c.raw$DecDOY, und.c.dat[,i],  main=paste('und', i), type='l')
}
for(i in 1:2){
  plot(wcr.c.raw$DecDOY, wcr.c.dat[,i], main=paste('wcr', i), type='l')
}


sites<-list(wcr.c.dat,syv.c.dat,und.c.dat)
meta.raw<-list(wcr.c.raw,syv.c.raw,und.c.raw)
names<-c('wcr','syv','und')

#Loop for all 3 sites. Probably would have been more efficient as a function but it works
for (s in 1:3){
  DAT<-sites[[s]]      #Grab data for a site
  meta<-meta.raw[[s]]  #Grab metadata for same site
  
  DAT.t<-cbind(meta$DOY, DAT) #Timeseries + raw data. Same for all sites
  colnames(DAT.t)<-c('DOY', colnames(DAT))
  
  #Set reusable params. Seriously this should have been a function.
  nObs<-nrow(DAT)
  DOYs<-unique(meta$DOY)
  nDays<-length(DOYs)
  nSensors<-ncol(DAT)
  Sensornames<-colnames(DAT)
  
  #Convert to flow. Throws errors because of days with all NaNs. Don't worry about it.
  maxes<-aggregate(DAT.t, by=list(DAT.t$DOY), FUN=max, na.rm=TRUE)
  maxes<-maxes[,3:ncol(maxes)]
  maxes[maxes==-Inf]<-NaN
  
  #Make repeating timeseries of maxes for sapflux calculation.
  Daymaxes<-as.data.frame(matrix(data=NaN,nrow=nObs, ncol=nSensors))
  Daymax.names<-rep(NaN, nSensors)
  for (v in 1:nSensors){
    print(v)
    Daymaxvec<-rep(NaN, nObs)
    for (i in 1:nDays){
      Daymaxvec[DAT.t$DOY==DOYs[i]]<-maxes[i,v]    
    }
    Daymaxes[,v]<-Daymaxvec 
    Daymax.names[v]<- paste(Sensornames[v],"max", sep='_')
  }
  colnames(Daymaxes)<-Daymax.names
  rm(Daymax.names, Daymaxvec)
  
# To flow
K<-as.data.frame(matrix(data=NaN,nrow=nObs, ncol=nSensors))
K.names<-rep(NaN, nSensors)
Flux<-as.data.frame(matrix(data=NaN,nrow=nObs, ncol=nSensors))
Flux.names<-rep(NaN, nSensors)
for(v in 1:nSensors){
  K[,v]<-((Daymaxes[,v]-DAT[,v])/DAT[,v])
  Flux[,v]<-0.000119*(K[,v])^1.231
  
  K.names[v]<-paste(Sensornames[v], "K",sep='_')
  Flux.names[v]<-paste(Sensornames[v], "Flux", sep='')                   
}

colnames(K)<-K.names
colnames(Flux)<-Flux.names

rm(K.names, K)

#Rename to save results
if(s==1){wcr.flux<-Flux} 
if(s==2){syv.flux<-Flux}
if(s==3){und.flux<-Flux}
  
}

#Plot flow. Nice decreasing pattern on most. Some SYV sensors break.
for(i in 1:4){
  plot( syv.c.raw$DecDOY,syv.flux[,i], main=paste('syv flux', i), type='l')
}
for(i in 1:5){
  plot(und.c.raw$DecDOY, und.flux[,i],  main=paste('und flux', i), type='l')
}
for(i in 1:2){
  plot(wcr.c.raw$DecDOY, wcr.flux[,i], main=paste('wcr flux', i), type='l')
}

rm('Flux','meta','Daymaxes')

#Ratios

#Prep for aggregation. Another thing that should be a function. 
#Geez 2016 me, get yourself together!
und.dectime<-(und.c.raw$H)+((und.c.raw$M)/60)
und.flux.t<-cbind(und.c.raw$DOY, und.dectime, und.flux)
colnames(und.flux.t)<-c('DOY','DecTime', colnames(und.flux))

syv.dectime<-(syv.c.raw$H)+((syv.c.raw$M)/60)
syv.flux.t<-cbind(syv.c.raw$DOY, syv.dectime, syv.flux)
colnames(syv.flux.t)<-c('DOY','DecTime', colnames(syv.flux))

wcr.c.raw$M<-as.numeric(levels(wcr.c.raw$M))[wcr.c.raw$M]
wcr.dectime<-(wcr.c.raw$H)+((wcr.c.raw$M)/60)
wcr.flux.t<-cbind(wcr.c.raw$DOY, wcr.dectime, wcr.flux)
colnames(wcr.flux.t)<-c('DOY','DecTime', colnames(wcr.flux))

#It rained on DOY 217. Remove.
#wcr.c.raw[wcr.c.raw$DOY == 217,]  #Change day for WCR so already NaN'd
syv.flux.t[syv.flux.t$DOY == 217 & syv.flux.t$DecTime >10 & syv.flux.t$DecTime<16,3:6]<-NaN
und.flux.t[und.flux.t$DOY == 217 & und.flux.t$DecTime >10 & und.flux.t$DecTime<16,3:7]<-NaN

#Position 1 (Normal 10mm depth, middle of 0 - 20 mm ring). Days from field journal.
und.d1<-und.flux.t[und.flux.t$DOY %in% c(212:214),]  #Pull days of interest
und.p1<-aggregate(und.d1, by=list(und.d1$DecTime), FUN=mean, na.rm=TRUE) #average 5-min rates across the ~3 days to make an avg daily profile

syv.d1<-syv.flux.t[syv.flux.t$DOY %in% c(211:213),]
syv.p1<-aggregate(syv.d1, by=list(syv.d1$DecTime), FUN=mean, na.rm=TRUE)

wcr.d1<-wcr.flux.t[wcr.flux.t$DOY %in% c(210:212),]
wcr.p1<-aggregate(wcr.d1, by=list(wcr.d1$DecTime), FUN=mean, na.rm=TRUE)

rm('und.d1','syv.d1','wcr.d1')

#Position 2 (30 mm depth; middle of 20 - 40mm ring)
und.d2<-und.flux.t[und.flux.t$DOY %in% c(216:219),]
und.p2<-aggregate(und.d2, by=list(und.d2$DecTime), FUN=mean, na.rm=TRUE)

syv.d2<-syv.flux.t[syv.flux.t$DOY %in% c(215:217),]
syv.p2<-aggregate(syv.d2, by=list(syv.d2$DecTime), FUN=mean, na.rm=TRUE)

wcr.d2<-wcr.flux.t[wcr.flux.t$DOY %in% c(214:216),]
wcr.p2<-aggregate(wcr.d2, by=list(wcr.d2$DecTime), FUN=mean, na.rm=TRUE)

rm('und.d2','syv.d2','wcr.d2')

#Position 3 (50mm depth, 40 - 60mm ring)
und.d3<-und.flux.t[und.flux.t$DOY %in% c(221:222),]
und.p3<-aggregate(und.d3, by=list(und.d3$DecTime), FUN=mean, na.rm=TRUE)

syv.d3<-syv.flux.t[syv.flux.t$DOY %in% c(219:221),]
syv.p3<-aggregate(syv.d3, by=list(syv.d3$DecTime), FUN=mean, na.rm=TRUE)

wcr.d3<-wcr.flux.t[wcr.flux.t$DOY %in% c(218:220),]
wcr.p3<-aggregate(wcr.d3, by=list(wcr.d3$DecTime), FUN=mean, na.rm=TRUE)

rm('und.d3','syv.d3','wcr.d3')

#Position 4 (70mm depth, 60 - 80mm ring)
und.d4<-und.flux.t[und.flux.t$DOY %in% c(224:225),]
und.p4<-aggregate(und.d4, by=list(und.d4$DecTime), FUN=mean, na.rm=TRUE)

syv.d4<-syv.flux.t[syv.flux.t$DOY %in% c(223:225),]
syv.p4<-aggregate(syv.d4, by=list(syv.d4$DecTime), FUN=mean, na.rm=TRUE)

wcr.d4<-wcr.flux.t[wcr.flux.t$DOY %in% c(222:223),]
wcr.p4<-aggregate(wcr.d4, by=list(wcr.d4$DecTime), FUN=mean, na.rm=TRUE)

rm('und.d4','syv.d4','wcr.d4')

#Scoop data together. This is attrociously inefficient.
wcr.c1<-cbind(wcr.p1[4],wcr.p2[4],wcr.p3[4],wcr.p4[4])
wcr.c2<-cbind(wcr.p1[5],wcr.p2[5],wcr.p3[5],wcr.p4[5])

syv.c1<-cbind(syv.p1[4],syv.p2[4],syv.p3[4],syv.p4[4])
syv.c2<-cbind(syv.p1[5],syv.p2[5],syv.p3[5],syv.p4[5])
syv.c3<-cbind(syv.p1[6],syv.p2[6],syv.p3[6],syv.p4[6])
syv.c4<-cbind(syv.p1[7],syv.p2[7],syv.p3[7],syv.p4[7])

und.c1<-cbind(und.p1[4],und.p2[4],und.p3[4],und.p4[4])
und.c2<-cbind(und.p1[5],und.p2[5],und.p3[5],und.p4[5])
und.c3<-cbind(und.p1[6],und.p2[6],und.p3[6],und.p4[6])
und.c4<-cbind(und.p1[7],und.p2[7],und.p3[7],und.p4[7])
und.c5<-cbind(und.p1[8],und.p2[8],und.p3[8],und.p4[8])

rm('syv.p1','syv.p2','syv.p3','syv.p4','und.p1','und.p2','und.p3','und.p4',
   'wcr.p1','wcr.p2','wcr.p3','wcr.p4')

#Combine into lists for looping
und.c<-list(und.c1,und.c2,und.c3,und.c4,und.c5)
syv.c<-list(syv.c1,syv.c2,syv.c3,syv.c4)
wcr.c<-list(wcr.c1,wcr.c2)

rm('und.c1','und.c2','und.c3','und.c4','und.c5','syv.c1',
        'syv.c2','syv.c3','syv.c4','wcr.c1','wcr.c2')


#Profiles

par(mfrow=c(1,2))

#UNDERC
datlist.und<-list()
profs.und<-matrix(data=NA, nrow=5, ncol=4) #space
for (r in 1:length(und.c)){  #each member of und.c is data from one of the UND sensors. Thus this loops over 5 sensors
  dat<-und.c[[r]]     #Pull sensor. Will be 288 rows b/c there are 288 5 min periods in a day
  dat.r<-dat/dat[,1]  #Flux relative to the normal 10mm position (theoretically highest flux rate); will normally be less than 1
  dat.r.sub<-dat.r[145:192,1:4] #Subset to midday 12 - 4 
  datlist.und[[r]]<-dat.r.sub
  profs.und[r,]<-colMeans(dat.r.sub)
  plot(colMeans(dat.r.sub), main=paste('UND C',r, sep=''))
  lines(colMeans(dat.r.sub))
}

#WCR
datlist.wcr<-list()
profs.wcr<-matrix(data=NA, nrow=length(wcr.c), ncol=4)
for (r in 1:length(wcr.c)){
  dat<-wcr.c[[r]]
  dat.r<-dat/dat[,1]
  dat.r.sub<-dat.r[145:192,1:4]
  datlist.wcr[[r]]<-dat.r.sub
  profs.wcr[r,]<-colMeans(dat.r.sub)
  plot(colMeans(dat.r.sub), main=paste('WCR C',r, sep=''))
  lines(colMeans(dat.r.sub))
}


#SYV
datlist.syv<-list()
profs.syv<-matrix(data=NA, nrow=length(syv.c), ncol=4)
for (r in 1:length(syv.c)){
  dat<-syv.c[[r]]
  dat.r<-dat/dat[,1]
  dat.r.sub<-dat.r[145:192,1:4]
  datlist.syv[[r]]<-dat.r.sub
  profs.syv[r,]<-colMeans(dat.r.sub)
  if(r==4){dat.r.sub[,4]<-NA}  #take out 4th measurement time for sensor 4 (sensor broke between depth 3 and 4)
  if(r==1 | r==4){  #Don't use 2 or 3; these sensors had strange behavior from day 1
    plot(colMeans(dat.r.sub), main=paste('SYV C',r, sep=''))
    lines(colMeans(dat.r.sub))}
}

rm('dat.r','dat.r.sub','dat')

#Aggregate profiles

syv.c.names<-c('S1', 'S2','S3','S4')
syv.c.valid<-c(2,3,1,3)  #Best guess: set by allometric sapwood depth of each tree, e.g. C1 must have predicted SWD either <50mm or <70mm
syv.agg<-(cbind(syv.c.names, profs.syv, syv.c.valid))
syv.agg[2:3,2:5]<-NaN #NAN broken sensors 2 & 3
syv.agg[4,5]<-NaN # NAN broken 4th band of sensor 4

und.c.names<-c('U1', 'U2','U3','U4','U5')
und.c.valid<-c(2,4,2,1,3)
und.agg<-cbind(und.c.names, profs.und, und.c.valid)

wcr.c.names<-c('W1','W2')
wcr.c.valid<-c(1,2)
wcr.agg<-cbind(wcr.c.names, profs.wcr, wcr.c.valid)

#All profiles in one place
profile<-as.data.frame(rbind(wcr.agg,und.agg,syv.agg))
colnames(profile)<-c('site','P1','P2','P3','P4','valid')

rm ('profs.syv','profs.und','profs.wcr','syv.agg','und.agg','wcr.agg')

#Fucking factors
profile$P1<-as.numeric(levels(profile$P1)[profile$P1])
profile$P2<-as.numeric(levels(profile$P2)[profile$P2])   
profile$P3<-as.numeric(levels(profile$P3)[profile$P3])
profile$P4<-as.numeric(levels(profile$P4)[profile$P4])
profile$valid<-as.numeric(levels(profile$valid)[profile$valid])

#Adjusted based on manual inspection of curves
adj.valid<-c(2,3,3,3,2,2,3,2,4,4,3) 
#Extended to points in the curve where it appeas sapwood continues 
#(i.e. there is substantial flux) even though allometry predicts thinner sapwood
profile$adj<-adj.valid

profile.val<-profile

#NANs unlikely profile values
for (r in 1:nrow(profile.val)){
  if(profile.val[r,7]<4){
  profile.val[r,((profile.val[r,7]+2):5)]<-NaN
  #This is terrible. The +2 (a) shifts for the site column and 
  #(b) makes sure that the first valid value is included.
  }
}

#Optionally take out the weird basswood value
profile.val.n<-profile.val
profile.val.n$P3[2]<-NaN
#Using a column index of 7 gives adapted indices. Column 6 has strict indices
#Overall profiles
plot(colMeans(profile.val[1:7, 2:5], na.rm=TRUE), main='Hardwoods')
lines(colMeans(profile.val[1:7, 2:5], na.rm=TRUE))

plot(colMeans(profile.val.n[1:7, 2:5], na.rm=TRUE), main='Hardwoods, no weird TIAM')
lines(colMeans(profile.val.n[1:7, 2:5], na.rm=TRUE))

plot(colMeans(profile.val[c(8,11), 2:5], na.rm=TRUE), main='Hemlock')
lines(colMeans(profile.val[c(8,11), 2:5], na.rm=TRUE))


#by species
par(mfrow=c(3,2), mar=c(4,5,2,0.3));ylab<-expression("Relative"~J[s])

plot(colMeans(profile.val[c(2,5), 2:5], na.rm=TRUE), main='TIAM', ylim=c(0,1.2), ylab=ylab, xlab="", xaxt='n')
lines(colMeans(profile.val[c(2,5), 2:5], na.rm=TRUE))
axis(side=1, at=c(1,2,3,4), labels=c(10,30,50,70))

plot(colMeans(profile.val[c(3:4), 2:5], na.rm=TRUE), main='POTR', ylim=c(0,1.2), ylab="", xlab="", xaxt='n')
lines(colMeans(profile.val[c(3:4), 2:5], na.rm=TRUE))
axis(side=1, at=c(1,2,3,4), labels=c(10,30,50,70))

plot(colMeans(profile.val[c(6:7), 2:5], na.rm=TRUE), main='Acer spp.', ylim=c(0,1.2), ylab=ylab, xlab="", xaxt='n')
lines(colMeans(profile.val[c(6:7), 2:5], na.rm=TRUE))
axis(side=1, at=c(1,2,3,4), labels=c(10,30,50,70))

plot(colMeans(profile.val[c(8,11), 2:5], na.rm=TRUE), main='TSCA', ylim=c(0,1.2), ylab="", xlab="Depth (mm)", xaxt='n')
lines(colMeans(profile.val[c(8,11), 2:5], na.rm=TRUE))
axis(side=1, at=c(1,2,3,4), labels=c(10,30,50,70))

plot(colMeans(profile.val[1, 2:5], na.rm=TRUE), main='OSVI', ylim=c(0,1.2), ylab=ylab, xlab="Depth (mm)", xaxt='n')
lines(colMeans(profile.val[1, 2:5], na.rm=TRUE))
axis(side=1, at=c(1,2,3,4), labels=c(10,30,50,70))

