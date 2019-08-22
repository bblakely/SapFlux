#source('SapFlow_Calc.R')
site.id<-'wcr'

wcr.raw<-read.csv("WCR_2015_SAPFLUX.csv")  #2016 data for both
syv.raw<-read.csv("SYV_2015_SAPFLUX.csv")

#Generalize by site
if(site.id=="wcr"){
  rawdata<-wcr.raw
  flux.site<-rawdata[5:18]
  nsens<-14
}else if(site.id=='syv'){
  rawdata<-syv.raw
  flux.site<-rawdata[5:24]
  nsens<-20
  flux.site$S11<-flux.site$S10
}



#STEP 1: LINEAR INTERPOLATION
flux.interp<-data.frame(matrix(data=-9999,nrow=nrow(flux.site), ncol=ncol(flux.site)))
colnames(flux.interp)<-colnames(flux.site)
flux.interp[flux.interp==-9999]<-NA
#Vars for plotting
interp.spacemap<-data.frame(matrix(data=NA,nrow=nrow(flux.site), ncol=ncol(flux.site)))
#Reusable params
gaplength<-4  #max number of consecutuve missing values to interpolate across

for(i in 1:nsens){
  if(!is.na(mean(flux.site[,i], na.rm=TRUE))){ #Line to skip over all-NA sensors
  sens<-flux.site[,i]  #Pull sensor to fill
  col<-rep('black', length(sens))
  gap<-which(is.na(sens))  #Where are missing data
  spaces<-rep(0,length(gap)) #Find distance between consecutive missing data
  for(x in 2:length(gap)){  #There must be a better way to do this...
    spaces[x]<-(gap[x]-gap[x-1])
  }
  start.ind<-which(spaces!=1) #Values greater than 1 indicate non-consecutive missing data, i.e. the breaks between two gaps
  starts<-gap[start.ind]
  ends<-c(gap[start.ind-1],gap[length(gap)]) #The number before the start of a new gap is the end of the previous gap
  lengths<-(ends-starts)+1 #Add one to include gap end members
  
  fills<-cbind(starts[lengths<=gaplength],ends[lengths<=gaplength], lengths[lengths<=gaplength])
  
  if(nrow(fills)>0){  #If there are fillable gaps
  for(r in 1:nrow(fills)){  #Each row is one gap; start, end, length
  interp<-approx(c(sens[fills[r,1]-1],sens[fills[r,2]+1]), n=fills[r,3]+2)$y #Approx needs end values so we need to start one before and end one after the gap 
  sens[fills[r,1]:fills[r,2]]<-interp[2:(length(interp)-1)] #This trims the end back off and fits them into the gap
  col[fills[r,1]:fills[r,2]]<-"red"  #Not sure why this isn't working
  }
  }
  
  #Diagnostic plots
  flux.interp[,i]<-sens 
  plot(sens,type='l', main=i)
  sens.space<-rep(NA, length(sens))
  sens.space[col=="red"]<-sens[col=="red"]
  lines(sens.space, col='red', lwd=3)
  
  interp.spacemap[,i]<-sens.space  #Make a lasting variable so we can look at interpolated spots later
}
  }
rm('gap', 'spaces','start.ind','starts','ends',
     'lengths','fills','interp','sens', 'sens.space') #get rid of a bunch of extra vars



#STEP 2: GAPFILL WITH BEST FIT ONSITE SENSOR
par(mfrow=c(2,2))
par(xpd=FALSE)
bestfits<-matrix(0,4,nsens)

startdate<-100 #Timespan to use for gapfill
enddate<-300 

DAT.SITE<-flux.interp[rawdata$DOY>startdate & rawdata$DOY<enddate,]  #Use interpolated values

insite.fillmap<-data.frame(matrix(data=NA,nrow=nrow(flux.site), ncol=ncol(flux.site))) #Create to look at insite filling later

#2.1 Gather information about interpolation
for (s in 1:nsens){
  if(!is.na(mean(DAT.SITE[,s], na.rm=TRUE))){
  sens.test<-DAT.SITE[,s]
  fitvec<-rep(0,ncol(DAT.SITE)) #R2
  slopevec<-rep(0,ncol(DAT.SITE)) #Slope
  intvec<-rep(0,ncol(DAT.SITE)) #Intercept
  
  for (i in 1:(ncol(DAT.SITE))){ 
    if(!is.na(mean(DAT.SITE[,i], na.rm=TRUE))){
    resid<-sens.test-DAT.SITE[,i] #Next several lines remove crazy outliers
    th.h<-quantile(resid, 0.99, na.rm=TRUE)
    th.l<-quantile(resid, 0.01, na.rm=TRUE)
    sens.test[resid>th.h | resid < th.l]<-NaN
    
    plot(sens.test~DAT.SITE[,i], pch='.', main=paste(s,'vs', i)) #Scatterplot
    fit<-lm(sens.test~DAT.SITE[,i]) #Fit regression across sensors and store parameters
    params<-unname(fit[[1]])
    slopevec[i]<-params[2]
    intvec[i]<-params[1]
    abline(params[1], params[2], col='red')
    fitvec[i]<-summary(fit)$r.squared
    
    bf<-max(fitvec[fitvec!=1]) #Find best fit (highest R2)
    }
    }
  
  print(paste('Best match for sensor', s, 'is sensor', which(fitvec==bf), "with r2 =", fitvec[which(fitvec==bf)]))
  bestfits[1,s]<-slopevec[which(fitvec==bf)] #Store slope, intercept, R2, and sensor ID of best fit sensor
  bestfits[2,s]<-intvec[which(fitvec==bf)]
  bestfits[3,s]<-fitvec[which(fitvec==bf)]
  bestfits[4,s]<-which(fitvec==bf)
}
  }

rm('resid','th.h','th.l')

#2.2 Actual filling
flux.gf.dat<-matrix(NA,nrow(flux.site), nsens) #Will hold final series
flux.fills<-matrix(NA,nrow(flux.site), nsens) #Will hold just gapfilled parts, mainly for plotting

for(i in 1:nsens){
  if(!is.na(mean(flux.interp[,i], na.rm=TRUE))){
  flux.gap<-flux.interp[,i] #pull out sensor of interest
  pair.ind<-bestfits[4,i] #Which sensor is best match?
  flux.pair<-flux.interp[,pair.ind] #Pull out matching sensor
  
  flux.fill<-(flux.pair*bestfits[1,i])+bestfits[2,i] #Create full regressed timeseries
 
  flux.gf<-flux.gap
  flux.gf[is.na(flux.gap)]<-flux.fill[is.na(flux.gap)] #fill in gaps with regressed time series
  
  flux.gf.dat[,i]<-flux.gf #enter into dataframe
  
  #Diagnostic plots
  flux.fills<-rep(NA, length(flux.gap))
  flux.fills[is.na(flux.gap)]<-flux.fill[is.na(flux.gap)] #pull filled gaps for diagnostic plots
  
  insite.fillmap[,i]<-flux.fills #store filled spots for later plotting
  
  plot(flux.gap, type='l', lwd=3, main=i)
  lines(interp.spacemap[,i], col='red', lwd=3) #Interpolation fills
  lines(insite.fillmap[,i], col='green') #Insite regression fills
  }
}

if(site.id=="wcr"){flux.gf.dat->wcr.gapfill}else{flux.gf.dat->syv.gapfill}

#[[[if time, against next closest sensor]]]



# #STEP 3: GAPFILL WITH SYV SENSORS
# 
# #Where are all sensors off
# none<-which((rowSums(flux.wcr.gf, na.rm=TRUE)==0))
# 
# bestfits.c<-matrix(0,4,14)
# DAT.WCR<-flux.wcr.gf
# DAT.SYV<-flux.syv
# 
# for (s in 1:14){  #Indexed for 14 WCR sensors
#   sens.test<-DAT.WCR[,s]
#   fitvec<-rep(0,ncol(DAT.WCR))
#   slopevec<-rep(0,ncol(DAT.WCR))
#   intvec<-rep(0,ncol(DAT.WCR))
#   for (i in 1:(ncol(DAT.SYV))){ #Cycle through 21 syv sensors
#     resid<-sens.test-DAT.SYV[,i]
#     th.h<-quantile(resid, 0.99, na.rm=TRUE)
#     th.l<-quantile(resid, 0.01, na.rm=TRUE)
#     sens.test[resid>th.h | resid < th.l]<-NaN
#     plot(DAT.SYV[,i],sens.test, pch='.', main=paste(s,'vs SYV', i))
#     fit<-lm(sens.test~DAT.SYV[,i])
#     params<-unname(fit[[1]])
#     slopevec[i]<-params[2]
#     intvec[i]<-params[1]
#     abline(params[1], params[2], col='red')
#     #print(paste("Sensor", i, ":", summary(fit)$r.squared))
#     fitvec[i]<-summary(fit)$r.squared
#     bf.c<-max(fitvec[fitvec!=1])
#     #print(paste('best match for sensor', s, 'is sensor', which(fitvec==bf)))
#   }
#   print(paste('Best match for sensor', s, 'is sensor SYV', 
#               which(fitvec==bf.c), "with r2 =", fitvec[which(fitvec==bf.c)]))
#   bestfits.c[1,s]<-slopevec[which(fitvec==bf.c)]
#   bestfits.c[2,s]<-intvec[which(fitvec==bf.c)]
#   bestfits.c[3,s]<-fitvec[which(fitvec==bf.c)]
#   bestfits.c[4,s]<-which(fitvec==bf.c)
# }
# 
# partners.c<-cbind(1:14, bestfits.c[4,])
# 
# flux.wcr.mass<-matrix(NA,nrow(flux.wcr.gf), 14)
# flux.wcr.mass.fills<-matrix(NA,nrow(flux.wcr.gf), 14)
# for(i in 1:14){
#   flux.gap.mass<-flux.wcr.gf[,i] #pull out sensor of interest
#   pair.ind<-bestfits.c[4,i] #Which sensor is best match?
#   flux.pair.mass<-DAT.SYV[,pair.ind] #Pull out matching sensor
#   
#   flux.gf.mass<-(flux.pair.mass*bestfits.c[1,i])+bestfits.c[2,i] #Create full regressed timeseries
#   
#   flux.gap.filled<-flux.gap.mass
#   flux.gap.filled[none]<-flux.gf.mass[none] #fill in gaps with regressed time series
#   
#   flux.wcr.mass[,i]<-flux.gap.filled #enter into dataframe
#   
#   #DIAGNOSTIC PLOTS
#   flux.fills.mass<-rep(NA, length(flux.gap))
#   flux.fills.mass[none]<-flux.gf.mass[none] #pull filled gaps for diagnostic plots
#   
#   plot(flux.gap.mass, type='l', lwd=3, main=i)
#   lines(interp.spacemap[,i], col='red')  #fill from linear interpolation
#   lines(insite.fillmap[,i], col='green') #fill from within-site regression
#   lines (flux.fills.mass, col='blue')
# }
# 
# for(m in 1:ncol(flux.wcr.mass)){
#   prct<-round(((length(which(is.na(flux.wcr.mass[,m])))/nrow(flux.wcr.mass))*100), digits=2)
#   print(paste('S',m,': ',prct,'% ','still missing', sep=''))
# }
# 
