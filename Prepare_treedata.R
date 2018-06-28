#Makes sapwood estimates for measured trees at SYV and WCR plots

#Read in data
wcr.tree<-read.csv('WCR_trees.csv')
syv.tree<-read.csv('SYV_trees.csv')

#Basal area
wcr.tree$BA<-3.14159*(wcr.tree$DBH/2)^2
syv.tree$BA<-3.14159*(syv.tree$DBH/2)^2

#Sapwood area
#Species Allometries
wcr.tree$SWA_calc[wcr.tree$SPP=='TSCA']<-4.2568*wcr.tree$DBH[wcr.tree$SPP=='TSCA']^1.37
wcr.tree$SWA_calc[wcr.tree$SPP=='ACSA']<-0.312*wcr.tree$DBH[wcr.tree$SPP=='ACSA']^2.07
wcr.tree$SWA_calc[wcr.tree$SPP=='BEAL']<-1.17*wcr.tree$DBH[wcr.tree$SPP=='BEAL']^1.79
wcr.tree$SWA_calc[wcr.tree$SPP=='OSVI']<-0.312*wcr.tree$DBH[wcr.tree$SPP=='OSVI']^2.07  #Just using ACSA allometry; no others found
wcr.tree$SWA_calc[wcr.tree$SPP=='TIAM']<-0.77*wcr.tree$DBH[wcr.tree$SPP=='TIAM']^1.868 #Aspen b/c no basswood found yet; originally used Bond-Lamberty aspen w/ 35 year old stand age.
wcr.tree$SWA_calc[wcr.tree$SPP=='FRPE']<-3.24*wcr.tree$DBH[wcr.tree$SPP=='FRPE']- 10.24 #Oak, b/c ring-porous.

syv.tree$SWA_calc[syv.tree$SPP=='TSCA']<-4.2568*syv.tree$DBH[syv.tree$SPP=='TSCA']^1.37
syv.tree$SWA_calc[syv.tree$SPP=='ACSA']<-0.312*syv.tree$DBH[syv.tree$SPP=='ACSA']^2.07
syv.tree$SWA_calc[syv.tree$SPP=='BEAL']<-1.17*syv.tree$DBH[syv.tree$SPP=='BEAL']^1.79
syv.tree$SWA_calc[syv.tree$SPP=='OSVI']<-0.312*syv.tree$DBH[syv.tree$SPP=='OSVI']^2.07  #Just using ACSA allometry; no others found
syv.tree$SWA_calc[syv.tree$SPP=='TIAM']<-0.77*syv.tree$DBH[syv.tree$SPP=='TIAM']^1.868 #Aspen b/c no basswood found yet; originally used Bond-Lamberty aspen w/ 35 year old stand age: 10^(2.415*log10(syv.tree$DBH[syv.tree$SPP=='TIAM'])-(0.033*35))
syv.tree$SWA_calc[syv.tree$SPP=='FRPE']<-3.24*syv.tree$DBH[syv.tree$SPP=='FRPE']- 10.24#Oak, b/c ring porous. Originally used red maple: 0.83*syv.tree$DBH[syv.tree$SPP=='FRPE']^1.97

#patch up SWA>BA problems
syv.tree$SWA_calc[syv.tree$SWA_calc>syv.tree$BA]<-syv.tree$BA[syv.tree$SWA_calc>syv.tree$BA]

#Back-calculating Sapwood depth (for radial profiles)
wcr.tree$SWD_calc<-(wcr.tree$DBH/2)-(sqrt((wcr.tree$BA-wcr.tree$SWA)/3.14159))
syv.tree$SWD_calc<-(syv.tree$DBH/2)-(sqrt((syv.tree$BA-syv.tree$SWA)/3.14159))

#Manually correct large trees at SYV becasue they skew so hard and allometries are bad with giant trees
#Correction dereved from Desai data (FIND)

#Reset Yellow birch SWD
syv.tree$SWD_calc[syv.tree$ID==41]<-(12.05*((syv.tree$DBH[syv.tree$ID==41]))^(-0.983))* (syv.tree$DBH[syv.tree$ID==41]/2) #This is a custom fit from Desai data
syv.tree$SWD_calc[syv.tree$ID==225]<-(12.05*((syv.tree$DBH[syv.tree$ID==225]))^(-0.983))* (syv.tree$DBH[syv.tree$ID==41]/2) #This is a custom fit from Desai data

#Yellow birch SWA (Normal calcualtion. Thanks high school geometry teacher)
syv.tree$SWA_calc[syv.tree$ID==41]<-(syv.tree$BA[syv.tree$ID==41])-(3.14159*((syv.tree$DBH[syv.tree$ID==41]/2)-(syv.tree$SWD_calc[syv.tree$ID==41]))^2)
syv.tree$SWA_calc[syv.tree$ID==225]<-(syv.tree$BA[syv.tree$ID==225])-(3.14159*((syv.tree$DBH[syv.tree$ID==225]/2)-(syv.tree$SWD_calc[syv.tree$ID==225]))^2)

#lazily do this for the maple and hemlock >50cm DBH
syv.tree$SWD_calc[syv.tree$ID==115]<-7.9137 #Where did this come from?
syv.tree$SWA_calc[syv.tree$ID==115]<-(syv.tree$BA[syv.tree$ID==115])-(3.14159*((syv.tree$DBH[syv.tree$ID==115]/2)-(syv.tree$SWD_calc[syv.tree$ID==115]))^2)

syv.tree$SWD_calc[syv.tree$ID==40]<-6.08 #Same here?
syv.tree$SWA_calc[syv.tree$ID==40]<-(syv.tree$BA[syv.tree$ID==40])-(3.14159*((syv.tree$DBH[syv.tree$ID==40]/2)-(syv.tree$SWD_calc[syv.tree$ID==40]))^2)

#Calculation for trees with measures SWD:
 #Estimated bark thickness; can do tree-specific when time

syv.tree$SWA_meas<-NA;syv.tree$meas<-'N' #Make columns for 'measured' SWA (from SWD) and a flag for whether this has a measured value or not
wcr.tree$SWA_meas<-NA;wcr.tree$meas<-'N'

calcSWA<-function(dat, bark=0){
  for (t in 1:nrow(dat)){
    if(!is.na(dat$SWD_meas[t])){  #If you have a measured value...
      
      BFDBH<-(dat$DBH[t]-bark*2) #bark-free DBH
      BFBA<-3.14*((0.5*BFDBH)^2) #bark-free area
      
      HWR<-(0.5*BFDBH)-dat$SWD_meas[t] ; HWA<-3.14*HWR^2 #heartwood radius, area
      
      dat$SWA_meas[t]<-BFBA-HWA #sapwood area
      dat$meas[t]<-'Y'

    }else{dat$SWA_meas[t]<-dat$SWA_calc[t]}
  }
  return(dat)
}

syv.tree<-calcSWA(syv.tree)
wcr.tree<-calcSWA(wcr.tree)

#Pull in radial profile multipliers.

#SYV and WCR_radial are excel-calculated sheets
#They apply the radial profiles calculated in Radial_calibration.R to 'rings' of sapwood depth

syv.calc.rp<-read.csv('SYV_radial.csv')
wcr.calc.rp<-read.csv('WCR_radial.csv')

syv.tree$MULT_calc<-syv.calc.rp[,24] #Column 24 is the multiplier for actual sapwood area to "effective" sapwood area
wcr.tree$MULT_calc<-wcr.calc.rp[,24] #i.e. where deeper sapwood with lower flux rates counts for less


