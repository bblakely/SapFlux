#Makes sapwood estimates for measured trees at SYV and WCR plots

#Read in data
wcr.tree<-read.csv('WCR_trees.csv')
syv.tree<-read.csv('SYV_trees.csv')

#Basal area
wcr.tree$BA<-3.14159*(wcr.tree$DBH/2)^2
syv.tree$BA<-3.14159*(syv.tree$DBH/2)^2

#Sapwood area
#Species Allometries
wcr.tree$SWA[wcr.tree$SPP=='TSCA']<-4.2568*wcr.tree$DBH[wcr.tree$SPP=='TSCA']^1.37
wcr.tree$SWA[wcr.tree$SPP=='ACSA']<-0.312*wcr.tree$DBH[wcr.tree$SPP=='ACSA']^2.07
wcr.tree$SWA[wcr.tree$SPP=='BEAL']<-1.17*wcr.tree$DBH[wcr.tree$SPP=='BEAL']^1.79
wcr.tree$SWA[wcr.tree$SPP=='OSVI']<-0.312*wcr.tree$DBH[wcr.tree$SPP=='OSVI']^2.07  #Just using ACSA allometry; no others found
wcr.tree$SWA[wcr.tree$SPP=='TIAM']<-0.77*wcr.tree$DBH[wcr.tree$SPP=='TIAM']^1.868 #Aspen b/c no basswood found yet; originally used Bond-Lamberty aspen w/ 35 year old stand age.
wcr.tree$SWA[wcr.tree$SPP=='FRPE']<-3.24*wcr.tree$DBH[wcr.tree$SPP=='FRPE']- 10.24 #Oak, b/c ring-porous.

syv.tree$SWA[syv.tree$SPP=='TSCA']<-4.2568*syv.tree$DBH[syv.tree$SPP=='TSCA']^1.37
syv.tree$SWA[syv.tree$SPP=='ACSA']<-0.312*syv.tree$DBH[syv.tree$SPP=='ACSA']^2.07
syv.tree$SWA[syv.tree$SPP=='BEAL']<-1.17*syv.tree$DBH[syv.tree$SPP=='BEAL']^1.79
syv.tree$SWA[syv.tree$SPP=='OSVI']<-0.312*syv.tree$DBH[syv.tree$SPP=='OSVI']^2.07  #Just using ACSA allometry; no others found
syv.tree$SWA[syv.tree$SPP=='TIAM']<-0.77*syv.tree$DBH[syv.tree$SPP=='TIAM']^1.868 #Aspen b/c no basswood found yet; originally used Bond-Lamberty aspen w/ 35 year old stand age: 10^(2.415*log10(syv.tree$DBH[syv.tree$SPP=='TIAM'])-(0.033*35))
syv.tree$SWA[syv.tree$SPP=='FRPE']<-3.24*syv.tree$DBH[syv.tree$SPP=='FRPE']- 10.24#Oak, b/c ring porous. Originally used red maple: 0.83*syv.tree$DBH[syv.tree$SPP=='FRPE']^1.97

#Back-calculating Sapwood depth (for radial profiles)
wcr.tree$SWD<-(wcr.tree$DBH/2)-(sqrt((wcr.tree$BA-wcr.tree$SWA)/3.14159))
syv.tree$SWD<-(syv.tree$DBH/2)-(sqrt((syv.tree$BA-syv.tree$SWA)/3.14159))


#Manually correct large trees at SYV becasue they skew so hard and allometries are bad with giant trees
#Correction dereved from Desai data (FIND)

#Reset Yellow birch SWD
syv.tree[syv.tree$ID==41,9]<-(12.05*((syv.tree$DBH[syv.tree$ID==41]))^(-0.983))* (syv.tree$DBH[syv.tree$ID==41]/2) #This is a custom fit from Desai data
syv.tree[syv.tree$ID==225,9]<-(12.05*((syv.tree$DBH[syv.tree$ID==225]))^(-0.983))* (syv.tree$DBH[syv.tree$ID==41]/2) #This is a custom fit from Desai data

#Yellow birch SWA (Normal calcualtion. Thanks high school geometry teacher)
syv.tree[syv.tree$ID==41,8]<-(syv.tree$BA[syv.tree$ID==41])-(3.14159*((syv.tree$DBH[syv.tree$ID==41]/2)-(syv.tree$SWD[syv.tree$ID==41]))^2)
syv.tree[syv.tree$ID==225,8]<-(syv.tree$BA[syv.tree$ID==225])-(3.14159*((syv.tree$DBH[syv.tree$ID==225]/2)-(syv.tree$SWD[syv.tree$ID==225]))^2)

#lazily do this for the maple and hemlock >50cm DBH
syv.tree[syv.tree$ID==115,9]<-7.9137 #Where did this come from?
syv.tree[syv.tree$ID==115,8]<-(syv.tree$BA[syv.tree$ID==115])-(3.14159*((syv.tree$DBH[syv.tree$ID==115]/2)-(syv.tree$SWD[syv.tree$ID==115]))^2)

syv.tree[syv.tree$ID==40,9]<-6.08 #Same here?
syv.tree[syv.tree$ID==40,8]<-(syv.tree$BA[syv.tree$ID==40])-(3.14159*((syv.tree$DBH[syv.tree$ID==40]/2)-(syv.tree$SWD[syv.tree$ID==40]))^2)


#Pull in radial profile multipliers.

#SYV and WCR_radial are excel-calculated sheets 
#They apply the radial profiles calculated in Radial_calibration.R to 'rings' of sapwood depth

syv.calc.rp<-read.csv('SYV_radial.csv')
wcr.calc.rp<-read.csv('WCR_radial.csv')

syv.tree$Multiplier<-syv.calc.rp[,24] #Column 24 is the multiplier for actual sapwood area to "effective" sapwood area
wcr.tree$Multiplier<-wcr.calc.rp[,24] #i.e. where deeper sapwood with lower flux rates counts for less
