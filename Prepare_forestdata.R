syv.forest.raw<-read.csv('SYV_FS.csv')

syv.forest<-syv.forest.raw[,1:8]
syv.forest$species<-as.character(syv.forest$species)
syv.forest$species[syv.forest$species=='acsa?']<-'acsa'  #assume we were right

#dummy forest file for same allometries
#syv.forest.dum<-syv.forest
#syv.forest.dum$SWA<-0.312*syv.forest$dbh^2.07

#Species Allometries
syv.forest$SWA<-NaN

syv.forest$SWA[syv.forest$species=='tsca']<-4.2568*syv.forest$dbh[syv.forest$species=='tsca']^1.37
syv.forest$SWA[syv.forest$species=='acsa']<-0.312*syv.forest$dbh[syv.forest$species=='acsa']^2.07
syv.forest$SWA[syv.forest$species=='beal']<-1.17*syv.forest$dbh[syv.forest$species=='beal']^1.79
syv.forest$SWA[syv.forest$species=='osvi']<-0.312*syv.forest$dbh[syv.forest$species=='osvi']^2.07  #Just using ACSA allometry; no others found
syv.forest$SWA[syv.forest$species=='tiam']<-10^(2.415*log10(syv.forest$dbh[syv.forest$species=='tiam'])-(0.033*35)) #Using aspen b/c no others found; also assumes trees are 35 years old
syv.forest$SWA[syv.forest$species=='frpe']<-0.83*syv.forest$dbh[syv.forest$species=='frpe']^1.97 #Using red maple because no others found
#unknowns treated like maple
syv.forest$SWA[syv.forest$species=='uk']<-0.312*syv.forest$dbh[syv.forest$species=='uk']^2.07



syv.forest$SWD<-NaN
syv.forest$SWD<-(syv.forest$dbh/2)-(sqrt((syv.forest$ba-syv.forest$SWA)/3.14159))


#Big tree corrections

bigbirch<-which(syv.forest$dbh>50 & syv.forest$species=='beal')
bigmaple<-which(syv.forest$dbh>50 & syv.forest$species=='acsa')
bighemlock<-which(syv.forest$dbh>50 & syv.forest$species=='tsca')
bigunknown<-which(syv.forest$dbh>50 & syv.forest$species=='uk')

#birch
syv.forest$SWD[bigbirch]<-(12.05*(syv.forest$dbh[bigbirch])^(-0.983))*(syv.forest$dbh[bigbirch]/2)
syv.forest$SWA[bigbirch]<-(syv.forest$ba[bigbirch])-(3.14159*((syv.forest$dbh[bigbirch]/2)-(syv.forest$SWD[bigbirch]))^2)

#maple
syv.forest$SWD[bigmaple]<-(-0.0073*(syv.forest$dbh[bigmaple])+0.7991)*(syv.forest$dbh[bigmaple]/2)
syv.forest$SWA[bigmaple]<-(syv.forest$ba[bigmaple])-(3.14159*((syv.forest$dbh[bigmaple]/2)-(syv.forest$SWD[bigmaple]))^2)
#unknowns as maples
syv.forest$SWD[bigunknown]<-(-0.0073*(syv.forest$dbh[bigunknown])+0.7991)*(syv.forest$dbh[bigunknown]/2)
syv.forest$SWA[bigunknown]<-(syv.forest$ba[bigunknown])-(3.14159*((syv.forest$dbh[bigunknown]/2)-(syv.forest$SWD[bigunknown]))^2)


#hemlock
syv.forest$SWD[bighemlock]<-(-0.0127*(syv.forest$dbh[bighemlock])+0.9696)*(syv.forest$dbh[bighemlock]/2)
syv.forest$SWA[bighemlock]<-(syv.forest$ba[bighemlock])-(3.14159*((syv.forest$dbh[bighemlock]/2)-(syv.forest$SWD[bighemlock]))^2)

Vars<-c('id','species','dbh', 'cc', 'ba','SWA','SWD')
syv.forest.dat<-subset(syv.forest, select=Vars)

#write.csv(syv.forest.dat, 'SYV_FOREST.csv')

###Now do it all again for WCR
wcr.forest.raw<-read.csv('WCR_FS.csv')

wcr.forest<-wcr.forest.raw[,c(1:8, 12)]
wcr.forest$species<-as.character(wcr.forest$species)

#Making a dummy forest file for all same allometries
#wcr.forest.dum<-wcr.forest
#wcr.forest.dum$SWA<-0.312*wcr.forest$dbh^2.07

#Species Allometries
wcr.forest$SWA<-NaN

wcr.forest$SWA[wcr.forest$species=='tsca']<-4.2568*wcr.forest$dbh[wcr.forest$species=='tsca']^1.37
wcr.forest$SWA[wcr.forest$species=='acsa']<-0.312*wcr.forest$dbh[wcr.forest$species=='acsa']^2.07
wcr.forest$SWA[wcr.forest$species=='beal']<-1.17*wcr.forest$dbh[wcr.forest$species=='beal']^1.79
wcr.forest$SWA[wcr.forest$species=='osvi']<-0.312*wcr.forest$dbh[wcr.forest$species=='osvi']^2.07  #Just using ACSA allometry; no others found
wcr.forest$SWA[wcr.forest$species=='tiam']<-10^(2.415*log10(wcr.forest$dbh[wcr.forest$species=='tiam'])-(0.033*35)) #Using aspen b/c no others found; also assumes trees are 35 years old
wcr.forest$SWA[wcr.forest$species=='frpe']<-0.83*wcr.forest$dbh[wcr.forest$species=='frpe']^1.97 #Using red maple because no others found
wcr.forest$SWA[wcr.forest$species=='quru']<-3.24*wcr.forest$dbh[wcr.forest$species=='quru']-10.24 #Using red maple because no others found
#Js of quru can be found in Bovard

#unknowns treated like maple
wcr.forest$SWA[wcr.forest$species=='uk']<-0.312*wcr.forest$dbh[wcr.forest$species=='uk']^2.07


wcr.forest$SWD<-NaN
wcr.forest$SWD<-(wcr.forest$dbh/2)-(sqrt((wcr.forest$ba-wcr.forest$SWA)/3.14159))


#Big tree corrections

bigmaple<-which(wcr.forest$dbh>50 & wcr.forest$species=='acsa')
bigunknown<-which(wcr.forest$dbh>50 & wcr.forest$species=='uk')

#maple
wcr.forest$SWD[bigmaple]<-(-0.0073*(wcr.forest$dbh[bigmaple])+0.7991)*(wcr.forest$dbh[bigmaple]/2)
wcr.forest$SWA[bigmaple]<-(wcr.forest$ba[bigmaple])-(3.14159*((wcr.forest$dbh[bigmaple]/2)-(wcr.forest$SWD[bigmaple]))^2)
#unknowns as maples
wcr.forest$SWD[bigunknown]<-(-0.0073*(wcr.forest$dbh[bigunknown])+0.7991)*(wcr.forest$dbh[bigunknown]/2)
wcr.forest$SWA[bigunknown]<-(wcr.forest$ba[bigunknown])-(3.14159*((wcr.forest$dbh[bigunknown]/2)-(wcr.forest$SWD[bigunknown]))^2)


Vars<-c('id','species','dbh', 'cc', 'ba','SWA','SWD')
wcr.forest.dat<-subset(wcr.forest, select=Vars)

write.csv(wcr.forest.dat, 'WCR_FOREST.csv')



