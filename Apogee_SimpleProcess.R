SYVraw<-read.csv('SYV_AP_clean.csv')
WCRraw<-read.csv('WCR_AP_clean.csv')


SYVcore<-SYVraw[,c(2:4, 7:10)]
WCRcore<-WCRraw[,c(2:4,7:10)]


SYVcore$Dectime=(SYVcore$H)+((SYVcore$M)/60)
SYVcore$DecDay=(SYVcore$DOY)+(SYVcore$Dectime/24)
WCRcore$Dectime=(WCRcore$H)+((WCRcore$M)/60)
WCRcore$DecDay=(WCRcore$DOY)+(WCRcore$Dectime/24)

##Earlier plots

# SYValign<-SYVcore[8673:38319,]
# 
# WCRmeans=colMeans(WCRcore[4:7], na.rm=TRUE)
# #WCRsdcalc=apply(WCRcore,2,sd, na.rm=TRUE)
# #WCRsd=WCRsdcalc[4:7]
# SYVmeans=colMeans(SYValign[4:7], na.rm=TRUE)
# #SYVsdcalc=apply(SYValign,2,sd, na.rm=TRUE)
# #SYVsd=SYVsdcalc[4:7]
# 
# #Differences=SYValign[,4:7]-WCRcore[4:7]
# #plot(SYValign$DecDay,Differences$T1, type='l')
# 
# AlignDifferences<-Differences[4600:29647,]
# #plot(SYValign$DecDay[4600:29647],AlignDifferences$T1, type='l')
# CombineSense<-rowMeans(AlignDifferences)
# plot(SYValign$DecDay[4600:29647],CombineSense, type='l', 
#      main='Temp. Differences',ylab='Temp. Difference (SYV-WCR)',
#      xlab='DOY')
# 
# SYValign2<-SYValign[4600:29647,]
# 
# Midday<-CombineSense[SYValign2$H == 12 |SYValign2$H == 11 |SYValign2$H == 13]
# mean(Midday, na.rm=TRUE)
# 
# Night<-CombineSense[SYValign2$H == 1 |SYValign2$H == 2 |SYValign2$H == 3]
# mean(Night, na.rm=TRUE)
# 
# AvgDiffs<-colMeans(AlignDifferences, na.rm=TRUE)
# DoubleAvg=mean(AvgDiffs)






