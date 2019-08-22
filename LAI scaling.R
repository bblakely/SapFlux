#LAI scaling

LAI<-read.csv('LAI_2016_2017.csv')

g<-LAI$WCR
m<-LAI$WCRM

cutLAI<-((g-0.56*m)/0.44)

rlai<-0.5 #road LAI proportion. Estiamted 1/2 usual LAI

roadfac<-2800/(2000+(800*rlai)) #rough accounting of road influence

LAI$WCRC<-cutLAI*roadfac

1-(LAI$WCRC/m)

#sapflux reduced by 30%/LE boosted by 30%

#LE per leaf area
plot(wcr.twr$LE[gsind]/0.7)

wcr.lemo<-aggregate(wcr.twr$LE/0.7, by=list(wcr.twr$MONTH), FUN='mean')

