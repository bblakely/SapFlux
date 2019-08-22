#script for setting all indices; these lines are also in 'refine tower data'

gs<-c(150:250) #growing season days
dayhr<-c(8:20)
midday<-c(12:15)

dayind<-which(twrdat$HOUR%in%dayhr) #made earlier; daytime hours
daymid<-which(twrdat$HOUR%in%midday)

gsind<-which(twrdat$DOY%in%gs)

daygs<-which(twrdat$HOUR%in%dayhr & twrdat$DOY%in%gs)
midgs<-which(twrdat$HOUR%in%midday & twrdat$DOY%in%gs)

