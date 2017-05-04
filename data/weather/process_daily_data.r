D=read.csv("USSES_climate.csv")
# how many missing values per month are allowed for temperature, ppt, and snow
# depth data? When these thresholds are exceeded, the monthly value is NA
TempNA=5
PptNA=5
SnwdNA=15

#--------------------------------------------------------
D=D[,c("year","month","day","Tmax","Tmin","Tmean","Prcp","Snow","Snwd")]

#make everything numeric
D$year=as.numeric(as.character(D$year))
D$month=as.numeric(as.character(D$month))
D$day=as.numeric(as.character(D$day))
D$Tmax=as.numeric(as.character(D$Tmax))
D$Tmin=as.numeric(as.character(D$Tmin))
D$Tmean=as.numeric(as.character(D$Tmean))
levels(D$Prcp)[length(levels(D$Prcp))]=0.005 # trace amounts
D$Prcp=as.numeric(as.character(D$Prcp))
levels(D$Snow)[length(levels(D$Snow))]=0.005 # trace amounts
D$Snow=as.numeric(as.character(D$Snow))
D$Snwd=as.numeric(as.character(D$Snwd))

# make missing observations=NA
tmp=D$year
D[D>999]=NA
D[,1]=tmp

# count missing observations in each month
missingD=aggregate(is.na(D[,4:NCOL(D)]),by=list("year"=D$year,"month"=D$month),FUN=sum)
missingD=missingD[order(missingD$year,missingD$month),]

# sum precip and snow by month
pptD=aggregate(D[,c("Prcp","Snow")],by=list("year"=D$year,"month"=D$month),FUN=sum,na.rm=T)
pptD=pptD[order(pptD$year,pptD$month),]
#average temperatures and snow depth by month
meanD=aggregate(D[,c("Tmax","Tmin","Tmean","Snwd")],by=list("year"=D$year,"month"=D$month),FUN=mean,na.rm=T)
meanD=meanD[order(meanD$year,meanD$month),]
names(meanD)[3:6]=paste("mean",names(meanD)[3:6],sep="")
# get min min temperature
minD=aggregate(D[,c("Tmin")],by=list("year"=D$year,"month"=D$month),FUN=min,na.rm=T)
minD=minD[order(minD$year,minD$month),]
Tmin=minD[,3]
#combine
monthlyD=cbind(meanD,pptD[,3:4],Tmin)

# put in missing values
monthlyD$meanTmax[missingD$Tmean>TempNA]=NA
monthlyD$meanTmin[missingD$Tmean>TempNA]=NA
monthlyD$meanTmean[missingD$Tmean>TempNA]=NA
monthlyD$meanSnwd[missingD$Snwd>SnwdNA]=NA
monthlyD$Prcp[missingD$Prcp>PptNA]=NA
monthlyD$Snow[missingD$Snow>PptNA]=NA
monthlyD$Tmin[missingD$Tmin>TempNA]=NA

#reshape and write monthly files
tmpOut=reshape(monthlyD[,c("year","month","Prcp")],timevar="month",idvar="year",
  direction="wide")
names(tmpOut)=c("YEAR","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC") 
write.table(tmpOut,"total_monthly_ppt_v2.csv",row.names=F,sep=",")

tmpOut=reshape(monthlyD[,c("year","month","Snow")],timevar="month",idvar="year",
  direction="wide")
names(tmpOut)=c("YEAR","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC") 
write.table(tmpOut,"total_monthly_snow_v2.csv",row.names=F,sep=",")

tmpOut=reshape(monthlyD[,c("year","month","meanSnwd")],timevar="month",idvar="year",
  direction="wide")
names(tmpOut)=c("YEAR","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC") 
write.table(tmpOut,"total_monthly_snowdepth_v2.csv",row.names=F,sep=",")

tmpOut=reshape(monthlyD[,c("year","month","meanTmean")],timevar="month",idvar="year",
  direction="wide")
names(tmpOut)=c("YEAR","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC") 
write.table(tmpOut,"monthly_mean_temp_v2.csv",row.names=F,sep=",")

tmpOut=reshape(monthlyD[,c("year","month","Tmin")],timevar="month",idvar="year",
  direction="wide")
names(tmpOut)=c("YEAR","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC") 
write.table(tmpOut,"monthly_min_min_temp_v2.csv",row.names=F,sep=",")




