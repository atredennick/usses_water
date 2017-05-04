
# import raw NCDC data
# calculate monthly totals 
# write to file

#Data Types:   
#TMAX - Maximum temperature (degrees C)
#SNWD - Snow depth (mm)
#SNOW - Snowfall (mm)
#PRCP - Precipitation (mm)
#TMIN - Minimum temperature (degrees C) 

input<-"USSES_climate.csv"

rawD=read.csv(input)
rawD=rawD[,c("DATE","PRCP","SNOW","SNWD","TMAX","TMIN")]
rawD[rawD==9999 | rawD==-9999]=NA  # replace missing value indicator

# convert PRCP to mm, SNOW to cm, and temps to degress C
#rawD[,2:NCOL(rawD)]=rawD[,2:NCOL(rawD)]/10

rawD$TMEAN=(rawD$TMAX+rawD$TMIN)/2

# format date
rawD$year=as.numeric(substring(rawD$DATE,1,4))
rawD$month=as.numeric(substring(rawD$DATE,5,6))
rawD$day=as.numeric(substring(rawD$DATE,7,8))



rawD=rawD[,-1]  # drop raw date column

# aggregate to month
monthD=aggregate(PRCP~year+month,data=rawD,na.action=na.omit,FUN=sum)
tmp=aggregate(SNOW~year+month,data=rawD,na.action=na.omit,FUN=sum)
monthD=merge(monthD,tmp,all=T)
tmp=aggregate(SNWD~year+month,data=rawD,na.action=na.omit,FUN=mean)
monthD=merge(monthD,tmp,all=T)
tmp=aggregate(TMIN~year+month,data=rawD,na.action=na.omit,FUN=mean)
monthD=merge(monthD,tmp,all=T)
tmp=aggregate(TMAX~year+month,data=rawD,na.action=na.omit,FUN=mean)
monthD=merge(monthD,tmp,all=T)
tmp=aggregate(TMEAN~year+month,data=rawD,na.action=na.omit,FUN=mean)
monthD=merge(monthD,tmp,all=T)

#ggplot(monthD, aes(x=as.factor(month),y=TMAX,group=year))+geom_line(alpha=0.5)

# flag bad months
missing=aggregate(is.na(rawD[,1:6])~year+month,data=rawD,FUN=sum)
names(missing)[3:8]=paste("miss.",names(rawD)[1:6],sep="")
monthD=merge(monthD,missing,all=T)

monthD$PRCP[monthD$miss.PRCP>10]=NA
monthD$SNOW[monthD$miss.SNOW>10]=NA
monthD$SNWD[monthD$miss.SNWD>15]=NA
monthD$TMIN[monthD$miss.TMIN>15]=NA
monthD$TMAX[monthD$miss.TMAX>15]=NA
monthD$TMEAN[monthD$miss.TMEAN>15]=NA

# write output
write.csv(monthD,"monthlyClimate.csv",row.names=F)
