# This version (2) based on daily, not monthly, NCDC data

setwd("H:\\idahochart\\records\\weather")

## PPT
pptD = read.csv("total_monthly_ppt_v2.csv")

#convert to mm
pptD[,2:13]=pptD[,2:13]*25.4

pptD$pptWin=rowSums(pptD[,c("JAN","FEB","MAR")])
pptD$pptSpr=rowSums(pptD[,c("APR","MAY","JUN")])
pptD$pptSum=rowSums(pptD[,c("JUL","AUG","SEP")])
pptD$pptFall=rowSums(pptD[,c("OCT","NOV","DEC")])

Nrows=NROW(pptD)
pptD$pptAnnual=c(NA,rowSums(pptD[2:Nrows,c("pptWin","pptSpr")])+rowSums(pptD[1:(Nrows-1),c("pptSum","pptFall")]))

lagPpt = pptD[,c("YEAR","pptAnnual")]
names(lagPpt)[2]="pptLag1"
lagPpt$YEAR=lagPpt$YEAR+1

pptD=merge(pptD,lagPpt,all.x=T)

## SNOW-------------------------------------------------------
snowD = read.csv("total_monthly_snow_v2.csv")
#convert to cm
snowD[,2:13]=snowD[,2:13]*2.54
Nrows=NROW(snowD)
#sum snowDec thru March
snowD$snow=c(NA,(snowD$DEC[1:(Nrows-1)]+rowSums(snowD[2:Nrows,2:4])))
snowD$FebMarSnow=snowD$FEB+snowD$MAR


## SNOW DEPTH-----------------------------------------------------
snowdD = read.csv("total_monthly_snowdepth_v2.csv")
#convert to cm
snowdD[,2:13]=snowdD[,2:13]*2.54
Nrows=NROW(snowdD)
#sum snowDec thru March
snowdD$Sdepth=NA
snowdD$Sdepth[2:Nrows]=rowMeans(cbind(snowdD$DEC[1:(Nrows-1)],snowdD[2:Nrows,2:4]))
snowdD$FebMarSDepth=rowMeans(snowdD[,c("FEB","MAR")])


## MEAN TEMP-------------------------------------------------------
TmeanD = read.csv("monthly_mean_temp_v2.csv")
#convert to C
TmeanD[,2:13]=(5/9)*(TmeanD[,2:13]-32)

TmeanD$TmeanWin=rowMeans(TmeanD[,c("JAN","FEB","MAR")])
TmeanD$TmeanSpr=rowMeans(TmeanD[,c("APR","MAY","JUN")])
TmeanD$TmeanSum=rowMeans(TmeanD[,c("JUL","AUG","SEP")])
TmeanD$TmeanFall=rowMeans(TmeanD[,c("OCT","NOV","DEC")])

Nrows=NROW(TmeanD)
TmeanD$TmeanAnnual==c(NA,rowSums(TmeanD[2:Nrows,c("TmeanWin","TmeanSpr")])+rowSums(TmeanD[1:(Nrows-1),c("TmeanSum","TmeanFall")]))


# format output
climD=merge(pptD[,c(1,14:NCOL(pptD))],snowD[,c(1,14,15)],all=T)
climD=merge(climD,snowdD[,c(1,14,15)],all=T)
climD=merge(climD,TmeanD[,c(1,14:NCOL(TmeanD))],all=T)

write.table(climD,"seasonal_climate_Jul2Jun_v2.csv",row.names=F,sep=",")
