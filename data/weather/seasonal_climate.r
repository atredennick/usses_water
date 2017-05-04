# read in monthly values, output seasonal and climate year totals

mD=read.csv("monthlyClimate.csv")

# add calendar year and season
mD$climYr=mD$year
mD$climYr[mD$month>9]=mD$climYr[mD$month>9]+1
mD$season="string"
mD$season[mD$month<=3]="Win"
mD$season[mD$month>3 & mD$month<=6]="Spr"
mD$season[mD$month>6 & mD$month<=9]="Sum"
mD$season[mD$month>9]="Fall"

# aggregate to calendar year and season and reshape
pptD=aggregate(PRCP~climYr+season,data=mD,FUN=sum)
pptD=reshape(pptD,timevar="season",idvar="climYr",direction="wide")
tmeanD=aggregate(TMEAN~climYr+season,data=mD,FUN=mean)
tmeanD=reshape(tmeanD,timevar="season",idvar="climYr",direction="wide")
seasD=merge(pptD,tmeanD,all=T)

# format for IPM
#seasD$pptAnnual=rowSums(seasD[,2:5])
#use July-June water year (to be consistent with old IPM model)
Nrows=NROW(seasD)
seasD$pptAnnual=c(NA,rowSums(seasD[2:Nrows,c("PRCP.Fall","PRCP.Win","PRCP.Spr")])+seasD$PRCP.Sum[1:(Nrows-1)])
seasD$ppt1=rowSums(seasD[,c(2,3,5)])
outD=seasD[,c("climYr","pptAnnual","ppt1","TMEAN.Spr")]
names(outD)=c("year","pptAnnual","ppt1","TmeanSpr1")
# add lag annual precip
tmpD=outD[,c("year","pptAnnual")]
tmpD$year=tmpD$year+1
names(tmpD)[2]="pptLag"
outD=merge(outD,tmpD,all.x=T)
# add next years ppt and tmean
tmpD=outD[,c("year","ppt1","TmeanSpr1")]
tmpD$year=tmpD$year-1
names(tmpD)[2:3]=c("ppt2","TmeanSpr2")
outD=merge(outD,tmpD,all.x=T)
outD=outD[,c("year","pptLag","ppt1","TmeanSpr1","ppt2","TmeanSpr2")]

write.csv(outD,"ClimateIPM.csv",row.names=F)
