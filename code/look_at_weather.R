##  look_at_weather.R: Script to look at the historical distribution of
##  precipitation at Dubois, Idaho, and to compare our treatment years to
##  historical events.
##
##  Author: Andrew Tredennick (atredenn@gmail.com)
##  Date created: May 10, 2017

##  Clear the workspace
rm(list = ls(all.names = T))

##  Set the working directory (RStudio only)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


####
####  LIBRARIES ----
####
library(tidyverse)
library(dplyr)
library(stringr)
library(ggthemes)
library(ggalt)
library(gridExtra)
library(RColorBrewer)



####
####  READ IN WEATHER DATA AND PLOT ----
####
weather <- read.csv("../data/weather/ClimateIPM.csv")
trt_data <- weather %>%
  filter(year>2011) %>%
  select(year, ppt1)
trt_data$Treatment <- "Control"
trt_data <- rbind(trt_data, data.frame(year = trt_data$year,
                                       ppt1 = trt_data$ppt1*0.5,
                                       Treatment = "Drought"))

ppt_histogram <- ggplot(weather, aes(x=ppt1))+
  geom_histogram(bins=20,color="white",fill="grey35")+
  geom_point(data=trt_data, aes(x=ppt1, y=rep(seq(0.3,1,length.out = 5),2),shape=Treatment, fill=as.factor(year)), size=2)+
  scale_x_continuous(expand=c(0,0), limits=c(0,620), breaks=seq(0,600,100))+
  scale_y_continuous(expand=c(0,0), breaks = seq(0,25,5),limits = c(0,28))+
  scale_shape_manual(values=c(21,22))+
  scale_fill_brewer(palette ="Set1", name="Year")+
  ylab("Frequency")+
  xlab("Growing Season Precipitation (mm)")+
  ggtitle("A")+
  theme_few()+
  theme(legend.position = c(0.8, 0.5),
        legend.key.size = unit(1,"pt"),
        legend.title = element_text(size=10),
        legend.text = element_text(size = 8),
        legend.key.height = unit(0.8,"line"))+
  guides(fill = guide_legend(override.aes = list(color = brewer.pal(5,"Set1"),size=1)),
         shape = guide_legend(override.aes = list(size=1)))
#ggsave(plot = g1,"../figures/historical_precip_trts.png", width = 4, height = 3.5, units="in", dpi=120)



####
####  READ IN ANPP DATA AND PLOT TREND ----
####
permanent_quad_biomass <- readRDS("../data/estimated_biomass/permanent_plots_estimated_biomass.RDS")



####
####  INITIAL PLOTS ----
####
permanent_quad_biomass <- permanent_quad_biomass %>% filter(Treatment %in% c("Control","Drought","Irrigation"))
biomass_year_treatment <- permanent_quad_biomass %>%
  group_by(Treatment,year) %>%
  summarise(mean_biomass = mean(biomass_grams_est))


biomass_yr_trt_summ <- permanent_quad_biomass %>%
  filter(!str_detect(quadname, 'P1|P7')) %>%
  group_by(Treatment,year) %>%
  summarise(mean_biomass = mean(biomass_grams_est),
            sd_biomass = sd(biomass_grams_est)) %>%
  filter(year > 2011)

stats_lab <- expression(paste("year*treatment  ", italic("P"), " = 0.67"))
anpp_means <- ggplot(biomass_yr_trt_summ, aes(x=year, y=mean_biomass, color=Treatment))+
  geom_line()+
  geom_errorbar(aes(ymin=mean_biomass-sd_biomass, ymax=mean_biomass+sd_biomass), width=0.05)+
  geom_point(color="white", size=3)+
  geom_point()+
  geom_point(color="grey35", shape=1)+
  # annotate("text",x=2015.1,y=190,label="a",size=3)+
  # annotate("text",x=2015.1,y=210,label="a",size=3)+
  # annotate("text",x=2015.1,y=148,label="b",size=3)+
  # annotate("text",x=2015.2,y=50,label="year%*%treatment~phantom(0)~italic('P')==0.67", parse=T, size=3)+
  scale_color_brewer(palette = "Set2", name="Treatment")+
  scale_x_continuous(breaks=c(2011:2016))+
  scale_y_continuous(breaks=seq(50,350,50))+
  ylab(expression(paste("ANPP (g ", m^-2,")")))+
  xlab("Year")+
  ggtitle("C")+
  theme_few()+
  guides(color = guide_legend(override.aes = list(size=1)))+
  theme(legend.position = c(0.2, 0.8),legend.key.size = unit(1,"pt"),legend.title = element_text(size=10),
        legend.text = element_text(size = 8),
        legend.key.height = unit(0.8,"line"))



####
####  PLOT SOIL WATER BY TREATMENT ----
####
soil_moisture <- read.csv("../data/soil_moisture_data/average_seasonal_soil_moisture.csv") %>%
  select(-year) %>%
  separate(simple_date, c("year","month","day")) %>%
  group_by(year,month,Treatment,type,year) %>%
  summarise(avg_vwc = mean(VWC,na.rm=TRUE)) %>%
  filter(type=="predicted") %>%
  mutate(month_year = as.factor(paste0(year,"-",month))) %>%
  ungroup()

soil_vwc <- ggplot(soil_moisture, aes(x=month_year, y=avg_vwc, group=Treatment, color=Treatment))+
  #geom_line()+
  geom_xspline(spline_shape=-0.5)+
  scale_color_brewer(palette = "Set2", name="Treatment")+
  ylab(expression(paste("Estimated Soil VWC (ml ", ml^-1,")")))+
  xlab("Date")+
  ggtitle("B")+
  scale_x_discrete(breaks = levels(soil_moisture$month_year)[c(T, rep(F, 3))])+
  scale_y_continuous(breaks=seq(2,16,2))+
  theme_few()+
  guides(color =FALSE)+
  theme(legend.position = c(0.2, 0.8),legend.key.size = unit(1,"pt"),legend.title = element_text(size=10),
        legend.text = element_text(size = 8),
        legend.key.height = unit(0.8,"line"),
        axis.text.x = element_text(angle = 90, hjust = 1))



####
####  MAKE SCATTERPLOT OF ANPP vs. PRECIP ----
####
source("read_format_data.R") # load data
data_plot <- ggplot(anpp_data, aes(x=ppt1,y=anpp))+
  geom_point(shape=21,color="grey25",alpha=0.8,aes(fill=Treatment))+
  #stat_smooth(method="lm", aes(color=Treatment), se=FALSE, size=0.7)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  scale_x_continuous(limits=c(100,300),breaks=seq(100,300,50))+
  scale_y_continuous(limits=c(40,360),breaks=seq(50,350,50))+
  xlab("Growing Season Precipitation")+
  ylab(expression(paste("ANPP (g ",m^2,")")))+
  ggtitle("D")+
  guides(fill=FALSE)+
  theme_few()+
  theme(legend.position = c(0.2,0.8),
        legend.key.size = unit(1,"pt"),
        legend.title = element_text(size=10),
        legend.text = element_text(size = 8),
        legend.key.height = unit(0.8,"line"))



####
####  COMBINE PLOTS AND SAVE ----
####
gout <- grid.arrange(ppt_histogram,soil_vwc,anpp_means,data_plot,ncol=2,nrow=2)
ggsave("../figures/Figure1.png", gout, width=7, height=6, units="in", dpi=120)



####
####  GET MEAN PPT AND TEMPERATURES FROM 2011-2015 ----
####
weather <- read.csv("../data/weather/monthlyClimate.csv") %>%
  filter(year > 2010 & year < 2016)

mar <- weather %>%
  group_by(year) %>%
  summarise(ann_ppt = sum(PRCP, na.rm=T))
avg_mar <- mean(mar$ann_ppt)

temperature <- weather %>%
  group_by(month) %>%
  summarise(avg_temp = mean(TMEAN))

weather_table <- data.frame(avg_ann_precip = avg_mar,
                            min_avg_monthly_temp = min(temperature$avg_temp),
                            max_avg_monthly_temp = max(temperature$avg_temp),
                            month_for_min = as.numeric(temperature[which(temperature$avg_temp==min(temperature$avg_temp)), "month"]),
                            month_for_max = as.numeric(temperature[which(temperature$avg_temp==max(temperature$avg_temp)), "month"]))

write.csv(x = weather_table, file = "../results/weather_summary.csv")

