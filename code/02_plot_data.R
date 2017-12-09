################################################################################
##  02_plot_data.R: Script to plot the historical distribution of precipitation 
##  at Dubois, Idaho, and to compare our treatment years to historical events. 
##  The script also plots modeled VWC for each treatment through time, and the
##  temporal trend of ANPP for each treatment.
##
##  ----------------------------------------------------------------------------
##  Author: Andrew Tredennick (atredenn@gmail.com)
##  Date created: May 10, 2017
################################################################################

##  Clear the workspace
rm(list = ls(all.names = T))

##  Set the working directory (RStudio only)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



####
####  LOAD LIBRARIES -----------------------------------------------------------
####
library(tidyverse) # Data science packages
library(dplyr)     # Data summarizing and manipulating
library(stringr)   # Working with strings
library(ggthemes)  # Pleasing themes for ggplot
library(cowplot)   # For combining ggplot objects



####
####  READ IN WEATHER DATA AND PLOT --------------------------------------------
####
weather <- read.csv("../data/weather/ClimateIPM.csv")
trt_data <- weather %>%
  filter(year>2011) %>%
  select(year, ppt1) %>%
  mutate(Treatment = "Control")

##  Adjust 2012 and 2015 values so they are distinguishable on the plot
trt_data[which(trt_data$year==2015),"ppt1"] <- trt_data[which(trt_data$year==2015),"ppt1"]+10
trt_data[which(trt_data$year==2012),"ppt1"] <- trt_data[which(trt_data$year==2012),"ppt1"]-5

ppt_histogram <- ggplot(weather, aes(x=ppt1))+
  geom_histogram(bins=20,color="lightblue",fill="lightblue", aes(y=..density..), alpha = 0.5, size=0.00001)+
  geom_line(stat="density", color="blue")+
  geom_segment(data=trt_data, aes(x=ppt1,xend=ppt1,y=0.00045,yend=0), arrow = arrow(length = unit(0.02, "npc")))+
  geom_text(data=trt_data, aes(x=ppt1, y=0.0009, label=year), angle=90, size=2.5)+
  scale_x_continuous(expand=c(0,0), limits=c(0,620), breaks=seq(0,600,100))+
  scale_y_continuous(expand=c(0,0), limits=c(0,0.0085))+
  ylab("Density")+
  xlab("Growing Season Precipitation (mm)")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())



####
####  PLOT SOIL WATER BY TREATMENT ---------------------------------------------
####
mycols <- c("#009E73", "#D55E00", "#0072B2")
soil_moisture <- read.csv("../data/soil_moisture_data/average_seasonal_soil_moisture.csv") %>%
  select(-year) %>%
  separate(simple_date, c("year","month","day")) %>%
  filter(type=="observed") %>%
  ungroup()

suppressWarnings(# ignore warnings about missing values, we know they are empty
  soil_vwc <- ggplot(soil_moisture, aes(x=julian_date, y=VWC, group=Treatment, color=Treatment))+
    geom_line(size=0.5)+
    scale_color_manual(values = mycols, name="Treatment")+
    ylab(expression(paste("Daily Soil VWC (ml ", ml^-1,")")))+
    xlab("Julian Day")+
    scale_y_continuous(breaks=seq(0,24,8))+
    facet_grid(year~.)+
    theme_bw()+
    theme(panel.grid.minor = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_text(size=6))+
    guides(color=FALSE)
)



####
####  ANPP PLOT ----------------------------------------------------------------
####
permanent_quad_biomass <- readRDS("../data/estimated_biomass/permanent_plots_estimated_biomass.RDS")
permanent_quad_biomass <- permanent_quad_biomass %>% 
  filter(Treatment %in% c("Control","Drought","Irrigation"))

biomass_year_treatment <- permanent_quad_biomass %>%
  group_by(Treatment,year) %>%
  summarise(mean_biomass = mean(biomass_grams_est))

biomass_yr_trt_summ <- permanent_quad_biomass %>%
  filter(!str_detect(quadname, 'P1|P7')) %>%
  group_by(Treatment,year) %>%
  summarise(mean_biomass = mean(biomass_grams_est),
            sd_biomass = sd(biomass_grams_est)) %>%
  filter(year > 2011)

anpp_means <- ggplot(biomass_yr_trt_summ, aes(x=year, y=mean_biomass, color=Treatment))+
  geom_line()+
  geom_errorbar(aes(ymin=mean_biomass-sd_biomass, ymax=mean_biomass+sd_biomass), width=0.05)+
  geom_point(color="white", size=3)+
  geom_point()+
  geom_point(color="grey35", shape=1)+
  scale_color_manual(values = mycols, name="Treatment")+
  scale_x_continuous(breaks=c(2011:2016))+
  scale_y_continuous(breaks=seq(50,350,50))+
  ylab(expression(paste("ANPP (g ", m^-2,")")))+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  guides(color = guide_legend(override.aes = list(size=1)))+
  theme(legend.position = c(0.2, 0.75),legend.key.size = unit(1,"pt"),legend.title = element_text(size=10),
        legend.text = element_text(size = 8),
        legend.key.height = unit(0.8,"line"),
        legend.box.background = element_rect())



####
####  COMBINE PLOTS AND SAVE ---------------------------------------------------
####
suppressWarnings(# ignore warnings about missing values, we know they are empty
  gout <- cowplot::plot_grid(ppt_histogram,soil_vwc,anpp_means,
                             ncol=1, labels = c("A)","B)","C)"), hjust = -0.4)
)
ggsave("../figures/data_panels.png", gout, width=3.3, height=8, units="in", dpi=120)



####
####  GET MEAN PPT AND TEMPERATURES FROM 2011-2015 -----------------------------
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



####
####  CALCULATE MIN AND MAX ANPP VALUES, AVERAGED OVER TREATMENT ---------------
####
source("read_format_data.R") # load data
anpp_data %>%
  group_by(year) %>%
  summarise(mean_anpp = mean(anpp)) %>%
  write_csv(path = "../results/avg_anpp_by_year.csv")



####
####  CALCULATE TREATMENT DIFFERENCES ACROSS YEARS -----------------------------
####
anpp_data %>%
  group_by(Treatment) %>%
  summarise(mean_anpp = mean(anpp)) %>%
  mutate(base_anpp = mean_anpp[Treatment == "Control"],
         perc_diff = round(((mean_anpp - base_anpp)/base_anpp)*100)) %>%
  write_csv(path =  "../results/anpp_trt_diffs.csv")



####
####  CALCULATE VWC DIFFERENCES ACROSS YEARS -----------------------------------
####
soil_moisture %>%
  group_by(Treatment) %>%
  summarise(mean_vwc = mean(total_seasonal_vwc)) %>%
  mutate(base_vwc = mean_vwc[Treatment == "Control"],
         perc_diff = round(((mean_vwc - base_vwc)/base_vwc)*100)) %>%
  write_csv(path =  "../results/vwc_trt_diffs.csv")
