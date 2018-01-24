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
##  Water year defined as precip in Oct-Dec in year t and Jan-Sept in year t+1
##  following USGS.
first_water_months <- c("10","11","12") # first months in water year, to be promoted a year
# weather <- read.csv("../data/weather/ClimateIPM.csv")
weather <- read.csv("../data/weather/dubois_station_weather_01092018.csv") %>%
  dplyr::select(DATE, PRCP) %>%
  dplyr::rename("date" = DATE, "precip" = PRCP) %>%
  separate(date, into = c("year", "month", "day"), sep = "-") %>%
  mutate(precip = ifelse(is.na(precip), 0, precip)) %>% # set missing station data to 0
  mutate(year = as.numeric(year)) %>%
  mutate(water_year = ifelse(month %in% first_water_months, year+1, year)) %>% # create water years, based on USGS defintion
  filter(year != 1925) %>% # remove first year because don't have first water-year months
  group_by(water_year) %>%
  summarise(annual_precip = sum(precip)) %>%
  rename(year = water_year)

trt_data <- weather %>%
  filter(year > 2011) %>%
  filter(year < 2017) %>%
  select(year, annual_precip) %>%
  mutate(Treatment = "Control")

##  Adjust 2012 and 2015 values so they are distinguishable on the plot
trt_data[which(trt_data$year==2015),"annual_precip"] <- trt_data[which(trt_data$year==2015),"annual_precip"]-1
trt_data[which(trt_data$year==2014),"annual_precip"] <- trt_data[which(trt_data$year==2014),"annual_precip"]+1

ppt_histogram <- ggplot(weather, aes(x=annual_precip))+
  geom_histogram(bins=20,color="lightblue",fill="lightblue", aes(y=..density..), alpha = 0.5, size=0.00001)+
  geom_line(stat="density", color="blue")+
  geom_segment(data=trt_data, aes(x=annual_precip,xend=annual_precip,y=0.00045,yend=0), arrow = arrow(length = unit(0.02, "npc")))+
  geom_text(data=trt_data, aes(x=annual_precip, y=0.0009, label=year), angle=90, size=2.5)+
  scale_x_continuous(expand=c(0,0), limits=c(0,620), breaks=seq(0,600,100))+
  scale_y_continuous(expand=c(0,0), limits=c(0,0.0065))+
  ylab("Density")+
  xlab(expression(paste("Water-year Precipitation (mm ", yr^-1,")")))+
  theme_bw()+
  theme(panel.grid.minor = element_blank())



####
####  PLOT SOIL WATER BY TREATMENT ---------------------------------------------
####
mycols <- c("#009E73", "#D55E00", "#0072B2")

soilwat <- readRDS("../data/soil_moisture_data/SOILWAT_treatment_years.RDS") %>%
  select(date, year, Treatment, VWC_raw, doy) %>%
  rename(julian_date = doy) %>%
  mutate(julian_date = as.integer(julian_date))

soil_moisture <- read.csv("../data/soil_moisture_data/average_seasonal_soil_moisture.csv") %>%
  select(-year) %>%
  separate(simple_date, c("year","month","day")) %>%
  mutate(year = as.integer(year),
         Treatment = as.character(Treatment)) %>%
  # filter(type=="predicted") %>%
  ungroup() %>%
  left_join(soilwat, by = c("year","Treatment","julian_date")) %>%
  mutate(VWC_combo = ifelse(is.na(VWC)==TRUE, VWC_raw, VWC),
         VWC_source = ifelse(is.na(VWC)==TRUE, "soilwat", "observed")) %>%
  filter(month %in% c("03","04","05","06"))

suppressWarnings(# ignore warnings about missing values, we know they are empty
  soil_vwc <- ggplot(filter(soil_moisture, type == "predicted"), 
                     aes(x=julian_date, y=VWC, group=Treatment, color=Treatment))+
    # geom_line(data = filter(soil_moisture, type == "predicted"), size=0.3, linetype=2, aes(x=julian_date, y=VWC, group=Treatment, color=Treatment))+
    geom_line(data = filter(soil_moisture, VWC_source == "soilwat"), 
              aes(x=julian_date, y=VWC_combo, group=Treatment, color=Treatment),
              size=0.3, linetype=2)+
    geom_line(size=0.3)+
    scale_color_manual(values = mycols, name="Treatment")+
    ylab(expression(paste("Daily Soil VWC (ml ", ml^-1,")")))+
    xlab("Julian Day")+
    scale_y_continuous(breaks=seq(0,24,8))+
    scale_x_continuous(breaks=seq(0,250,20))+
    facet_grid(year~.)+
    theme_bw()+
    theme(panel.grid.minor = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_text(size=6))+
    guides(color=FALSE, linetype = FALSE)
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
ggsave("../figures/data_panels.pdf", gout, width=3.3, height=8, units="in")



####
####  GET MEAN PPT AND TEMPERATURES FROM 2011-2015 -----------------------------
####
weather <- read.csv("../data/weather/monthlyClimate.csv") %>%
  filter(year > 2010 & year < 2017)

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
