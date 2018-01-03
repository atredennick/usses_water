##  Script to read in and format data for use in all analysis scripts. Subsets
##  out some plots and split data into irrigation and drought sets.
##
##  Author: Andrew Tredennick
##  Date created: May 25, 2017


# ####
# ####  LOAD LIBRARIES ----
# ####
library(tidyverse)    # Data munging
library(dplyr)        # Data summarizing
library(stringr)      # Working with strings




####
####  READ IN AND EXTRACT EXPERIMENT ANPP DATA ----
####
weather <- read.csv("../data/weather/ClimateIPM.csv") %>%
  select(-pptLag,-ppt2,-TmeanSpr1,-TmeanSpr2)

soil_moisture <- read.csv("../data/soil_moisture_data/average_seasonal_soil_moisture.csv") %>%
  dplyr::select(-year) %>%
  separate(simple_date, c("year","month","day")) %>%
  group_by(year,month,Treatment,type,year) %>%
  summarise(avg_vwc = mean(VWC,na.rm=TRUE)) %>%
  filter(type=="predicted") %>%
  mutate(month_year = paste0(year,"-",month)) %>%
  filter(month %in% c("03","04","05","06")) %>%
  group_by(year,Treatment) %>%
  summarise(total_seasonal_vwc = sum(avg_vwc, na.rm = T)) %>%
  ungroup() %>%
  mutate(year = as.numeric(year)) %>%
  filter(year != 2012) %>%
  left_join(weather, by = "year")

soil_moisture <- read.csv("../data/soil_moisture_data/average_seasonal_soil_moisture.csv") %>%
  select(-year) %>%
  separate(simple_date, c("year","month","day")) %>%
  filter(type=="observed") %>%
  filter(month %in% c("03","04","05","06")) %>%
  ungroup()

ggplot(soil_moisture, aes(x=julian_date, y=VWC, group=Treatment, color=Treatment))+
  geom_line(size=0.5)+
  ylab(expression(paste("Daily Soil VWC (ml ", ml^-1,")")))+
  xlab("Julian Day")+
  scale_y_continuous(breaks=seq(0,24,8))+
  facet_grid(year~.)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank(), 
        strip.text = element_text(size=6))+
  guides(color=FALSE)




##  Read in weather data and summarise by year
new_weather <- read.csv("../data/weather/dubois_station_weather_01032018.csv") %>%
  dplyr::select(DATE, PRCP) %>%
  dplyr::rename("date" = DATE, "precip" = PRCP) %>%
  separate(date, into = c("year", "month", "day"), sep = "-") %>%
  group_by(year) %>%
  summarise(annual_precip = sum(precip))
print(new_weather)

##  All years except 2015 and 2016 have NA values
weather_nas <- read.csv("../data/weather/dubois_station_weather_01032018.csv") %>%
  dplyr::select(DATE, PRCP) %>%
  dplyr::rename("date" = DATE, "precip" = PRCP) %>%
  separate(date, into = c("year", "month", "day"), sep = "-") %>%
  dplyr::filter(is.na(precip)) %>%
  group_by(year) %>%
  summarise(num_nas = length(precip))

##  Set those NAs to zero since the precip values are just descriptive
new_weather <- read.csv("../data/weather/dubois_station_weather_01032018.csv") %>%
  dplyr::select(DATE, PRCP) %>%
  dplyr::rename("date" = DATE, "precip" = PRCP) %>%
  separate(date, into = c("year", "month", "day"), sep = "-") %>%
  mutate(precip = ifelse(is.na(precip), 0, precip)) %>%
  group_by(year) %>%
  summarise(annual_precip = sum(precip)) %>%
  left_join(weather_nas, by = "year") %>%
  mutate(num_nas = ifelse(is.na(num_nas), 0, num_nas))

##  Look at distribution of precip in 2013 and 2014, years with many NAs
na_years <- read.csv("../data/weather/dubois_station_weather_01032018.csv") %>%
  dplyr::select(DATE, PRCP) %>%
  dplyr::rename("date" = DATE, "precip" = PRCP) %>%
  separate(date, into = c("year", "month", "day"), sep = "-") %>%
  dplyr::filter(year %in% c(2013,2014)) %>%
  dplyr::mutate(date = paste(year,month,day,sep = "-")) 

ggplot(na_years, aes(x =date, y = precip, color = year, group = year))+
  geom_line()