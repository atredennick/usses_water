##  plot_soil_moisture.R: Script to aggregate and plot preprocessed soil
##  moisture data from experimental plots.
##
##  Author: Andrew Tredennick
##  Date created: May 15, 2017

##  Clear the workspace
rm(list=ls(all.names = T))

##  Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # only for RStudio



####
####  LOAD LIBRARIES ----
####
library(tidyverse)
library(dplyr)
library(ggthemes)
library(stringr)



####
####  READ IN AND FORMAT DATA ----
####
soil_water <- read.csv("../data/soil_moisture_data/data/processed_data/soil_files/USSES_1_2_C_SoilWater.csv") %>%
  mutate(Date = as.character(Date)) %>%
  separate(Date, c("year","month","day"))

ggplot(soil_water, aes(x=doy,y=VWC_L1))+
  geom_line()+
  facet_wrap("year")

water <- readRDS("../data/soil_moisture_data/data/processed_data/decagon_data_with_station_data.RDS")


