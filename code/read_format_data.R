##  Script to read in and format data for use in all analysis scripts. Subsets
##  out some plots and split data into irrigation and drought sets.
##
##  Author: Andrew Tredennick
##  Date created: May 25, 2017


####
####  LOAD LIBRARIES ----
####
library(tidyverse)    # Data munging
library(dplyr)        # Data summarizing
library(stringr)      # Working with strings




####
####  READ IN AND EXTRACT EXPERIMENT ANPP DATA ----
####
data_path <- "../data/estimated_biomass/"
anpp_fname <- "permanent_plots_estimated_biomass.RDS"
permanent_quad_biomass <- readRDS(paste0(data_path,anpp_fname))

weather <- read.csv("../data/weather/ClimateIPM.csv") %>%
  select(-pptLag,-ppt2,-TmeanSpr1,-TmeanSpr2)

anpp_data <- permanent_quad_biomass %>% 
  filter(Treatment %in% c("Control","Drought","Irrigation")) %>%
  filter(!str_detect(quadname, 'P1|P7')) %>%
  filter(year > 2011) %>%
  rename(anpp = biomass_grams_est) %>%
  left_join(weather, by = "year") %>%
  select(-QuadName,-quad,-Grazing,-paddock,-ndvi) %>%
  mutate(ppt1_scaled = as.numeric(scale(ppt1)),
         year_id = year - 2011)

drought_data <- anpp_data %>%
  filter(Treatment != "Irrigation") %>%
  mutate(trt_id = as.numeric(as.factor(Treatment)) - 1)

irrigate_data <- anpp_data %>%
  filter(Treatment != "Drought") %>%
  mutate(trt_id = as.numeric(as.factor(Treatment)) - 1)

