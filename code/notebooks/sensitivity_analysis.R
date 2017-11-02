##  anpp_randcoefs_model.R: Script to run GLMM analysis to test for treatment 
##  effects on the relationship between precipitation and ANPP.
##
##  NOTE: Stan may issue a couple warnings after running the MCMC that, as
##  the messages state, can be safely ignored. Just rejects a couple proposals
##  that result in ill-formed covariance matrices.
##
##  Author: Andrew Tredennick
##  Date created: November

##  Clear everything
rm(list = ls(all.names = TRUE))

##  Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # only for RStudio
setwd("../")


####
####  LOAD LIBRARIES ----
####
library(tidyverse)    # Data munging
library(dplyr)        # Data summarizing
library(ggthemes)     # Pleasing ggplot themes
library(stringr)      # Working with strings
library(rstan)        # For MCMC
library(lme4)         # Mixed-effects modeling



####
####  READ IN AND EXTRACT EXPERIMENT ANPP DATA ----
####
source("read_format_data.R")

##  By treatment
sens_anpp <- anpp_data %>%
  group_by(Treatment, year) %>%
  summarise(avg_anpp = mean(anpp)) %>%
  spread(Treatment, avg_anpp) %>%
  mutate(control_drought = Control - Drought,
         control_irrigation = Control - Irrigation) %>%
  select(-Control,-Drought,-Irrigation) %>%
  gather(comparison, anpp_diff, -year)

sens_vwc <- anpp_data %>%
  group_by(Treatment, year) %>%
  summarise(vwc = mean(total_seasonal_vwc)) %>%
  spread(Treatment, vwc) %>%
  mutate(control_drought = Control - Drought,
         control_irrigation = Control - Irrigation) %>%
  select(-Control,-Drought,-Irrigation) %>%
  gather(comparison, vwc_diff, -year)

sensitivity <- left_join(sens_anpp, sens_vwc, by = c("year", "comparison")) %>%
  mutate(sens = anpp_diff / vwc_diff)

ggplot(sensitivity, aes(x=year, y=sens, color=comparison))+
  geom_line()


##  By plot-treatment -- compare each treatment plot to each control in each year
controls <- filter(anpp_data, Treatment == "Control") %>%
  select(Treatment,quadname,year,anpp)

droughts <- filter(anpp_data, Treatment == "Drought") %>%
  select(Treatment,quadname,year,anpp)

irrigates <- filter(anpp_data, Treatment == "Irrigation") %>%
  select(Treatment,quadname,year,anpp)

for(do_year in unique(droughts$year)){
  year_controls <- filter(controls, year==do_year)
  year_droughts <- filter(droughts, year==do_year)
  for(do_plot in unique(year_droughts$quadname)){
    plot_droughts <- filter(year_droughts, quadname == do_plot)
    diffs <- year_controls$anpp - plot_droughts$anpp
    print(diffs)
  } # end plot
  
} # end year

