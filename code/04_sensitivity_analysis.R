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
  dplyr::select(-Control,-Drought,-Irrigation) %>%
  gather(comparison, anpp_diff, -year)

sens_vwc <- anpp_data %>%
  group_by(Treatment, year) %>%
  summarise(vwc = mean(avg_vwc)) %>%
  spread(Treatment, vwc) %>%
  mutate(control_drought = Control - Drought,
         control_irrigation = Control - Irrigation) %>%
  dplyr::select(-Control,-Drought,-Irrigation) %>%
  gather(comparison, vwc_diff, -year)

sensitivity <- left_join(sens_anpp, sens_vwc, by = c("year", "comparison")) %>%
  mutate(sens = anpp_diff / vwc_diff)

# ggplot(sensitivity, aes(x=year, y=sens, color=comparison))+
#   geom_line()


##  By plot-treatment -- compare each treatment plot to each control in each year
controls <- filter(anpp_data, Treatment == "Control") %>%
  dplyr::select(Treatment,quadname,year,anpp)

droughts <- filter(anpp_data, Treatment == "Drought") %>%
  dplyr::select(Treatment,quadname,year,anpp)

irrigates <- filter(anpp_data, Treatment == "Irrigation") %>%
  dplyr::select(Treatment,quadname,year,anpp)

drought_diffs <- {}
for(do_year in unique(droughts$year)){
  year_controls <- filter(controls, year==do_year)
  year_droughts <- filter(droughts, year==do_year)
  for(do_plot in unique(year_droughts$quadname)){
    plot_droughts <- filter(year_droughts, quadname == do_plot)
    diffs <- mean(year_controls$anpp) - plot_droughts$anpp
    tmp_out <- data.frame(treatment = "drought",
                          year = do_year,
                          quadname = do_plot,
                          cntrl_minus_treat = diffs)
    drought_diffs <- rbind(drought_diffs, tmp_out)
  } # end plot
} # end year

irrigate_diffs <- {}
for(do_year in unique(droughts$year)){
  year_controls <- filter(controls, year==do_year)
  year_irrigates <- filter(irrigates, year==do_year)
  for(do_plot in unique(year_irrigates$quadname)){
    plot_irrigates <- filter(year_irrigates, quadname == do_plot)
    diffs <- mean(year_controls$anpp) - plot_irrigates$anpp
    tmp_out <- data.frame(treatment = "irrigation",
                          year = do_year,
                          quadname = do_plot,
                          cntrl_minus_treat = diffs)
    irrigate_diffs <- rbind(irrigate_diffs, tmp_out)
  } # end plot
} # end year

vwc_diffs <- sens_vwc %>%
  separate(comparison, c("control","treatment")) %>%
  dplyr::select(-control)

all_diffs <- rbind(drought_diffs, irrigate_diffs) %>%
  mutate(treatment = as.character(treatment)) %>%
  left_join(vwc_diffs, by = c("year","treatment")) %>%
  mutate(sensitivity = cntrl_minus_treat / vwc_diff)

saveRDS(object = all_diffs, file = "../results/sensitvities.RDS")



####
####  FIT LINEAR MODELS FOR EFFECT OF TIME -------------------------------------
####
drought_fit <- lm(formula = sensitivity ~ year, 
                  data = filter(all_diffs, treatment == "drought"))
irrigate_fit <- lm(formula = sensitivity ~ year, 
                   data = filter(all_diffs, treatment == "irrigation"))

pvals <- c(summary(drought_fit)$coefficients[2,4], 
           summary(irrigate_fit)$coefficients[2,4])

data.frame(treatment = c("drought", "irrigation"),
           pvalues = pvals) %>%
  write_csv("../results/sensitivity_year_pvalues.csv")


