##  anpp_anova.R: Script to run ANOVA analysis to test for treatment effects
##  on ANPP. Runs ANOVA for entirety of experiment and independent tests
##  within years.
##
##  Author: Andrew Tredennick
##  Date created: May 12, 2017

##  Clear everything
rm(list=ls(all.names = T))

##  Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # only for RStudio



####
####  LOAD LIBRARIES ----
####
library(tidyverse)    # Data munging
library(dplyr)        # Data summarizing
library(broom)        # Working with model output
library(stringr)      # Working with strings
library(car)          # Type II ANOVA function
library(lsmeans)      # Post-hoc pairwise comparisons
library(multcompView) # Just for viewing pairwise results



####
####  READ IN AND EXTRACT EXPERIMENT ANPP DATA ----
####
data_path <- "../data/estimated_biomass/"
fname <- "permanent_plots_estimated_biomass.RDS"
permanent_quad_biomass <- readRDS(paste0(data_path,fname))

anpp_data <- permanent_quad_biomass %>% 
  filter(Treatment %in% c("Control","Drought","Irrigation")) %>%
  filter(!str_detect(quadname, 'P1|P7')) %>%
  group_by(Treatment,year) %>%
  filter(year > 2011)



####
####  FIT ALL YEAR ANOVAS ----
####
all_model <- lm(log(biomass_grams_est) ~ year*Treatment, 
                data=anpp_data)
car::Anova(all_model)
cld(lsmeans(all_model,"Treatment",adjust="Tukey"), alpha=0.05, Letters=letters)

# Irrigation
all_drt_model <-  lm(log(biomass_grams_est) ~ year*Treatment, 
                     data=filter(anpp_data, Treatment!="Irrigation"))
car::Anova(all_drt_model)

# Drought
all_irr_model <- lm(log(biomass_grams_est) ~ year*Treatment, 
                    data=filter(anpp_data, Treatment!="Drought"))
car::Anova(all_irr_model)



####
####  FIT YEAR-INDEPENDENT ANOVAS ----
####
years <- unique(anpp_data$year)
capture.output(
               for(doyear in years){
                 tmpd <- filter(anpp_data, year==doyear)
                 drought_mod <- lm(log(biomass_grams_est) ~ Treatment, 
                                   data=filter(tmpd, Treatment!="Irrigation"))
                 irrigat_mod <- lm(log(biomass_grams_est) ~ Treatment, 
                                   data=filter(tmpd, Treatment!="Drought"))
                 
                 print(doyear)
                 print("******************************************************")
                 print("DROUGHT")
                 print(car::Anova(drought_mod))
                 
                 cat("\n\n")
                 
                 print("IRRIGATION")
                 print(car::Anova(irrigat_mod))
                 
                 print("******************************************************")
                 cat("\n\n\n\n")
               },
               file = "../results/within_year_anovas.txt")


##  Run again for LaTeX table
stats_out <- {}
for(doyear in years){
  tmpd <- filter(anpp_data, year==doyear)
  drought_mod <- lm(log(biomass_grams_est) ~ Treatment, 
                    data=filter(tmpd, Treatment!="Irrigation"))
  irrigat_mod <- lm(log(biomass_grams_est) ~ Treatment, 
                    data=filter(tmpd, Treatment!="Drought"))
  
  drt <- broom::tidy(car::Anova(drought_mod))
  irr <- broom::tidy(car::Anova(irrigat_mod))
  
  irr$Effect <- "Irrigation"
  drt$Effect <- "Drought"
  tmpout <- rbind(irr,drt)
  tmpout <- filter(tmpout, term=="Treatment")
  tmpout$df <- paste0(as.character(tmpout$df),", 12")
  tmpout$Year <- doyear
  tmpout <- select(tmpout, -c(term,sumsq))
  tmpout <- tmpout[,c("Year","Effect","df","statistic","p.value")]
  colnames(tmpout) <- c("Year","Effect","df","F","P")
  stats_out <- rbind(stats_out, tmpout)
}

# Save output dataframe for LaTeX table making in R Markdown
saveRDS(object = stats_out, "../results/within_year_anova_table.RDS")



####
####  MORE COMPLEX MODEL WITH PPT ----
####
weather <- read.csv("../data/weather/ClimateIPM.csv")
anpp_data <- anpp_data %>%
  left_join(weather)

ggplot(anpp_data, aes(x=ppt1, y=biomass_grams_est, color=as.factor(year), shape=Treatment))+
  geom_point()+
  facet_wrap(~Treatment)

drought_mod <- lm(log(biomass_grams_est) ~ ppt1 + Treatment*year, 
                  data=filter(anpp_data, Treatment!="Irrigation"))
summary(drought_mod)
car::Anova(drought_mod, type=2)

irrigate_mod <- lm(biomass_grams_est ~ ppt1 + Treatment*year, 
                  data=filter(anpp_data, Treatment!="Drought"))
summary(irrigate_mod)
car::Anova(irrigate_mod, type=2)
