##  Script to plot results from the GLMM Stan object.
##
##  Author: Andrew Tredennick
##  Date created: May 25, 2017

##  Clear everything
rm(list=ls(all.names = T))

##  Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # only for RStudio



####
####  LOAD LIBRARIES ----
####
library(tidyverse)    # Data munging
library(dplyr)        # Data summarizing
library(ggthemes)     # Pleasing ggplot themes
library(stringr)      # Working with strings
library(rstan)        # For MCMC and Stan objects
library(gridExtra)    # For combining ggplot objects
library(viridis)



####
####  LOAD DATA AND MODEL RESULTS ----
####
source("read_format_data.R") # load data
all_fit <- readRDS("../results/randcoefs_alltreatments_fit.RDS")
# year_fit <- readRDS("../results/randyears_alltreatments_fit.RDS")

####
####  CALCULATE MAX(Pr(effect) > 0, Pr(effect) < 0) FUNCTION ----
####
get_one_tailed <- function(values){
  above <- 1 - ecdf(values)(0)
  below <- ecdf(values)(0)
  max(above,below)
}



####
####  PLOT TREATMENT-LEVEL POSTERIOR DISTRIBUTIONS ----
####
param_labels <- c("Control", "Drought", "Irrigation","ControlxSoilMoisture", "DroughtxSoilMoisture", "IrrigationxSoilMoisture")

betas <- data.frame( extract(all_fit, pars = 'beta') ) %>% 
  mutate( iteration = row_number()) %>% 
  gather( param_name, estimate, starts_with('beta'))

summary(all_fit, 'beta')$summary

betas$param_name <-  factor(betas$param_name, labels = param_labels)

ggplot( betas, aes( x = estimate, color = param_name)) + 
  geom_hline(aes(yintercept=0), color="grey45", size=0.1)+
  geom_line(stat="density",adjust=5,size=1) + 
  facet_grid(param_name ~ . ) + 
  xlab("Parameter Value")+
  ylab("Probability Density")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

TreatEffects <- 
  betas %>% 
  spread( param_name, estimate ) %>% 
  mutate( Drought = Drought + Control, 
          Irrigation = Irrigation + Control,  
          DroughtxSoilMoisture = DroughtxSoilMoisture + ControlxSoilMoisture, 
          IrrigationxSoilMoisture = IrrigationxSoilMoisture + ControlxSoilMoisture)


TreatEffects <- TreatEffects %>% gather( param_name , estimate, Control:IrrigationxSoilMoisture) %>% separate(param_name, c('Treatment', 'Type'), sep = 'x')

TreatEffects$Type[ is.na(TreatEffects$Type) ]  <- 'Intercept'

ggplot( TreatEffects, aes( x = estimate, color = Treatment)) + 
  geom_hline(aes(yintercept=0), color="grey45", size=0.1)+
  geom_line(stat="density",adjust=5,size=1) + 
  facet_wrap(~Type) + 
  xlab("Parameter Value")+
  ylab("Probability Density")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  theme(legend.position = c(0.4,0.78),
        legend.key.size = unit(6,"pt"),
        legend.title = element_text(size=10),
        legend.text = element_text(size = 8),
        legend.key.height = unit(0.8,"line"),
        legend.box.background = element_rect(),
        strip.background = element_blank(), 
        strip.text = element_text(size=10))+
  ggtitle("A)")
