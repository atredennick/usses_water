##  anpp_anova.R: Script to run ANOVA analysis to test for treatment effects
##  on ANPP. Runs ANOVA for entirety of experiment and independent tests
##  within years.
##
##  Author: Andrew Tredennick
##  Date created: May 23, 2017

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
library(rstan)        # For MCMC




####
####  READ IN AND EXTRACT EXPERIMENT ANPP DATA ----
####
source("read_format_data.R")



####
####  SET UP AND FIT MODEL IN STAN ----
####
fit_stan_model <- function(model_data, check_diags=FALSE, treattype){
  lmod <- lm(log(anpp) ~ ppt1_scaled, model_data)
  x <- model.matrix(lmod)
  newx <- unique(x)
  anppdat <- list(Nobs = nrow(model_data),
                  Npreds = ncol(x),
                  Nplots = length(unique(model_data$quadname)),
                  Ntreats = length(unique(model_data$Treatment)),
                  Nppts = nrow(xnew),
                  y = as.numeric(scale(log(model_data$anpp))),
                  x = x,
                  newx = newx,
                  plot_id = as.numeric(as.factor(model_data$quadname)),
                  treat_id = as.numeric(as.factor(as.character(model_data$Treatment))),
                  R = diag(1,ncol(x)))
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  rt <- stanc("anpp_randcoefs.stan")
  sm <- stan_model(stanc_ret = rt, verbose=FALSE)
  set.seed(123)
  fit <- sampling(sm, data=anppdat, iter=2000, chains=3, thin=2)
  
  if(check_diags){
    pnames <- c("sigmaeps", "sigmaint", "sigmaslope","beta[1]",
                "beta[2]","beta[3]","beta[4]","beta[5]")
    for(p in pnames){
      muc <- rstan::extract(fit, pars=p,  permuted=FALSE, inc_warmup=FALSE)
      mdf <- reshape2::melt(muc)
      dir.create("./scratch/")
      ggplot(mdf,aes(x=iterations,y=value,color=chains)) + 
        geom_line() + 
        ylab(mdf$parameters[1])
      ggsave(paste0("./scratch/diag_",treattype,"_",p,".png"), 
             height = 4, width = 5, units = "in", dpi=72)
    }
  }
  return(fit)
}

drought_fit <- fit_stan_model(drought_data,check_diags = T,treattype = "drought")
irrigate_fit <- fit_stan_model(irrigate_data,check_diags = T,treattype = "irrigate")
saveRDS(drought_fit, "../results/randcoefs_drought_fit.RDS")
saveRDS(irrigate_fit, "../results/randcoefs_irrigate_fit.RDS")

