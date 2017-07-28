##  anpp_randcoefs_model.R: Script to run GLMM analysis to test for treatment 
##  effects on the relationship between precipitation and ANPP.
##
##  NOTE: Stan will issue a couple warnings after running the MCMC that, as
##  the messages state, can be safely ignored. Just rejects a couple proposals
##  that result in ill-formed covariance matrices.
##
##  Author: Andrew Tredennick
##  Date created: June 2, 2017

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
  lmod <- lm(log(anpp) ~ vwc_scaled, model_data)
  x <- model.matrix(lmod)
  newx <- unique(x)
  anppdat <- list(Nobs = nrow(model_data),
                  Npreds = ncol(x),
                  Nplots = length(unique(model_data$quadname)),
                  Ntreats = length(unique(model_data$Treatment)),
                  Nppts = nrow(newx),
                  Nyears = length(unique(model_data$year)),
                  y = as.numeric(scale(log(model_data$anpp))),
                  x = x,
                  newx = newx,
                  plot_id = as.numeric(as.factor(model_data$quadname)),
                  treat_id = as.numeric(as.factor(as.character(model_data$Treatment))),
                  year_id = as.numeric(as.factor(model_data$year)),
                  R = diag(1,ncol(x)))
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  rt <- stanc("anpp_randcoefs.stan")
  sm <- stan_model(stanc_ret = rt, verbose=FALSE)
  set.seed(123)
  fit <- sampling(sm, data=anppdat, iter=2000, chains=4, 
                  thin=2, control = list(adapt_delta = 0.99))
  
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

all_fit <- fit_stan_model(anpp_data,check_diags = FALSE, treattype = "all")
saveRDS(all_fit, "../results/randcoefs_alltreatments_fit.RDS")



####
####  PLOT RESIDUALS ----
####
# predictions <- reshape2::melt(rstan::extract(all_fit, pars="yhat")) %>%
#   rename(iteration = iterations, obs_id = Var2, estimate = value, stan_name = L1) %>%
#   group_by(obs_id) %>%
#   summarise(mean_estimate = mean(estimate))
# 
# resids <- reshape2::melt(rstan::extract(all_fit, pars="resid")) %>%
#   rename(iteration = iterations, obs_id = Var2, resid = value, stan_name = L1) %>%
#   left_join(predictions, by="obs_id") %>%
#   group_by(obs_id) %>%
#   summarise(mean_resid = mean(resid),
#             mean_est_anpp = mean(mean_estimate)) %>%
#   ungroup()
# 
# ggplot(resids, aes(x=mean_est_anpp, y=mean_resid))+
#   geom_hline(aes(yintercept=0))+
#   geom_point()
