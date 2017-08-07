##  anpp_randcoefs_model.R: Script to run GLMM analysis to test for treatment 
##  effects on the relationship between precipitation and ANPP.
##
##  NOTE: Stan may issue a couple warnings after running the MCMC that, as
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
library(lme4)         # Mixed-effects modeling



####
####  READ IN AND EXTRACT EXPERIMENT ANPP DATA ----
####
source("read_format_data.R")



####
####  SET UP AND FIT MODEL IN STAN ----
####
##  Set up fixed and random design matrices
lmod <- lm(log(anpp) ~ Treatment*vwc_scaled, anpp_data)
x    <- model.matrix(lmod)
newx <- unique(x)

lmod <- lm(log(anpp) ~ vwc_scaled, anpp_data)
z    <- model.matrix(lmod)

##  Make data list for Stan
anppdat <- list(
  Nobs     = nrow(anpp_data),
  Npreds   = ncol(x),
  Npreds2  = ncol(z),
  Nplots   = length(unique(anpp_data$quadname)),
  Ntreats  = length(unique(anpp_data$Treatment)),
  Nppts    = nrow(newx),
  Nyears   = length(unique(anpp_data$year)),
  y        = as.numeric(scale(log(anpp_data$anpp))),
  x        = x,
  z        = z,
  newx     = newx,
  plot_id  = as.numeric(as.factor(anpp_data$quadname)),
  treat_id = as.numeric(as.factor(as.character(anpp_data$Treatment))),
  year_id  = as.numeric(as.factor(anpp_data$year)),
  R        = diag(1,ncol(z))
  ) # close list

##  Fit the model in Stan
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(123)

rt  <- stanc("anpp_randcoefs.stan")
sm  <- stan_model(stanc_ret = rt, verbose=FALSE)
fit <- sampling(sm, data = anppdat, iter = 10000, chains = 4, thin = 10)

##  Save the model fit
saveRDS(fit, "../results/randcoefs_alltreatments_fit.RDS")



####
####  OPTIONAL DIAGNOSTICS ----
####
# library(bayesplot)
# stan_diag(fit)
# draws <- as.array(all_fit, pars = 'Sigma_u')
# mcmc_trace(draws)
# 
# summary(all_fit, pars = 'beta')$summary
# fixef(m1)
# 
# summary(all_fit, pars = 'L_u')$summary
# summary(all_fit, pars = 'sigma_u')
# summary(all_fit, pars = 'Sigma_u')$summary
# 
