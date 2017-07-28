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

library(lme4)
anpp_data$x <- anpp_data$vwc_scaled
anpp_data$y <- as.numeric(scale(log(anpp_data$anpp)))

m1 <- lmer(data = anpp_data, y ~ Treatment*x + (1|year) + (x|quadname) )
model.matrix(m1)

summary(m1)


####
####  SET UP AND FIT MODEL IN STAN ----
####
fit_stan_model <- function(model_data, model_file, check_diags=FALSE, treattype){
  lmod <- lm(log(anpp) ~ Treatment*vwc_scaled, model_data)
  x <- model.matrix(lmod)
  newx <- unique(x)
  
  lmod <- lm(log(anpp) ~ vwc_scaled, model_data)
  z <- model.matrix(lmod)
  
  anppdat <- list(Nobs = nrow(model_data),
                  Npreds = ncol(x),
                  Npreds2 = ncol(z),
                  Nplots = length(unique(model_data$quadname)),
                  Ntreats = length(unique(model_data$Treatment)),
                  Nppts = nrow(newx),
                  Nyears = length(unique(model_data$year)),
                  y = as.numeric(scale(log(model_data$anpp))),
                  x = x,
                  z = z,
                  newx = newx,
                  plot_id = as.numeric(as.factor(model_data$quadname)),
                  treat_id = as.numeric(as.factor(as.character(model_data$Treatment))),
                  year_id = as.numeric(as.factor(model_data$year)),
                  R = diag(1,ncol(z)))
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  rt <- stanc(model_file)
  sm <- stan_model(stanc_ret = rt, verbose=FALSE)
  set.seed(123)
  fit <- sampling(sm, data = anppdat, iter = 10000, chains = 4, thin = 10)


  # if(check_diags){
  #   pnames <- c("sigmaeps", "sigmaint", "sigmaslope","beta[1]",
  #               "beta[2]","beta[3]","beta[4]","beta[5]")
  #   for(p in pnames){
  #     muc <- rstan::extract(fit, pars=p,  permuted=FALSE, inc_warmup=FALSE)
  #     mdf <- reshape2::melt(muc)
  #     dir.create("./scratch/")
  #     ggplot(mdf,aes(x=iterations,y=value,color=chains)) + 
  #       geom_line() + 
  #       ylab(mdf$parameters[1])
  #     ggsave(paste0("./scratch/diag_",treattype,"_",p,".png"), 
  #            height = 4, width = 5, units = "in", dpi=72)
  #   }
  # }
  return(fit)
}


all_fit <- fit_stan_model(anpp_data, model_file = "anpp_randcoefs.stan", 
                          check_diags = FALSE, treattype = "all")



stan_diag(fit)

library(bayesplot)
draws <- as.array(all_fit, pars = 'Sigma_u')
mcmc_trace(draws)


summary(all_fit, pars = 'beta')$summary
fixef(m1)

summary(all_fit, pars = 'L_u')$summary
summary(all_fit, pars = 'sigma_u')
summary(all_fit, pars = 'Sigma_u')$summary

saveRDS(all_fit, "../results/randcoefs_alltreatments_fit.RDS")

