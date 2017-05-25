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
  lmod <- lm(log(anpp) ~ year_id*trt_id + ppt1_scaled, model_data)
  x <- model.matrix(lmod)
  anppdat <- list(Nobs = nrow(model_data),
                  Npreds = ncol(x),
                  Ngroups = length(unique(model_data$quadname)),
                  y = as.numeric(scale(log(model_data$anpp))),
                  x = x,
                  timevar = model_data$year_id,
                  group = as.numeric(as.factor(model_data$quadname)))
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  rt <- stanc("anpp_longitudinal.stan")
  sm <- stan_model(stanc_ret = rt, verbose=FALSE)
  set.seed(123)
  fit <- sampling(sm, data=anppdat, iter=10000, chains=4, thin=10)
  
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
saveRDS(drought_fit, "../results/repmeas_drought_fit.RDS")
saveRDS(irrigate_fit, "../results/repmeas_irrigate_fit.RDS")



####
####  PLOT RESIDUALS FOR EVIDENCE OF AUTOCORRELATION STUCTURE ----
####
# drought_errors <- reshape2::melt(rstan::extract(drought_fit, pars="resids"))
# drought_errors_avg <- drought_errors %>%
#   group_by(Var2) %>%
#   summarise(mean_resid = mean(value))
# drought_errors_avg <- cbind(drought_errors_avg, drought_data)
# 
# allquads <- unique(drought_errors_avg$quadname)
# for(doquad in allquads){
#   tmperror <- filter(drought_errors_avg, quadname == doquad)
#   tmperrorlag <- tmperror
#   tmperrorlag$year_id <- tmperrorlag$year_id+1
#   tmperrorlag$lag_resid <- tmperrorlag$mean_resid
#   tmperrorlag <- tmperrorlag %>%
#     select(year_id,lag_resid)
#   tmperror <- left_join(tmperror, tmperrorlag)
#   plot(mean_resid~lag_resid,data=tmperror, main=doquad)
# }


