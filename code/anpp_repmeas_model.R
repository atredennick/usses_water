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



####
####  PLOT ANPP DATA AS LAGS TO LOOK FOR CORRELATION ----
####
anpp_wide <- anpp_data %>%
  select(-year_id, -ppt1, -ppt1_scaled) %>%
  mutate(year = paste0("year",year)) %>%
  spread(key = year, value = anpp)

par(mfrow=c(2,2))
plot(year2013 ~ year2012, data=anpp_wide)
title(round(cor(anpp_wide$year2012,anpp_wide$year2013),2))
plot(year2014 ~ year2013, data=anpp_wide)
title(round(cor(anpp_wide$year2014,anpp_wide$year2013),2))
plot(year2015 ~ year2014, data=anpp_wide)
title(round(cor(anpp_wide$year2014,anpp_wide$year2015),2))
plot(year2016 ~ year2015, data=anpp_wide)
title(round(cor(anpp_wide$year2015,anpp_wide$year2016),2))




####
####  FIT LONGITUDINAL MODEL WITH AR(1) ERROR COVARIANCE ----
####
fit_error_model <- function(model_data, check_diags=FALSE, treattype){
  lmod <- lm(log(anpp) ~ year_id*trt_id + ppt1_scaled, model_data)
  x <- model.matrix(lmod)
  # x <- x[,2:ncol(x)] # exclude intercept term
  Nplots <- length(unique(model_data$quadname))
  Ntimes <- length(unique(model_data$year_id))
  y_matrix <- matrix(data = NA, ncol=Ntimes, nrow=Nplots)
  x_array <- array(data = NA, dim = c(Nplots,Ntimes,ncol(x)))
  plot_names <- unique(model_data$quadname)
  for(j in 1:Nplots){
    plot_rows <- which(model_data$quadname == plot_names[j])
    y_matrix[j,] <- as.numeric(scale(log(model_data[plot_rows,"anpp"])))
    x_array[j,,] <- x[plot_rows,]
  }
  
  anppdat <- list(Npreds = ncol(x),
                  Nplots = Nplots,
                  Ntimes = Ntimes,
                  y = y_matrix,
                  x = x_array)
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  rt <- stanc("anpp_longitudinal_errorcov.stan")
  sm <- stan_model(stanc_ret = rt, verbose=FALSE)
  set.seed(123)
  fit <- sampling(sm, data=anppdat, iter=5000, chains=4, thin=2, control = list(adapt_delta = 0.99))
  plot(fit, pars="betas")
  
  if(check_diags){
    pnames <- c("beta[1]","beta[2]","beta[3]","beta[4]","beta[5]")
    for(p in pnames){
      muc <- rstan::extract(fit, pars="betas[3]",  permuted=FALSE, inc_warmup=FALSE)
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

