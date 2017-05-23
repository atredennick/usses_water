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
data_path <- "../data/estimated_biomass/"
anpp_fname <- "permanent_plots_estimated_biomass.RDS"
permanent_quad_biomass <- readRDS(paste0(data_path,anpp_fname))

weather <- read.csv("../data/weather/ClimateIPM.csv") %>%
  select(-pptLag,-ppt2,-TmeanSpr1,-TmeanSpr2)

anpp_data <- permanent_quad_biomass %>% 
  filter(Treatment %in% c("Control","Drought","Irrigation")) %>%
  filter(!str_detect(quadname, 'P1|P7')) %>%
  filter(year > 2011) %>%
  rename(anpp = biomass_grams_est) %>%
  left_join(weather, by = "year") %>%
  select(-QuadName,-quad,-Grazing,-paddock,-ndvi) %>%
  mutate(ppt1_scaled = as.numeric(scale(ppt1)),
         year_id = year - 2011)

drought_data <- anpp_data %>%
  filter(Treatment != "Irrigation") %>%
  mutate(trt_id = as.numeric(as.factor(Treatment)) - 1)

irrigate_data <- anpp_data %>%
  filter(Treatment != "Drought") %>%
  mutate(trt_id = as.numeric(as.factor(Treatment)) - 1)


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



####
####  CALCULATE MAX(Pr(effect) > 0, Pr(effect) < 0) ----
####
get_one_tailed <- function(values){
  above <- 1 - ecdf(values)(0)
  below <- ecdf(values)(0)
  max(above,below)
}

lmod <- lm(log(anpp) ~ year_id*trt_id + ppt1_scaled, drought_data)
x <- model.matrix(lmod)
drought_ref <- reshape2::melt(rstan::extract(drought_fit, pars="beta"))
colnames(drought_ref)[2:3] <- c("parameter","loganpp")
drought_ref$parameter <- factor(colnames(x)[drought_ref$parameter])
drought_probs <- {}
for(doparam in unique(drought_ref$parameter)){
  tmp <- filter(drought_ref, parameter == doparam)
  tmp_prob <- get_one_tailed(tmp$loganpp)
  out_probs <- data.frame(coefficient = doparam,
                          prob = tmp_prob,
                          treatment = "drought")
  drought_probs <- rbind(drought_probs, out_probs)
}

lmod <- lm(log(anpp) ~ year_id*trt_id + ppt1_scaled, irrigate_data)
x <- model.matrix(lmod)
irrigate_ref <- reshape2::melt(rstan::extract(irrigate_fit, pars="beta"))
colnames(irrigate_ref)[2:3] <- c("parameter","loganpp")
irrigate_ref$parameter <- factor(colnames(x)[irrigate_ref$parameter])
irrigate_probs <- {}
for(doparam in unique(irrigate_ref$parameter)){
  tmp <- filter(irrigate_ref, parameter == doparam)
  tmp_prob <- get_one_tailed(tmp$loganpp)
  out_probs <- data.frame(coefficient = doparam,
                          prob = tmp_prob,
                          treatment = "irrigate")
  irrigate_probs <- rbind(irrigate_probs, out_probs)
}

coef_probs <- (rbind(drought_probs, irrigate_probs)) %>%
  filter(coefficient != "(Intercept)") %>%
  mutate(coefficient = as.character(coefficient)) %>%
  spread(treatment, prob)
coef_probs$coefficient <- c("Precipitation","Treatment", "Year", "Treatment x Year")
colnames(coef_probs) <- c("log(ANPP) Coefficient", "Drought", "Irrigation")

##  Save coefficient probabilities for LaTeX table in manuscript
saveRDS(coef_probs, "../results/coef_probabilities.RDS")



####
####  EXTRA PLOTS ----
####
# ref <- reshape2::melt(rstan::extract(fit, pars="beta"))
# colnames(ref)[2:3] <- c("parameter","loganpp")
# ref$parameter <- factor(colnames(x)[ref$parameter])
# ggplot(ref, aes(x=loganpp))+geom_line(stat="density")+geom_vline(xintercept=0)+facet_wrap(~parameter,scales="free")

