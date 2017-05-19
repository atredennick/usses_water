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
library(ggthemes)     # Pleasing ggplot themes
library(stringr)      # Working with strings
library(rjags)        # For MCMC
library(coda)         # For collecting/summarizing MCMC output



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
####  WRITE JAGS MODEL FOR ANPP MODEL ----
####
model_string <- "
model{
  ### PRIOR MODELS
  intercept ~ dnorm(0,0.001)
  Bppt ~ dnorm(0,0.001)
  Btrt ~ dnorm(0,0.001)
  Byr ~ dnorm(0,0.001)
  # Bppttrt ~ dnorm(0,0.001)
  Btrtyr ~ dnorm(0,0.001)
  sigma ~ dgamma(0.001,0.001)
  
  ### PROCESS MODEL
  for(i in 1:nobs){
    mu[i] = intercept + Bppt*xrain[i] + Btrt*xtrt[i] + Byr*xyr[i] + Btrtyr*xtrt[i]*xyr[i]
  }
  
  ### LIKELIHOOD MODEL
  for(i in 1:nobs){
    y[i] ~ dnorm(mu[i], sigma)
  }
}
"



####
####  FIT MODELS ----
####
fit_model <- function(df,rain_var="ppt1_scaled",iters=1000,chains=3,thin=1){
  jags_data <- list(xrain = df[,rain_var],
                    xtrt = df[,"trt_id"],
                    xyr = df[,"year_id"],
                    y = log(df[,"anpp"]),
                    nobs = nrow(df))
  vars_to_track <- c("intercept","Bppt","Btrt","Byr","Btrtyr")
  jm <- jags.model(textConnection(model_string),
                   data = jags_data,
                   n.chains = chains,
                   n.adapt = 1000)
  update(jm, n.iter = iters)
  out <- coda.samples(jm, 
                      variable.names = vars_to_track,
                      n.iter = iters, 
                      n.thin = thin)
  out_stat <- summary(out,quantiles=c(0.025,0.5,0.975))
  out_stat <- cbind(out_stat$statistics,out_stat$quantiles)
  if(chains > 1){
    rhat <- gelman.diag(out,multivariate=F,autoburnin=F)
    if(sum(rhat$psrf[,1]>1.1)>0) stop("check convergence")
  }
  ret_list <- list(out_mcmc = out,
                   out_stat = out_stat)
}

drought_model <- fit_model(drought_data,chains=3,iters=10000,thin=10)
irrigate_model <- fit_model(irrigate_data,chains=3,iters=10000,thin=10)



####
####  PLOT PARAMETER ESTIMATES ----
####
drought_params <- as.data.frame(drought_model[[2]])
drought_params$effect <- rownames(drought_params)
colnames(drought_params) <- c("mean","sdev","serr","ts_serr","lowerquant","median","upperquant","effect")
drought_params$treatment <- "Drought"

irrigate_params <- as.data.frame(irrigate_model[[2]])
irrigate_params$effect <- rownames(irrigate_params)
colnames(irrigate_params) <- c("mean","sdev","serr","ts_serr","lowerquant","median","upperquant","effect")
irrigate_params$treatment <- "Irrigation"

all_params <- rbind(drought_params,irrigate_params)
all_params <- filter(all_params, effect!="intercept")
tick_labels <- rev(c("Year","Treatment x Year","Treatment","Precip."))

ggplot(all_params, aes(x=effect,y=median))+
  geom_hline(aes(yintercept=0),color="grey35",size=0.2)+
  geom_errorbar(aes(ymin=lowerquant,ymax=upperquant), width=0.1, size=0.3)+
  geom_point(size=2,color="white")+
  geom_point(size=1)+
  scale_x_discrete(labels=tick_labels)+
  coord_flip()+
  ylab("Posterior Estimate")+
  xlab("")+
  facet_wrap(~treatment)+
  theme_few()+
  theme(axis.title = element_text(size=9),
        axis.text = element_text(size=7))
ggsave("../figures/anpp_posterior_quants.png",width = 4, height = 2, units = "in", dpi = 120)

ggplot(anpp_data, aes(x=ppt1,y=anpp,color=Treatment))+
  geom_jitter(size = 2,width = 2, alpha=0.5, shape=21, color="grey35",aes(fill=Treatment))+
  stat_smooth(method = "lm", se=F)+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  theme_few()
