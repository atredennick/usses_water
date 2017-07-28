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
library(ggjoy)        # Joy plots!
library(stringr)      # Working with strings
library(rstan)        # For MCMC and Stan objects
library(gridExtra)    # For combining ggplot objects
library(viridis)      # Pleasing color palette



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
param_id_names <- data.frame(param_id = c(1,2),
                             param_name = c("Intercept", "Soil Moisture"))
treat_id_names <- data.frame(treat_id = c(1,2,3),
                             treatment = c("Control","Drought","Irrigation"))
betas <- reshape2::melt(rstan::extract(all_fit, pars="beta_treat")) %>%
  rename(iteration = iterations, treat_id = Var2, param_id = Var3, estimate = value, stan_name = L1) %>%
  left_join(param_id_names, by="param_id") %>%
  left_join(treat_id_names, by="treat_id")

g1 <- ggplot(betas, aes(x=estimate))+
  geom_hline(aes(yintercept=0), color="grey45", size=0.1)+
  geom_line(stat="density",aes(color=treatment),adjust=5,size=1)+
  scale_color_brewer(palette = "Set2",name="Treatment")+
  #scale_x_continuous(breaks=seq(-1.5,2,0.5))+
  #scale_y_continuous(breaks=seq(0,2,0.25))+
  facet_wrap(~param_name)+
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
#ggsave("../figures/glmm_treatment_posteriors.png",plot = g1, width = 6, height = 3, units = "in", dpi =120)



####
####  PLOT MODEL PREDICTIONS WITH DATA ----
####
lmod <- lm(log(anpp) ~ total_seasonal_vwc, anpp_data)
x <- model.matrix(lmod)
newx <- as.data.frame(unique(x)) %>% select(total_seasonal_vwc)
newx$vwc_id <- c(1:nrow(newx))
mean_log_anpp <- mean(log(anpp_data$anpp))
sd_log_anpp <- sd(log(anpp_data$anpp))
model_preds <- reshape2::melt(rstan::extract(all_fit, pars="ypreds_mu")) %>%
  rename(iteration = iterations, vwc_id = Var2, estimate = value, stan_name = L1) %>%
  left_join(newx, by="vwc_id") %>%
  group_by(total_seasonal_vwc) %>%
  summarise(mean_estimate = mean(estimate*sd_log_anpp+mean_log_anpp)) %>%
  mutate(backtrans_estimate = mean_estimate) %>%
  ungroup()

trt_vwc <- data.frame(treatment = rep(c("Control","Drought","Irrigation"), each = nrow(newx)),
                      treat_id = rep(c(1:3), each = nrow(newx)),
                      vwc = rep(newx$total_seasonal_vwc, times = 3),
                      vwc_id = rep(newx$vwc_id, times = 3))
treat_preds <- reshape2::melt(rstan::extract(all_fit, pars="ypreds")) %>%
  rename(iteration = iterations, treat_id = Var2, vwc_id = Var3, estimate = value, stan_name = L1) %>%
  left_join(trt_vwc, by = c("vwc_id","treat_id")) %>%
  group_by(treatment, vwc) %>%
  summarise(mean_estimate = mean(estimate*sd_log_anpp+mean_log_anpp)) %>%
  mutate(backtrans_estimate = mean_estimate) %>%
  ungroup()

 
g2 <- ggplot(anpp_data, aes(x=total_seasonal_vwc,y=log(anpp)))+
  geom_point(shape=21,color="grey25",alpha=0.8,aes(fill=Treatment))+
  geom_line(data=treat_preds, aes(x=vwc, y=backtrans_estimate, color=treatment))+
  geom_line(data=model_preds, aes(x=total_seasonal_vwc, y=backtrans_estimate), size=1)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  #scale_x_continuous(limits=c(100,300),breaks=seq(100,300,50))+
  #scale_y_continuous(breaks=seq(50,350,50))+
  xlab("March-June Cumulative VWC")+
  ylab(expression(paste("log[ANPP (g ",m^2,")]")))+
  guides(fill=FALSE,color=FALSE)+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  ggtitle("B)")

mylayout <- c(1,1,2)
gout <- grid.arrange(grobs=list(g1,g2),layout.matrix=mylayout)
ggsave("../figures/glmm_main_results.png",plot = gout, width = 5.5, height = 6, units = "in", dpi =120)



# ####
# ####  DATA AND REGRESSION PLOT ON ARITHMETIC SCALE FOR SI ----
# ####
# ggplot(anpp_data, aes(x=total_seasonal_vwc,y=anpp))+
#   geom_point(shape=21,color="grey25",alpha=0.8,aes(fill=Treatment))+
#   geom_line(data=treat_preds, aes(x=vwc, y=exp(backtrans_estimate), color=treatment))+
#   geom_line(data=model_preds, aes(x=total_seasonal_vwc, y=exp(backtrans_estimate)), size=1)+
#   scale_fill_brewer(palette = "Set2")+
#   scale_color_brewer(palette = "Set2")+
#   #scale_x_continuous(limits=c(100,300),breaks=seq(100,300,50))+
#   #scale_y_continuous(breaks=seq(50,350,50))+
#   xlab("March-June Cumulative VWC")+
#   ylab(expression(paste("ANPP (g ",m^2,")")))+
#   guides(fill=FALSE,color=FALSE)+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank())
# ggsave("../figures/glmm_main_results_arithmetic.png", width = 4, height = 3, units = "in", dpi =120)



####
####  PLOT TREATMENT DIFFERENCES BY YEAR POSTERIORS ----
####
param_id_names <- data.frame(param_id = c(1:3),
                             param_name = c("Control", "Drought", "Irrigation"))
year_id_names <- data.frame(year_id = c(1:5),
                            year_name = c(1:5))
betas <- reshape2::melt(rstan::extract(all_fit, pars="year_off")) %>%
  rename(iteration = iterations, year_id = Var2, estimate = value, stan_name = L1) %>%
  left_join(year_id_names, by="year_id")

ggplot(betas, aes(x=estimate, y=as.factor(year_id), fill=as.factor(year_id), color=as.factor(year_id), height = ..density..))+
  geom_vline(aes(xintercept=0), linetype=2, color="grey45")+
  geom_joy(stat="density", adjust=3, alpha=0.9)+
  scale_x_continuous(breaks=seq(-3,3,0.5), limits=c(-3,3))+
  scale_y_discrete(labels = c("2012","2013","2014","2015","2016"))+
  scale_fill_viridis(end=0.8,discrete=T)+
  scale_color_viridis(end=0.8,discrete=T)+
  guides(fill=FALSE, color=FALSE)+
  xlab("Parameter Value")+
  ylab("Year")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.border     = element_blank(), 
        axis.line        = element_blank(),
        axis.ticks       = element_blank())
ggsave("../figures/glmm_yeardiffs.png", width = 6, height = 3, units = "in", dpi =120)


# g2 <- ggplot(ydiffs, aes(y=`50%`, x=pptyear, color=Treatment))+
#   geom_hline(aes(yintercept=0), linetype=2, size=0.2)+
#   geom_errorbar(aes(ymin = `2.5%`, ymax=`97.5%`), width=0, position = dodge)+
#   geom_errorbar(aes(ymin=`10%`, ymax=`90%`), size=1.2, width=0, position = dodge)+
#   geom_point(size=2.5,shape=21,fill="white", position = dodge)+
#   scale_color_manual(values = RColorBrewer::brewer.pal(3,name = "Set2")[2:3])+
#   xlab("Year of Experiment")+
#   ylab("Difference Between\nControl and Treatment")+
#   theme_few()+
#   theme(legend.position = c(0.15,0.85),
#         legend.key.size = unit(6,"pt"),
#         legend.title = element_text(size=10),
#         legend.text = element_text(size = 8),
#         legend.key.height = unit(0.8,"line"))
# ggsave("../figures/glmm_yeardiffs.png",plot = g2, width = 4, height = 3.5, units = "in", dpi = 120)



####
####  EXTRACT AND SAVE ALL COEFFICIENT SUMMARIES ----
####
all_betas <- as.data.frame(summary(all_fit, pars = c("beta_treat","beta_mu"), probs = c(0.025,0.1,0.5,0.9,0.975))$summary)
write.csv(all_betas, "../results/beta_summaries.csv")



####
####  CALCULATE PROBABILITY OF DIFFERENCE BASED ON INTERCEPTS ----
####
intercept_diffs <- reshape2::melt(rstan::extract(all_fit, pars="inter_diffs")) %>%
  rename(iteration = iterations, treat_id = Var2, estimate = value, stan_name = L1)
Pr_control_above_drought <- 1 - ecdf(filter(intercept_diffs,treat_id == 1)[,"estimate"])(0)
Pr_control_below_irrigation <- ecdf(filter(intercept_diffs,treat_id == 2)[,"estimate"])(0)
mean_pptyear_probs <- data.frame(Pr_control_above_drought,Pr_control_below_irrigation)
write.csv(mean_pptyear_probs, "../results/mean_pptyear_probs.csv")
