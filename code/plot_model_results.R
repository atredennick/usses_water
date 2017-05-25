##  Script to plot results from statistical model.
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
library(gridExtra)



####
####  LOAD DATA AND MODEL RESULTS ----
####
source("read_format_data.R") # load data
drought_fit <- readRDS("../results/repmeas_drought_fit.RDS")
irrigate_fit <- readRDS("../results/repmeas_irrigate_fit.RDS")



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
####  PLOT POSTERIOR MEDIANS AND CIS ----
####
lmod <- lm(log(anpp) ~ year_id*trt_id + ppt1_scaled, drought_data)
x <- model.matrix(lmod)
beta_names <- colnames(x)

drought_summary <- as.data.frame(summary(drought_fit, pars = c("beta"), probs = c(0.025,0.1,0.5,0.9,0.975))$summary)
irrigate_summary <- as.data.frame(summary(irrigate_fit, pars = c("beta"), probs = c(0.025,0.1,0.5,0.9,0.975))$summary)
drought_summary$effect <- beta_names
drought_summary$treatment <- "Drought"
irrigate_summary$effect <- beta_names
irrigate_summary$treatment <- "Irrigation"
model_summary <- rbind(drought_summary, irrigate_summary) %>%
  filter(effect != "(Intercept)")

model_plot <- ggplot(model_summary, aes(y=`50%`, x=effect))+
  geom_hline(aes(yintercept=0), linetype=2)+
  geom_errorbar(aes(ymin = `2.5%`, ymax=`97.5%`), width=0, color="grey45")+
  geom_errorbar(aes(ymin=`10%`, ymax=`90%`), size=1.2, width=0)+
  geom_point(size=2.1,shape=21,fill="white")+
  facet_wrap(~treatment)+
  coord_flip()+
  xlab(NULL)+
  ylab("Posterior Estimate")+
  ggtitle("B")+
  scale_x_discrete(labels = rev(c("Year x Treatment", "Year", "Treatment", "Precipitation")))+
  theme_few()



####
####  PLOT DATA ----
####
data_plot <- ggplot(anpp_data, aes(x=ppt1,y=anpp))+
  geom_point(shape=21,color="grey25",alpha=0.8,aes(fill=Treatment))+
  stat_smooth(method="lm", aes(color=Treatment), se=FALSE, size=0.7)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  scale_x_continuous(limits=c(100,300),breaks=seq(100,300,50))+
  scale_y_continuous(limits=c(40,360),breaks=seq(50,350,50))+
  xlab("Growing Season Precipitation")+
  ylab(expression(paste("ANPP (g ",m^2,")")))+
  ggtitle("A")+
  theme_few()+
  theme(legend.position = c(0.2,0.8),
        legend.key.size = unit(1,"pt"),
        legend.title = element_text(size=10),
        legend.text = element_text(size = 8),
        legend.key.height = unit(0.8,"line"))

####
####  COMBINE PLOTS AND SAVE ----
####
lay <- rbind(c(1,1,2,2,2))
combo_plot <- grid.arrange(data_plot, model_plot, layout_matrix = lay)
ggsave("../figures/data_and_posterior_estimates.png", plot = combo_plot, height = 3, width = 8, units = "in", dpi=120)



