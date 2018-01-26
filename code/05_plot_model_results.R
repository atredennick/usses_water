##  Script to plot results from the GLMM Stan object.
##
##  Author: Andrew Tredennick
##  Date created: May 25, 2017

##  Clear everything
rm(list=ls(all.names = TRUE))

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
library(RColorBrewer) # More color palettes
library(cowplot)      # For combining ggplots



####
####  LOAD DATA AND MODEL RESULTS ----
####
source("read_format_data.R") # load data
all_fit <- readRDS("../results/randcoefs_alltreatments_fit.RDS")



####
####  FUNCTION TO CALCULATE MAX(Pr(effect) > 0, Pr(effect) < 0) ----
####
get_one_tailed <- function(values){
  above <- 1 - ecdf(values)(0)
  below <- ecdf(values)(0)
  max(above,below)
}



####
####  PLOT TREATMENT-LEVEL POSTERIOR DISTRIBUTIONS ----
####
mycols <- c("#009E73", "#D55E00", "#0072B2")
param_labels <- c("Control::Intercept",
                  "Drought::Intercept", 
                  "Irrigation::Intercept",
                  "Control::Slope", 
                  "Drought::Slope", 
                  "Irrigation::Slope")

betas <- data.frame(extract(all_fit, pars = 'beta')) %>% 
  mutate(iteration = row_number()) %>% 
  gather(param_name, estimate, starts_with('beta')) %>%
  mutate(param_name = factor(param_name, labels = param_labels)) %>%
  separate(param_name, c("Treatment", "Type"), "::")

treat_slopes <- betas %>%
  filter(Treatment != "Control") %>%
  mutate(Type = paste(Type,"Offset"))

slope_probs <- treat_slopes %>%
  group_by(Treatment, Type) %>%
  summarise(probs = round(get_one_tailed(estimate),2)) %>%
  mutate(prob_text = paste("Pr > 0 =", probs))
slope_probs$prob_text[3] <- paste("Pr < 0 =", slope_probs$probs[3]) # switch direction of irrigation intercpet since less than 0
slope_probs$prob_text[4] <- paste("Pr < 0 =", 1-slope_probs$probs[4]) # switch direction of irrigation slope to match hypothesis
slope_probs$xpos <- c(-4.5, -0.32, -4.5, -0.27)
slope_probs$ypos <- c(1.3,1.3,2.55,2.7)

treat_cols <- mycols[2:3]

treat_posteriors <- ggplot(treat_slopes)+
  geom_vline(aes(xintercept=0), linetype=2, color="grey45")+
  geom_joy(stat="density", adjust=3, alpha=0.8, aes(x=estimate, y=Treatment, fill=Treatment, color=Treatment, height = ..density..))+
  geom_text(data = slope_probs, aes(x=xpos, y=ypos, label = prob_text), size = 3)+
  facet_wrap(~Type, scales = "free")+
  scale_y_discrete(labels = c("Drought","Irrigation"))+
  scale_fill_manual(values = treat_cols)+
  scale_color_manual(values = treat_cols)+
  guides(fill=FALSE, color=FALSE)+
  xlab("Coefficient Value")+
  ylab(NULL)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.border     = element_blank(), 
        axis.line        = element_blank(),
        axis.ticks       = element_blank(),
        strip.background = element_blank(),
        strip.text       = element_text(face="bold"),
        panel.spacing    = unit(1, units = "cm"))



####
####  PLOT MEAN REGRESSION LINES FOR EACH TREATMENT ----
####
treat_effects <- data.frame(extract(all_fit, pars = 'beta')) %>% 
  mutate(iteration = row_number()) %>% 
  gather(param_name, estimate, starts_with('beta')) %>%
  mutate(param_name = factor(param_name, labels = param_labels)) %>% 
  spread(param_name, estimate) %>% 
  mutate(`Drought::Intercept` = `Drought::Intercept` + `Control::Intercept`, 
         `Irrigation::Intercept` = `Irrigation::Intercept` + `Control::Intercept`,  
         `Drought::Slope` = `Drought::Slope` + `Control::Slope`, 
         `Irrigation::Slope` = `Irrigation::Slope` + `Control::Slope`) %>%
  gather(param_name, estimate, -iteration) 

effect_summary <- treat_effects %>%
  group_by(param_name) %>%
  summarise(mean_estimate = round(mean(estimate), 2),
            median_estimate = round(median(estimate), 2),
            lower_95_ci = round(quantile(estimate, probs = c(0.025)), 2),
            upper_95_ci = round(quantile(estimate, probs = c(0.975)), 2)) %>%
  separate(param_name, into = c("Treatment", "Coefficient"), sep = "::") %>%
  arrange(Coefficient, Treatment) %>%
  select(Coefficient, everything())
saveRDS(effect_summary, "../results/effect_summary.RDS")

mean_effects <- treat_effects %>%
  group_by(param_name) %>%
  summarise(mean_estimate = mean(estimate))

regression <- mean_effects %>%
  separate(param_name, into = c("Treatment","Coefficient"), sep = "::") %>%
  spread(Coefficient, mean_estimate)

lmod <- lm(log(anpp) ~ vwc_scaled, anpp_data)
x    <- model.matrix(lmod) 
newx <- data.frame(x = seq(min(x[,2]), max(x[,2]), length.out = 50)) %>%
  mutate(id = paste("x",1:50,sep = "::")) %>%
  spread(id, x)
newx <- rbind(rbind(newx,newx),newx)
newx$Treatment <- regression$Treatment

regression <- regression %>%
  left_join(newx, by="Treatment") %>%
  gather(xid, newx, -Treatment, -Intercept, -Slope)
regression$estimate <- with(regression, Intercept+Slope*newx)

mean_log_anpp <- mean(log(anpp_data$anpp))
sd_log_anpp <- sd(log(anpp_data$anpp))
mean_vwc <- mean(anpp_data$total_seasonal_vwc)
sd_vwc <- sd(anpp_data$total_seasonal_vwc)

regression <- regression %>%
  mutate(backtrans_estimate = estimate*sd_log_anpp+mean_log_anpp,
         backtrans_vwc = newx*sd_vwc+mean_vwc)

regression_limited <- {}
for(dotrt in unique(regression$Treatment)){
  tmpdat <- filter(regression, Treatment == dotrt)
  tmprange <- range(anpp_data[which(anpp_data$Treatment==dotrt),"total_seasonal_vwc"])
  tmpdat$backtrans_vwc[tmpdat$backtrans_vwc < tmprange[1] | tmpdat$backtrans_vwc > tmprange[2]] <- NA
  regression_limited <- rbind(regression_limited, tmpdat)
}

suppressWarnings( # ignore wanrnings about NA values
  regress_plot <- ggplot(anpp_data, aes(x=total_seasonal_vwc,y=log(anpp)))+
    geom_point(shape=21,color="grey25",alpha=0.8,aes(fill=Treatment))+
    geom_line(data=regression_limited, aes(x=backtrans_vwc, y=backtrans_estimate, color=Treatment), size=1)+
    scale_fill_manual(values = mycols, name = NULL)+
    scale_color_manual(values = mycols, name = NULL)+
    xlab("March-June Cumulative VWC")+
    ylab(expression(paste("log[ANPP (g ",m^2,")]")))+
    theme_bw()+
    theme(panel.grid.minor = element_blank(),
          legend.position = c(0.12, 0.85),
          legend.background = element_rect(colour = NA, fill = NA),
          legend.text=element_text(size=7),
          legend.key.size = unit(0.4, "cm"))
)


####
####  SENSITIVITIES ------------------------------------------------------------
####
all_diffs <- readRDS("../results/sensitvities.RDS")
sens_plot <- ggplot(all_diffs, aes(x = year, y = sensitivity, fill = treatment))+
    geom_jitter(shape = 21, width = 0.1, color = "grey25", alpha=0.8)+
    stat_smooth(data = filter(all_diffs, treatment == "drought"), color = treat_cols[1], se=F, method="lm")+
    scale_fill_manual(values = treat_cols, name = NULL, labels = c("Drought","Irrigation"))+
    scale_color_manual(values = treat_cols, name = NULL, labels = c("Drought","Irrigation"))+
    ylab("Sensitivity")+
    xlab("Year")+
    guides(color = FALSE, fill = FALSE)+
    theme_bw()+
    theme(panel.grid.minor = element_blank(),
          legend.position = c(0.12, 0.9),
          legend.background = element_rect(colour = NA, fill = NA))



####
####  COMBINE PLOTS AND SAVE ---------------------------------------------------
####
##  Version without sensitivity analysis
# suppressWarnings( # ignore warnings about NA values
#   gridplot <- cowplot::plot_grid(treat_posteriors, NULL, regress_plot, sens_plot, 
#                                  rel_heights = c(1, 0.08, 1),
#                                  nrow = 3,
#                                  ncol = 2,
#                                  labels = c("A)","","B)","C)"))
# )

##  Version with sensitivity analysis
suppressWarnings( # ignore warnings about NA values
  bottom_row <- plot_grid(regress_plot, NULL, sens_plot, 
                          labels = c('C','', 'D'),
                          align = 'h', 
                          rel_widths = c(1, 0.1, 1), 
                          nrow = 1)
)

suppressWarnings( # ignore warnings about NA values
  gridplot <- plot_grid(treat_posteriors, NULL, bottom_row, 
                        ncol = 1, 
                        nrow = 3, 
                        labels = c("A"), 
                        rel_heights = c(1, 0.1, 1))
)
ggsave("../figures/glmm_main_results.png", plot = gridplot, width = 7, height = 5, units = "in", dpi =120)
ggsave("../figures/Figure3.pdf", plot = gridplot, width = 7, height = 5, units = "in")



####
####  PLOT PRIORS AND POSTERIORS (now in App. 2) ----
####
# posteriors <- treat_effects %>%
#   separate(param_name, into = c("Treatment", "Coefficient")) %>%
#   mutate(prior = rnorm(n = nrow(treat_effects), 0, 5)) %>%
#   gather(Distribution, Value, -iteration, -Treatment, -Coefficient)
# 
# suppressWarnings( # ignore wanrnings about NA values
#   ggplot(posteriors, aes(x=Value, linetype=Distribution))+
#     geom_line(stat = "density")+
#     geom_vline(aes(xintercept=0), color="red", size=0.2)+
#     scale_x_continuous(limits=c(-5,5))+
#     scale_linetype_discrete(labels = c("Posterior","Prior"), name = NULL)+
#     facet_grid(Coefficient~Treatment)+
#     ylab("Density")+
#     theme_few()+
#     theme(axis.text = element_text(size=7))
# )
# ggsave("../figures/priors_and_posteriors.png", width = 7, height = 4, units = "in", dpi =120)



####
####  OLD STUFF ----
####
# ggplot(betas, aes( x = estimate, color = param_name)) +
#   geom_vline(aes(xintercept=0))+
#   geom_line(stat="density",adjust=5,size=1) +
#   facet_grid(param_name ~ . ) +
#   xlab("Parameter Value")+
#   ylab("Probability Density")+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank())
# 
# treat_effects <- betas %>% 
#   spread( param_name, estimate ) %>% 
#   mutate(Drought = Drought + Control, 
#          Irrigation = Irrigation + Control,  
#          DroughtxSoilMoisture = DroughtxSoilMoisture + ControlxSoilMoisture, 
#          IrrigationxSoilMoisture = IrrigationxSoilMoisture + ControlxSoilMoisture)
# 
# 
# treat_effects <- treat_effects %>% 
#   gather(param_name, estimate, Control:IrrigationxSoilMoisture) %>% 
#   separate(param_name, c('Treatment', 'Type'), sep = 'x')
# 
# treat_effects$Type[is.na(treat_effects$Type)]  <- 'Intercept'
# 
# coefs_plot <- ggplot(treat_effects, aes( x = estimate, color = Treatment)) + 
#   geom_hline(aes(yintercept=0), color="grey45", size=0.1)+
#   geom_line(stat="density",adjust=5,size=1) + 
#   facet_wrap(~Type) + 
#   xlab("Parameter Value")+
#   ylab("Probability Density")+
#   scale_color_brewer(palette = "Set2")+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank())+
#   theme(legend.position = c(0.4,0.78),
#         legend.key.size = unit(6,"pt"),
#         legend.title = element_text(size=10),
#         legend.text = element_text(size = 8),
#         legend.key.height = unit(0.8,"line"),
#         legend.box.background = element_rect(),
#         strip.background = element_blank(), 
#         strip.text = element_text(size=10))+
#   ggtitle("A)")
# 
# 
# 
# 
# ####
# ####  CALCULATE PROBABILITIES OF TREATMENT DIFFERENCES ----
# ####
# 
# 
# comp_id_names <- data.frame(comp_id    = c(1,2,3),
#                             Comparison = c("Control - Drought",
#                                            "Control - Irrigation",
#                                            "Drought - Irrigation"))
# 
# inter_diffs <- reshape2::melt(rstan::extract(all_fit, pars="inter_diffs")) %>%
#   rename(iteration = iterations, comp_id = Var2, estimate = value, stan_name = L1) %>%
#   left_join(comp_id_names, by="comp_id")
# intercept_probs <- inter_diffs %>%
#   group_by(Comparison) %>%
#   summarise(prob = get_one_tailed(estimate)) %>%
#   mutate(beta = "Intercept")
# 
# slope_diffs <- reshape2::melt(rstan::extract(all_fit, pars="vwc_diffs")) %>%
#   rename(iteration = iterations, comp_id = Var2, estimate = value, stan_name = L1) %>%
#   left_join(comp_id_names, by="comp_id")
# slope_probs <- slope_diffs %>%
#   group_by(Comparison) %>%
#   summarise(prob = get_one_tailed(estimate)) %>%
#   mutate(beta = "Soil Moisture Effect")
# 
# beta_probs <- rbind(intercept_probs, slope_probs) %>%
#   spread(key = beta, value = prob)
# saveRDS(beta_probs, file = "../results/betadiff_probabilities.RDS")
# 
# 
# ####
# ####  PLOT TREATMENT-LEVEL POSTERIOR DISTRIBUTIONS ----
# ####
# param_id_names <- data.frame(param_id = c(1,2),
#                              param_name = c("Intercept", "Soil Moisture"))
# treat_id_names <- data.frame(treat_id = c(1,2,3),
#                              treatment = c("Control","Drought","Irrigation"))
# betas <- reshape2::melt(rstan::extract(all_fit, pars="beta_treat")) %>%
#   rename(iteration = iterations, treat_id = Var2, param_id = Var3, estimate = value, stan_name = L1) %>%
#   left_join(param_id_names, by="param_id") %>%
#   left_join(treat_id_names, by="treat_id")
# 
# g1 <- ggplot(betas, aes(x=estimate))+
#   geom_hline(aes(yintercept=0), color="grey45", size=0.1)+
#   geom_line(stat="density",aes(color=treatment),adjust=5,size=1)+
#   scale_color_brewer(palette = "Set2",name="Treatment")+
#   #scale_x_continuous(breaks=seq(-1.5,2,0.5))+
#   #scale_y_continuous(breaks=seq(0,2,0.25))+
#   facet_wrap(~param_name)+
#   xlab("Parameter Value")+
#   ylab("Probability Density")+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank())+
#   theme(legend.position = c(0.4,0.78),
#         legend.key.size = unit(6,"pt"),
#         legend.title = element_text(size=10),
#         legend.text = element_text(size = 8),
#         legend.key.height = unit(0.8,"line"),
#         legend.box.background = element_rect(),
#         strip.background = element_blank(), 
#         strip.text = element_text(size=10))+
#   ggtitle("A)")
# #ggsave("../figures/glmm_treatment_posteriors.png",plot = g1, width = 6, height = 3, units = "in", dpi =120)
# 
# 
# 
# ####
# ####  PLOT MODEL PREDICTIONS WITH DATA ----
# ####
# lmod <- lm(log(anpp) ~ total_seasonal_vwc, anpp_data)
# x <- model.matrix(lmod)
# newx <- as.data.frame(unique(x)) %>% select(total_seasonal_vwc)
# newx$vwc_id <- c(1:nrow(newx))
# mean_log_anpp <- mean(log(anpp_data$anpp))
# sd_log_anpp <- sd(log(anpp_data$anpp))
# model_preds <- reshape2::melt(rstan::extract(all_fit, pars="ypreds_mu")) %>%
#   rename(iteration = iterations, vwc_id = Var2, estimate = value, stan_name = L1) %>%
#   left_join(newx, by="vwc_id") %>%
#   group_by(total_seasonal_vwc) %>%
#   summarise(mean_estimate = mean(estimate*sd_log_anpp+mean_log_anpp)) %>%
#   mutate(backtrans_estimate = mean_estimate) %>%
#   ungroup()
# 
# trt_vwc <- data.frame(treatment = rep(c("Control","Drought","Irrigation"), each = nrow(newx)),
#                       treat_id = rep(c(1:3), each = nrow(newx)),
#                       vwc = rep(newx$total_seasonal_vwc, times = 3),
#                       vwc_id = rep(newx$vwc_id, times = 3))
# treat_preds <- reshape2::melt(rstan::extract(all_fit, pars="ypreds")) %>%
#   rename(iteration = iterations, treat_id = Var2, vwc_id = Var3, estimate = value, stan_name = L1) %>%
#   left_join(trt_vwc, by = c("vwc_id","treat_id")) %>%
#   group_by(treatment, vwc) %>%
#   summarise(mean_estimate = mean(estimate*sd_log_anpp+mean_log_anpp)) %>%
#   mutate(backtrans_estimate = mean_estimate) %>%
#   ungroup()
# 
#  
# g2 <- ggplot(anpp_data, aes(x=total_seasonal_vwc,y=log(anpp)))+
#   geom_point(shape=21,color="grey25",alpha=0.8,aes(fill=Treatment))+
#   geom_line(data=treat_preds, aes(x=vwc, y=backtrans_estimate, color=treatment))+
#   geom_line(data=model_preds, aes(x=total_seasonal_vwc, y=backtrans_estimate), size=1)+
#   scale_fill_brewer(palette = "Set2")+
#   scale_color_brewer(palette = "Set2")+
#   #scale_x_continuous(limits=c(100,300),breaks=seq(100,300,50))+
#   #scale_y_continuous(breaks=seq(50,350,50))+
#   xlab("March-June Cumulative VWC")+
#   ylab(expression(paste("log[ANPP (g ",m^2,")]")))+
#   guides(fill=FALSE,color=FALSE)+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank())+
#   ggtitle("B)")
# 
# mylayout <- c(1,1,2)
# gout <- grid.arrange(grobs=list(g1,g2),layout.matrix=mylayout)
# ggsave("../figures/glmm_main_results.png",plot = gout, width = 5.5, height = 6, units = "in", dpi =120)
# 
# 
# 
# # ####
# # ####  DATA AND REGRESSION PLOT ON ARITHMETIC SCALE FOR SI ----
# # ####
# # ggplot(anpp_data, aes(x=total_seasonal_vwc,y=anpp))+
# #   geom_point(shape=21,color="grey25",alpha=0.8,aes(fill=Treatment))+
# #   geom_line(data=treat_preds, aes(x=vwc, y=exp(backtrans_estimate), color=treatment))+
# #   geom_line(data=model_preds, aes(x=total_seasonal_vwc, y=exp(backtrans_estimate)), size=1)+
# #   scale_fill_brewer(palette = "Set2")+
# #   scale_color_brewer(palette = "Set2")+
# #   #scale_x_continuous(limits=c(100,300),breaks=seq(100,300,50))+
# #   #scale_y_continuous(breaks=seq(50,350,50))+
# #   xlab("March-June Cumulative VWC")+
# #   ylab(expression(paste("ANPP (g ",m^2,")")))+
# #   guides(fill=FALSE,color=FALSE)+
# #   theme_bw()+
# #   theme(panel.grid.minor = element_blank())
# # ggsave("../figures/glmm_main_results_arithmetic.png", width = 4, height = 3, units = "in", dpi =120)
# 
# 
# 
# ####
# ####  PLOT TREATMENT DIFFERENCES BY YEAR POSTERIORS ----
# ####
# param_id_names <- data.frame(param_id = c(1:3),
#                              param_name = c("Control", "Drought", "Irrigation"))
# year_id_names <- data.frame(year_id = c(1:5),
#                             year_name = c(1:5))
# betas <- reshape2::melt(rstan::extract(all_fit, pars="year_off")) %>%
#   rename(iteration = iterations, year_id = Var2, estimate = value, stan_name = L1) %>%
#   left_join(year_id_names, by="year_id")
# 
# ggplot(betas, aes(x=estimate, y=as.factor(year_id), fill=as.factor(year_id), color=as.factor(year_id), height = ..density..))+
#   geom_vline(aes(xintercept=0), linetype=2, color="grey45")+
#   geom_joy(stat="density", adjust=3, alpha=0.9)+
#   scale_x_continuous(breaks=seq(-3,3,0.5), limits=c(-3,3))+
#   scale_y_discrete(labels = c("2012","2013","2014","2015","2016"))+
#   scale_fill_viridis(end=0.8,discrete=T)+
#   scale_color_viridis(end=0.8,discrete=T)+
#   guides(fill=FALSE, color=FALSE)+
#   xlab("Parameter Value")+
#   ylab("Year")+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank(),
#         panel.border     = element_blank(), 
#         axis.line        = element_blank(),
#         axis.ticks       = element_blank())
# ggsave("../figures/glmm_yeardiffs.png", width = 6, height = 3, units = "in", dpi =120)
# 
# 
# # g2 <- ggplot(ydiffs, aes(y=`50%`, x=pptyear, color=Treatment))+
# #   geom_hline(aes(yintercept=0), linetype=2, size=0.2)+
# #   geom_errorbar(aes(ymin = `2.5%`, ymax=`97.5%`), width=0, position = dodge)+
# #   geom_errorbar(aes(ymin=`10%`, ymax=`90%`), size=1.2, width=0, position = dodge)+
# #   geom_point(size=2.5,shape=21,fill="white", position = dodge)+
# #   scale_color_manual(values = RColorBrewer::brewer.pal(3,name = "Set2")[2:3])+
# #   xlab("Year of Experiment")+
# #   ylab("Difference Between\nControl and Treatment")+
# #   theme_few()+
# #   theme(legend.position = c(0.15,0.85),
# #         legend.key.size = unit(6,"pt"),
# #         legend.title = element_text(size=10),
# #         legend.text = element_text(size = 8),
# #         legend.key.height = unit(0.8,"line"))
# # ggsave("../figures/glmm_yeardiffs.png",plot = g2, width = 4, height = 3.5, units = "in", dpi = 120)
# 
# 
# 
# ####
# ####  EXTRACT AND SAVE ALL COEFFICIENT SUMMARIES ----
# ####
# all_betas <- as.data.frame(summary(all_fit, pars = c("beta_treat","beta_mu"), probs = c(0.025,0.1,0.5,0.9,0.975))$summary)
# write.csv(all_betas, "../results/beta_summaries.csv")
# 
# 
# 
# ####
# ####  CALCULATE PROBABILITY OF DIFFERENCE BASED ON INTERCEPTS ----
# ####
# intercept_diffs <- reshape2::melt(rstan::extract(all_fit, pars="inter_diffs")) %>%
#   rename(iteration = iterations, treat_id = Var2, estimate = value, stan_name = L1)
# Pr_control_above_drought <- 1 - ecdf(filter(intercept_diffs,treat_id == 1)[,"estimate"])(0)
# Pr_control_below_irrigation <- ecdf(filter(intercept_diffs,treat_id == 2)[,"estimate"])(0)
# mean_pptyear_probs <- data.frame(Pr_control_above_drought,Pr_control_below_irrigation)
# write.csv(mean_pptyear_probs, "../results/mean_pptyear_probs.csv")
