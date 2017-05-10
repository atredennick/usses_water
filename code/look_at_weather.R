##  look_at_weather.R: Script to look at the historical distribution of
##  precipitation at Dubois, Idaho, and to compare our treatment years to
##  historical events.
##
##  Author: Andrew Tredennick (atredenn@gmail.com)
##  Date created: May 10, 2017

##  Clear the workspace
rm(list = ls(all.names = T))

##  Set the working directory (RStudio only)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


####
####  LIBRARIES ----
####
library(tidyverse)
library(dplyr)
library(stringr)
library(ggthemes)
library(gridExtra)
library(RColorBrewer)



####
####  READ IN WEATHER DATA AND PLOT ----
####
weather <- read.csv("../data/weather/ClimateIPM.csv")
trt_data <- weather %>%
  filter(year>2011) %>%
  select(year, ppt1)
trt_data$Treatment <- "Control"
trt_data <- rbind(trt_data, data.frame(year = trt_data$year,
                                       ppt1 = trt_data$ppt1*0.5,
                                       Treatment = "Drought"))

g1 <- ggplot(weather, aes(x=ppt1))+
  geom_histogram(bins=20,color="white",fill="grey35")+
  geom_point(data=trt_data, aes(x=ppt1, y=rep(seq(0.3,1,length.out = 5),2),shape=Treatment, fill=as.factor(year)), size=2)+
  scale_x_continuous(expand=c(0,0), limits=c(0,620), breaks=seq(0,600,100))+
  scale_y_continuous(expand=c(0,0), breaks = seq(0,25,5),limits = c(0,28))+
  scale_shape_manual(values=c(21,22))+
  scale_fill_brewer(palette ="Set1", name="Year")+
  ylab("Frequency")+
  xlab("Growing Season Precipitation (mm)")+
  ggtitle("A")+
  theme_few()+
  theme(legend.position = c(0.8, 0.6),legend.key.size = unit(1,"pt"),legend.title = element_text(size=10),
        legend.text = element_text(size = 8),
        legend.key.height = unit(0.8,"line"))+
  guides(fill = guide_legend(override.aes = list(color = brewer.pal(5,"Set1"),size=1)),
         shape = guide_legend(override.aes = list(size=1)))
ggsave(plot = g1,"../figures/historical_precip_trts.png", width = 4, height = 3.5, units="in", dpi=120)



####
####  READ IN ANPP DATA AND PLOT TREND ----
####
permanent_quad_biomass <- readRDS("../data/estimated_biomass/permanent_plots_estimated_biomass.RDS")



####
####  INITIAL PLOTS ----
####
permanent_quad_biomass <- permanent_quad_biomass %>% filter(Treatment %in% c("Control","Drought","Irrigation"))
biomass_year_treatment <- permanent_quad_biomass %>%
  group_by(Treatment,year) %>%
  summarise(mean_biomass = mean(biomass_grams_est))


biomass_yr_trt_summ <- permanent_quad_biomass %>%
  filter(!str_detect(quadname, 'P1|P7')) %>%
  group_by(Treatment,year) %>%
  summarise(mean_biomass = mean(biomass_grams_est),
            sd_biomass = sd(biomass_grams_est)) %>%
  filter(year > 2011)

g2 <- ggplot(biomass_yr_trt_summ, aes(x=year, y=mean_biomass, color=Treatment))+
  geom_line()+
  geom_errorbar(aes(ymin=mean_biomass-sd_biomass, ymax=mean_biomass+sd_biomass), width=0.05)+
  geom_point(color="white", size=3)+
  geom_point()+
  geom_point(color="grey35", shape=1)+
  scale_color_brewer(palette = "Set2", name="Treatment")+
  scale_x_continuous(breaks=c(2011:2016))+
  scale_y_continuous(breaks=seq(50,350,50))+
  ylab(expression(paste("Estimated ANPP (g ", m^-2,")")))+
  xlab("Year")+
  ggtitle("B")+
  theme_few()+
  guides(color = guide_legend(override.aes = list(size=1)))+
  theme(legend.position = c(0.2, 0.8),legend.key.size = unit(1,"pt"),legend.title = element_text(size=10),
        legend.text = element_text(size = 8),
        legend.key.height = unit(0.8,"line"))



####
####  COMBINE PLOTS AND SAVE ----
####
gout <- grid.arrange(g1,g2,ncol=2)
ggsave("../figures/histppt_and_anpptrend.png", gout, width=8, height=3.5, units="in", dpi=120)
