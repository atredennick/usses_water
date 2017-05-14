##  usses_stability.R: R script to analyze the temporal stability of plots at
##    USSES in Idaho. Plots received irrigation/drought treatments starting
##    in 2011.
##
##  Author:       Andrew Tredennick
##  Date created: April 24, 2017
##
##  UPDATES  ###################################################################
##
##
##  ############################################################################

##  Clear the workspace
rm(list=ls(all.names = TRUE))



####
####  LOAD PACKAGES ----
####
library(tidyverse)
library(dplyr)
library(stringr)
library(ggthemes)
library(ggalt)
library(broom)
library(RColorBrewer)
library(lme4)
library(codyn)
library(vegan)
library(gridExtra)
library(car)



####
####  SET DIRECTORIES; READ IN DATA ----
####
# Set working dir to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # only works in RStudio

# Set paths
biomass_path <- "../data/estimated_biomass/"
weather_path <- "../data/weather/"
output_path  <- "../output/"
figure_path  <- "../figures/"

# Read in data
plot_biomass <- readRDS(paste0(biomass_path,"permanent_plots_estimated_biomass.RDS"))
weather_data <- read.csv(paste0(weather_path,"ClimateIPM.csv"))
plot_info    <- read.csv("../data/estimated_biomass/quad_info.csv")

# Merge biomass and weather data; keep irrigation treatments only
plot_biomass_weather <- plot_biomass %>%
  left_join(weather_data, by="year") %>%
  filter(Treatment == "Control" | Treatment == "Irrigation" | Treatment == "Drought") %>%
  filter(is.na(biomass_grams_est) == FALSE)



####
####  CALCULATE TEMPORAL STABILITY OF PLOTS ----
####
trt_biomass <- plot_biomass_weather %>%
  filter(year > 2010) %>%
  mutate(trt_quad = paste(Treatment,quadname,sep="::"))
plot_stability <- codyn::community_stability(trt_biomass, time.var = "year", 
                                             abundance.var = "biomass_grams_est", 
                                             replicate.var = "trt_quad")
plot_stability <- plot_stability %>%
  separate(trt_quad, c("Treatment", "quadname"), sep="::")
 
density_species <- read.csv("../data/vital_rates/allrecords_density_v24.csv") %>%
  select(quad,year,species)

cover_species <- read.csv("../data/vital_rates/allrecords_cover_v24.csv") %>%
  select(quad,year,species)

all_species <- rbind(density_species, cover_species)
all_spp_richness <- all_species %>%
  group_by(quad,year) %>%
  summarise(species_richness = length(unique(species))) %>%
  left_join(plot_info) %>%
  mutate(quadname = gsub(" ","",QuadName)) %>%
  group_by(quadname) %>%
  summarise(avg_richness = mean(species_richness))
  

stability_richness <- plot_stability %>%
  left_join(all_spp_richness) %>%
  filter(!str_detect(quadname, 'P1|P7'))

ggplot(stability_richness, aes(x=avg_richness, y=stability, color=Treatment))+
  geom_point(size=2)+
  geom_point(shape=1, color="grey35", size=2)+
  scale_color_brewer(palette = "Set2")+
  xlab(expression(paste("Average species richness (",m^-2,")")))+
  ylab(expression(paste("Stability of ANPP (", mu/sigma,")")))+
  theme_few()
ggsave(paste0(figure_path,"stability_richness.png"), height = 3, width = 4.5, units = "in", dpi = 120)

ggplot(stability_richness, aes(y = stability, x = Treatment, fill=Treatment))+
  geom_boxplot(outlier.color = NA, width=0.2, color="grey24",alpha=0.5)+
  geom_jitter(width=0.1,shape=21,color="grey35")+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(breaks=seq(1.4,2.6,0.2))+
  ylab(expression(paste("Stability of ANPP (", mu/sigma,")")))+
  guides(fill=F)+
  theme_few()
ggsave(paste0(figure_path,"stability_treatment.png"), height = 3, width = 3, units = "in", dpi = 120)



####
####  ANOVA FOR STABILITY ----
####
stability_mod <- lm(stability ~ Treatment, data=stability_richness)
capture.output(
  print(car::Anova(stability_mod)),
  file = "../results/stability_anova.txt"
)



####
####  CALCULATE SYNCHRONY OF SPECIES COVER ----
####
cover_species <- read.csv("../data/vital_rates/allrecords_cover_v24.csv") %>%
  group_by(quad, year, species) %>%
  summarise(total_area = sum(area)) %>%
  left_join(plot_info) %>%
  filter(Treatment == "Control" | Treatment == "Irrigation" | Treatment == "Drought") %>%
  filter(!str_detect(QuadName, 'P1|P7')) %>%
  group_by(year, species, Treatment) %>%
  summarise(avg_area = mean(total_area)) %>%
  ungroup()

ggplot(cover_species, aes(x=year, y=avg_area, col=species))+
  geom_line()+
  geom_text(data=subset(cover_species,year==2011),aes(label=species))+
  facet_grid(Treatment~.)+
  guides(color=F)

cover_synchrony <- codyn::synchrony(filter(cover_species, year > 2010 & is.na(species)==F), 
                                    time.var = "year", species.var = "species", 
                                    metric = "Loreau", abundance.var = "avg_area", 
                                    replicate.var = "Treatment")
cover_synchrony
ggplot(cover_synchrony, aes(x=synchrony, y=Treatment, color=Treatment))+
  geom_lollipop(horizontal = T, point.size = 2)+
  scale_x_continuous(limits = c(0,1), expand = c(0,0))+
  scale_color_brewer(palette = "Set2")+
  labs(x="Community synchrony", y=NULL) +
  guides(color=F)+
  theme_minimal()+
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line.y=element_line(color="#2b2b2b", size=0.15),
        axis.text.y=element_text(margin=margin(r=0, l=0)),
        plot.margin=unit(rep(30, 4), "pt"),
        plot.title=element_text(face="bold"),
        plot.subtitle=element_text(margin=margin(b=10)),
        plot.caption=element_text(size=8, margin=margin(t=10)))
ggsave(paste0(figure_path,"synchrony_treatment.png"), height = 2, width = 3.5, units = "in", dpi = 120)



####
####  COMPARE SYNCHRONY OF DOMINANT SPECIES ----
####




####
####  COMPARE COMMUNITY COMPOSITION AMONG TREATMENTS -- COVER ----
####
cover_community_matrix <- read.csv("../data/vital_rates/allrecords_cover_v24.csv") %>%
  group_by(quad, year, species) %>%
  summarise(total_area = sum(area)) %>%
  left_join(plot_info) %>%
  filter(Treatment == "Control" | Treatment == "Irrigation" | Treatment == "Drought") %>%
  filter(!str_detect(QuadName, 'P1|P7')) %>%
  filter(year > 2010 & is.na(species)==F) %>%
  spread(species,total_area, fill = 0) %>%
  select(-QuadName,-Grazing,-paddock,-Group) %>%
  ungroup()

mycol <- RColorBrewer::brewer.pal(3,"Set2")
# 
# par(mfrow=c(2,3))
nmds_df <- {}
for(doyr in unique(cover_community_matrix$year)){
  tmp <- filter(cover_community_matrix, year == doyr) %>% select(-year)
  torm <- as.numeric(which(colSums(tmp[3:ncol(tmp)], na.rm = T) == 0)) # find spp with NA in each treatment
  tmp <- tmp[,-(torm+2)] # add 2 because we lopped off two columns above
  tmp_rda <- metaMDS(tmp[3:ncol(tmp)], "bray")
  ## Try to plot in ggplot
  nmds_points <- as.data.frame(tmp_rda$points)
  nmds_points$Treatment <- tmp$Treatment
  nmds_points$Year <- doyr
  nmds_df <- rbind(nmds_df, nmds_points)
}

comp1 <- ggplot(nmds_df, aes(x=MDS1, y=MDS2, color=Treatment))+
  geom_point()+
  geom_point(color="grey35",shape=1)+
  scale_color_brewer(palette = "Set2")+
  scale_y_continuous(limits=c(-2,2))+
  scale_x_continuous(limits=c(-2,2))+
  ylab("NMDS 2")+
  xlab("NMDS 1")+
  facet_wrap("Year")+
  ggtitle("A. Cover data")+
  theme_few()
ggsave(paste0(figure_path,"sppcomp_nmds.png"), width=6, height = 4, units = "in", dpi = 120)

 

####
####  COMPARE COMMUNITY COMPOSITION AMONG TREATMENTS -- DENSITY ----
####
density_community_matrix <- read.csv("../data/vital_rates/allrecords_density_v24.csv") %>%
  mutate(ind_num = 1) %>%
  group_by(year, quad, species) %>%
  summarise(total_inds = sum(ind_num)) %>%
  left_join(plot_info) %>%
  filter(Treatment == "Control" | Treatment == "Irrigation" | Treatment == "Drought") %>%
  filter(!str_detect(QuadName, 'P1|P7')) %>%
  filter(year > 2010 & is.na(species)==F) %>%
  spread(species,total_inds, fill = 0) %>%
  select(-QuadName,-Grazing,-paddock,-Group) %>%
  ungroup()

mycol <- RColorBrewer::brewer.pal(3,"Set2")
nmds_df <- {}
for(doyr in unique(density_community_matrix$year)){
  tmp <- filter(density_community_matrix, year == doyr) %>% select(-year)
  torm <- as.numeric(which(colSums(tmp[3:ncol(tmp)], na.rm = T) == 0)) # find spp with NA in each treatment
  tmp <- tmp[,-(torm+2)] # add 2 because we lopped off two columns above
  tmp_rda <- metaMDS(tmp[3:ncol(tmp)], "bray")
  ## Try to plot in ggplot
  nmds_points <- as.data.frame(tmp_rda$points)
  nmds_points$Treatment <- tmp$Treatment
  nmds_points$Year <- doyr
  nmds_df <- rbind(nmds_df, nmds_points)
}

comp2 <- ggplot(nmds_df, aes(x=MDS1, y=MDS2, color=Treatment))+
  geom_point()+
  geom_point(color="grey35",shape=1)+
  scale_color_brewer(palette = "Set2")+
  scale_y_continuous(limits=c(-2,2))+
  scale_x_continuous(limits=c(-2,2))+
  ylab("NMDS 2")+
  xlab("NMDS 1")+
  facet_wrap("Year")+
  ggtitle("B. Density data")+
  theme_few()
ggsave(paste0(figure_path,"sppcomp_density_nmds.png"), width=6, height = 4, units = "in", dpi = 120)

gout <- grid.arrange(comp1,comp2,nrow=2)
ggsave("../figures/nmds_comp_combined.png", plot = gout, width = 6, height = 8, units="in", dpi=120)

