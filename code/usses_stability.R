##  usses_stability.R: R script to analyze the temporal stability of plots at
##    USSES in Idaho. Plots received irrigation/drought treatments starting
##    in 2012.
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

ggplot(stability_richness, aes(y = stability, x = Treatment, color=Treatment))+
  geom_jitter(width=0.1)+
  geom_boxplot(fill=NA,outlier.color = NA, width=0.25)+
  scale_color_brewer(palette = "Set2")+
  ylab(expression(paste("Stability of ANPP (", mu/sigma,")")))+
  guides(color=F)+
  theme_few()
ggsave(paste0(figure_path,"stability_treatment.png"), height = 3, width = 3, units = "in", dpi = 120)



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
####  COMPARE COMMUNITY COMPOSITION AMONG TREATMENTS ----
####
cover_community_matrix <- read.csv("../data/vital_rates/allrecords_cover_v24.csv") %>%
  group_by(quad, year, species) %>%
  summarise(total_area = sum(area)) %>%
  left_join(plot_info) %>%
  filter(Treatment == "Control" | Treatment == "Irrigation" | Treatment == "Drought") %>%
  filter(year > 2010 & is.na(species)==F) %>%
  spread(species,total_area, fill = 0) %>%
  select(-QuadName,-Grazing,-paddock,-Group) %>%
  ungroup()

mycol <- RColorBrewer::brewer.pal(3,"Set2")
png(paste0(figure_path,"sppcomp_nmds.png"), width=6, height = 4, units = "in", res = 120)
par(mfrow=c(2,3))
for(doyr in unique(cover_community_matrix$year)){
  tmp <- filter(cover_community_matrix, year == doyr) %>% select(-year)
  torm <- as.numeric(which(colSums(tmp[3:ncol(tmp)], na.rm = T) == 0)) # find spp with NA in each treatment
  tmp <- tmp[,-(torm+2)] # add 2 because we lopped off two columns above
  tmp_rda <- metaMDS(tmp[3:ncol(tmp)], "bray")
  tmp_dist <- vegdist(tmp[3:ncol(tmp)], method = "bray")
  tmp_clust <- hclust(tmp_dist, method = "average")
  mds.fig <- ordiplot(tmp_rda, type = "none", main = doyr)
  # plot just the samples, colour by habitat, pch=19 means plot a circle
  ordicluster(tmp_rda, tmp_clust, col = "gray")
  points(mds.fig, "sites", pch = 19, col = mycol[1], select = tmp$Treatment == "Control")
  points(mds.fig, "sites", pch = 19, col = mycol[2], select = tmp$Treatment == "Irrigation")
  points(mds.fig, "sites", pch = 19, col = mycol[3], select = tmp$Treatment == "Drought")
  points(mds.fig, "sites", pch = 1, col = "grey35", select = tmp$Treatment == "Control")
  points(mds.fig, "sites", pch = 1, col = "grey35", select = tmp$Treatment == "Irrigation")
  points(mds.fig, "sites", pch = 1, col = "grey35", select = tmp$Treatment == "Drought")
  # pca_scores <- scores(tmp_rda)
  # plot(pca_scores[,1],
  #      pca_scores[,2],
  #      pch=21,
  #      bg=as.numeric(tmp$Treatment),
  #      ylab="PCA1", xlab=("PCA2"),
  #      main = doyr)
}
dev.off()
