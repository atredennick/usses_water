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
library(viridis)
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

cover_species <- read.csv("../data/vital_rates/allrecords_cover_v24.csv") %>%
  group_by(quad, year, species) %>%
  summarise(total_area = sum(area)) %>%
  left_join(plot_info) %>%
  filter(Treatment == "Control" | Treatment == "Irrigation" | Treatment == "Drought") %>%
  filter(!str_detect(QuadName, 'P1|P7')) %>%
  group_by(year, species, Treatment) %>%
  summarise(avg_area = mean(total_area)) %>%
  ungroup()



####
####  RANK CLOCKS (COVER) ----
####
domspp <- c("Artemisia tripartita", "Poa secunda", "Pseudoroegneria spicata", "Hesperostipa comata")
cover_ts <- cover_species %>%
  filter(year > 2010) 

## Create the graph
g1 <- ggplot(cover_ts, aes(year, log(avg_area), color = species)) + 
  geom_line(size = 1) + 
  facet_wrap(~Treatment) +
  coord_polar()+
  labs(x=NULL, y=NULL) + 
  guides(color=FALSE)+
  theme_dark()+
  theme(text = element_text(size = 14), 
        strip.text.x = element_text(size = 14, color="black"), 
        strip.background = element_blank(),
        panel.grid.major = element_line(size = 1, color="grey55"),
        panel.grid.minor = element_line(colour="grey55")) + 
  theme(legend.position="bottom", 
        legend.text=element_text(face = "italic")) +
  geom_segment(aes(x = 2011, y = 0, xend = 2011, yend = 0.4), color = "grey70")+
  scale_color_viridis(discrete=T)+
  scale_x_continuous(breaks=c(2011,2012,2013,2014,2015))+
  ggtitle("A. All species")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(color="grey90"),
        axis.ticks.y=element_blank())
 

cover_ts <- cover_species %>%
  filter(year > 2010, species %in% domspp) 

## Create the graph
g2 <- ggplot(cover_ts, aes(year, log(avg_area), color = species)) + 
  geom_line(size = 1) + 
  facet_wrap(~Treatment) +
  coord_polar()+
  labs(x=NULL, y=NULL, color="Species") + 
  theme_dark()+
  theme(text = element_text(size = 14), 
        strip.text.x = element_text(size = 14, color="black"), 
        strip.background = element_blank(),
        panel.grid.major = element_line(size = 1, color="grey55"),
        panel.grid.minor = element_line(colour="grey55")) + 
  # theme_bw() +
  # theme(text = element_text(size = 14), 
  #       strip.text.x = element_text(size = 14), 
  #       strip.background = element_blank(),
  #       panel.grid.major = element_line(size = 1)) + 
  theme(legend.position="bottom", 
        legend.text=element_text(face = "italic")) +
  geom_segment(aes(x = 2011, y = 0, xend = 2011, yend = 0.4), color = "grey70")+
  scale_x_continuous(breaks=c(2011,2012,2013,2014,2015))+
  scale_color_brewer(palette = "Accent")+
  #guides(color=FALSE)+
  ggtitle("B. Dominant species")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(color="grey90"),
        axis.ticks.y=element_blank())
# 
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())

gout <- grid.arrange(g1,g2,nrow=2)
ggsave("../figures/rank_clocks_cover.png", plot = gout, width = 9, height = 7.5, units="in", dpi=120)



####
####  RANK CLOCKS (DENSITY) ----
####
density_species <- read.csv("../data/vital_rates/allrecords_density_v24.csv") %>%
  mutate(ind_num = 1) %>%
  group_by(year, quad, species) %>%
  summarise(total_inds = sum(ind_num)) %>%
  left_join(plot_info) %>%
  filter(Treatment == "Control" | Treatment == "Irrigation" | Treatment == "Drought") %>%
  filter(!str_detect(QuadName, 'P1|P7')) %>%
  group_by(year, species, Treatment) %>%
  summarise(avg_dens = mean(total_inds)) %>%
  ungroup()

domspp <- c("Artemisia tripartita", "Poa secunda", "Pseudoroegneria spicata", "Hesperostipa comata")
density_ts <- density_species %>%
  filter(year > 2010) 

## Create the graph
g1 <- ggplot(density_ts, aes(year, log(avg_dens), color = species)) + 
  geom_line(size = 1) + 
  facet_wrap(~Treatment) +
  coord_polar()+
  labs(x=NULL, y=NULL) + 
  guides(color=FALSE)+
  theme_bw() +
  theme(text = element_text(size = 14), 
        strip.text.x = element_text(size = 14), 
        strip.background = element_blank(),
        panel.grid.major = element_line(size = 1)) + 
  theme(legend.position="bottom", 
        legend.text=element_text(face = "italic")) +
  geom_segment(aes(x = 2011, y = 0, xend = 2011, yend = 0.4), color = "grey70")+
  scale_x_continuous(breaks=c(2011,2012,2013,2014,2015))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave("../figures/rank_clocks_density.png", plot = g1, width = 8, height = 3.5, units="in", dpi=120)



# density_ts <- density_species %>%
#   filter(year > 2010, species %in% domspp) 

## Create the graph
# g2 <- ggplot(density_ts, aes(year, log(avg_dens), color = species)) + 
#   geom_line(size = 1) + 
#   facet_wrap(~Treatment) +
#   coord_polar()+
#   labs(x=NULL, y=NULL, color="Species") + 
#   theme_bw() +
#   theme(text = element_text(size = 14), 
#         strip.text.x = element_text(size = 14), 
#         strip.background = element_blank(),
#         panel.grid.major = element_line(size = 1)) + 
#   theme(legend.position="bottom", 
#         legend.text=element_text(face = "italic")) +
#   geom_segment(aes(x = 2011, y = 0, xend = 2011, yend = 0.4), color = "grey70")+
#   scale_x_continuous(breaks=c(2011,2012,2013,2014,2015))+
#   guides(color=FALSE)+
#   ggtitle("B. Dominant species")+
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# 
# gout <- grid.arrange(g1,g2,nrow=2)




####
####  MEAN RANK SHIFTS ----
####
rank_shifts <- codyn::rank_shift(df = density_ts,
                                 time.var = "year", 
                                 species.var = "species", 
                                 replicate.var = "Treatment", 
                                 abundance.var = "avg_dens")

ggplot(rank_shifts, aes(x=year_pair,y=MRS,color=Treatment,group=Treatment))+
  geom_line()+
  scale_color_brewer(palette = "Set2")+
  theme_few()



####
####  COMPARE COMMUNITY COMPOSITION AMONG TREATMENTS ----
####  USING STANDARDIZED ABUNDANCES FROM COVER AND DENSITY
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

density_spp <- colnames(density_community_matrix[4:ncol(density_community_matrix)])
cover_spp <- colnames(cover_community_matrix[4:ncol(cover_community_matrix)])
overlap_spp <- density_spp[which(density_spp %in% cover_spp)]
rmids <- which(colnames(density_community_matrix) %in% overlap_spp)
density_community_matrix <- density_community_matrix[,-rmids]

annual_community_matrix <- read.csv("../data/vital_rates/idaho_annuals.csv") %>%
  select(-Notes) %>%
  gather(species,abundance,-c(1:3)) %>%
  left_join(plot_info, by="quad") %>%
  select(-QuadName.y) %>%
  rename(QuadName = QuadName.x) %>%
  filter(Treatment == "Control" | Treatment == "Irrigation" | Treatment == "Drought") %>%
  filter(!str_detect(QuadName, 'P1|P7')) %>%
  filter(year > 2010 & is.na(species)==F) %>%
  separate(species,c("genus","species1")) %>%
  unite(species,genus,species1,sep = " ") %>%
  spread(species,abundance, fill = 0)%>%
  select(-QuadName,-Grazing,-paddock,-Group,-`Lappula occidentalis`,-`Tragopogon dubius`) %>%
  ungroup()

full_community_matrix <- cover_community_matrix %>%
  left_join(density_community_matrix) %>%
  left_join(annual_community_matrix)

# set missing species abundances as 0, which == absent
full_community_matrix[is.na(full_community_matrix)] <- 0 

full_community_matrix_scaled <- cbind(full_community_matrix[,1:3,drop=F], 
                                      scale(full_community_matrix[,4:ncol(full_community_matrix)]))

spp_to_remove <- names(which(is.nan(colMeans(full_community_matrix_scaled[4:ncol(full_community_matrix_scaled)]))))
ids_to_remove <- which(colnames(full_community_matrix_scaled) %in% spp_to_remove)
full_community_matrix_scaled <- full_community_matrix_scaled %>%
  select(-ids_to_remove,-`Agropyron cristatum`,-`Antennaria rosea`,-`Linanthus pungens`)


nmds_df <- {}
out_stats <- {}
pdf("../figures/rda_raw.pdf",onefile = TRUE)
for(doyr in unique(full_community_matrix_scaled$year)){
  tmp <- filter(full_community_matrix_scaled, year == doyr) %>% select(-year)
  # tmp_rda <- vegan::rda(tmp[3:ncol(tmp)]~tmp$Treatment)
  tmp_rda <- metaMDS(tmp[3:ncol(tmp)]+2, "bray", trymax = 999)
  # nmds_points <- as.data.frame(summary(tmp_rda)$sites[,1:2])
  # nmds_points$Treatment <- tmp$Treatment
  # nmds_points$Year <- doyr
  # nmds_df <- rbind(nmds_df, nmds_points)
  nmds_points <- as.data.frame(tmp_rda$points)
  nmds_points$Treatment <- tmp$Treatment
  nmds_points$Year <- doyr
  nmds_df <- rbind(nmds_df, nmds_points)
  plot(tmp_rda, main=doyr)
  adonis_stats <- as.data.frame(vegan::adonis(tmp[3:ncol(tmp)]+2~tmp$Treatment)$aov.tab)[1,]
  betadisper_stats <- anova(vegan::betadisper(vegan::vegdist(tmp[3:ncol(tmp)]+2),tmp$Treatment))[1,]
  tmp_out <- rbind(adonis_stats[,-which(colnames(adonis_stats)=="R2")],
                   as.numeric(betadisper_stats))
  row.names(tmp_out) <- c()
  out_stats <- rbind(out_stats, data.frame(year = doyr,
                                           test = c("adonis","betadisper"),
                                           n = nrow(tmp),
                                           tmp_out))
}
dev.off()

ggplot(nmds_df, aes(x=MDS1, y=MDS2, color=Treatment))+
  geom_point()+
  geom_point(color="grey35",shape=1)+
  scale_color_brewer(palette = "Set2")+
  ylab("NMDS 2")+
  xlab("NMDS 1")+
  facet_wrap("Year")+
  scale_y_continuous(limits=c(-0.15,0.15))+
  scale_x_continuous(limits=c(-0.18,0.18))+
  theme_few()+
  theme(panel.grid.major = element_line(color="grey90"))
ggsave(paste0(figure_path,"sppcomp_bray_all.png"), width=6, height = 4, units = "in", dpi = 120)




####
####  COMPARE COMMUNITY COMPOSITION AMONG TREATMENTS -- COVER ----
####
# cover_community_matrix <- read.csv("../data/vital_rates/allrecords_cover_v24.csv") %>%
#   group_by(quad, year, species) %>%
#   summarise(total_area = sum(area)) %>%
#   left_join(plot_info) %>%
#   filter(Treatment == "Control" | Treatment == "Irrigation" | Treatment == "Drought") %>%
#   filter(!str_detect(QuadName, 'P1|P7')) %>%
#   filter(year > 2010 & is.na(species)==F) %>%
#   spread(species,total_area, fill = 0) %>%
#   select(-QuadName,-Grazing,-paddock,-Group) %>%
#   ungroup()
# 
# mycol <- RColorBrewer::brewer.pal(3,"Set2")
# # 
# # par(mfrow=c(2,3))
# nmds_df <- {}
# for(doyr in unique(cover_community_matrix$year)){
#   tmp <- filter(cover_community_matrix, year == doyr) %>% select(-year)
#   torm <- as.numeric(which(colSums(tmp[3:ncol(tmp)], na.rm = T) == 0)) # find spp with NA in each treatment
#   tmp <- tmp[,-(torm+2)] # add 2 because we lopped off two columns above
#   tmp_rda <- metaMDS(tmp[3:ncol(tmp)], "bray")
#   ## Try to plot in ggplot
#   nmds_points <- as.data.frame(tmp_rda$points)
#   nmds_points$Treatment <- tmp$Treatment
#   nmds_points$Year <- doyr
#   nmds_df <- rbind(nmds_df, nmds_points)
# }
# 
# comp1 <- ggplot(nmds_df, aes(x=MDS1, y=MDS2, color=Treatment))+
#   geom_point()+
#   geom_point(color="grey35",shape=1)+
#   scale_color_brewer(palette = "Set2")+
#   scale_y_continuous(limits=c(-2,2))+
#   scale_x_continuous(limits=c(-2,2))+
#   ylab("NMDS 2")+
#   xlab("NMDS 1")+
#   facet_wrap("Year")+
#   ggtitle("A. Cover data")+
#   theme_few()
# ggsave(paste0(figure_path,"sppcomp_nmds.png"), width=6, height = 4, units = "in", dpi = 120)
# 
#  
# 
# ####
# ####  COMPARE COMMUNITY COMPOSITION AMONG TREATMENTS -- DENSITY ----
# ####
# density_community_matrix <- read.csv("../data/vital_rates/allrecords_density_v24.csv") %>%
#   mutate(ind_num = 1) %>%
#   group_by(year, quad, species) %>%
#   summarise(total_inds = sum(ind_num)) %>%
#   left_join(plot_info) %>%
#   filter(Treatment == "Control" | Treatment == "Irrigation" | Treatment == "Drought") %>%
#   filter(!str_detect(QuadName, 'P1|P7')) %>%
#   filter(year > 2010 & is.na(species)==F) %>%
#   spread(species,total_inds, fill = 0) %>%
#   select(-QuadName,-Grazing,-paddock,-Group) %>%
#   ungroup()
# 
# mycol <- RColorBrewer::brewer.pal(3,"Set2")
# nmds_df <- {}
# for(doyr in unique(density_community_matrix$year)){
#   tmp <- filter(density_community_matrix, year == doyr) %>% select(-year)
#   torm <- as.numeric(which(colSums(tmp[3:ncol(tmp)], na.rm = T) == 0)) # find spp with NA in each treatment
#   tmp <- tmp[,-(torm+2)] # add 2 because we lopped off two columns above
#   tmp_rda <- metaMDS(tmp[3:ncol(tmp)], "bray")
#   ## Try to plot in ggplot
#   nmds_points <- as.data.frame(tmp_rda$points)
#   nmds_points$Treatment <- tmp$Treatment
#   nmds_points$Year <- doyr
#   nmds_df <- rbind(nmds_df, nmds_points)
# }
# 
# comp2 <- ggplot(nmds_df, aes(x=MDS1, y=MDS2, color=Treatment))+
#   geom_point()+
#   geom_point(color="grey35",shape=1)+
#   scale_color_brewer(palette = "Set2")+
#   scale_y_continuous(limits=c(-2,2))+
#   scale_x_continuous(limits=c(-2,2))+
#   ylab("NMDS 2")+
#   xlab("NMDS 1")+
#   facet_wrap("Year")+
#   ggtitle("B. Density data")+
#   theme_few()
# ggsave(paste0(figure_path,"sppcomp_density_nmds.png"), width=6, height = 4, units = "in", dpi = 120)
# 
# gout <- grid.arrange(comp1,comp2,nrow=2)
# ggsave("../figures/nmds_comp_combined.png", plot = gout, width = 6, height = 8, units="in", dpi=120)



####
####  CALCULATE SYNCHRONY OF SPECIES COVER ----
####

# 
# ggplot(cover_species, aes(x=year, y=avg_area, col=species))+
#   geom_line()+
#   geom_text(data=subset(cover_species,year==2011),aes(label=species))+
#   facet_grid(Treatment~.)+
#   guides(color=F)
# 
# cover_synchrony <- codyn::synchrony(filter(cover_species, year > 2010 & is.na(species)==F), 
#                                     time.var = "year", species.var = "species", 
#                                     metric = "Loreau", abundance.var = "avg_area", 
#                                     replicate.var = "Treatment")
# cover_synchrony
# ggplot(cover_synchrony, aes(x=synchrony, y=Treatment, color=Treatment))+
#   geom_lollipop(horizontal = T, point.size = 2)+
#   scale_x_continuous(limits = c(0,1), expand = c(0,0))+
#   scale_color_brewer(palette = "Set2")+
#   labs(x="Community synchrony", y=NULL) +
#   guides(color=F)+
#   theme_minimal()+
#   theme(panel.grid.major.y=element_blank(),
#         panel.grid.minor=element_blank(),
#         axis.line.y=element_line(color="#2b2b2b", size=0.15),
#         axis.text.y=element_text(margin=margin(r=0, l=0)),
#         plot.margin=unit(rep(30, 4), "pt"),
#         plot.title=element_text(face="bold"),
#         plot.subtitle=element_text(margin=margin(b=10)),
#         plot.caption=element_text(size=8, margin=margin(t=10)))
# ggsave(paste0(figure_path,"synchrony_treatment.png"), height = 2, width = 3.5, units = "in", dpi = 120)

