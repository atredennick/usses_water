##  usses_stability.R: R script to analyze the temporal stability of plots at
##    USSES in Idaho. Plots received irrigation/drought treatments starting
##    in 2011.
##
##  Author:       Andrew Tredennick
##  Date created: April 24, 2017

##  Clear the workspace
rm(list=ls(all.names = TRUE))

##  Set working dir to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # only for RStudio



####
####  LOAD PACKAGES ----
####
library(tidyverse) # all the good things from Wikham
library(dplyr)     # data munging
library(stringr)   # working with strings
library(ggthemes)  # pleasing ggplot2 themes
library(viridis)   # color color scheme
library(vegan)     # community similarity metrics



####
####  SET DIRECTORIES; READ IN DATA ----
####
# Set paths
biomass_path <- "../data/estimated_biomass/"
weather_path <- "../data/weather/"
output_path  <- "../output/"
figure_path  <- "../figures/"

# Read in data
plot_biomass <- readRDS(paste0(biomass_path,"permanent_plots_estimated_biomass.RDS"))
weather_data <- read.csv(paste0(weather_path,"ClimateIPM.csv"))
plot_info    <- read.csv("../data/estimated_biomass/quad_info.csv")

suppressWarnings( # ignore factor to character warning. It's OK.
  cover_species <- read.csv("../data/vital_rates/allrecords_cover_v24.csv") %>%
    group_by(quad, year, species) %>%
    summarise(total_area = sum(area)) %>%
    left_join(plot_info, by = "quad") %>%
    filter(Treatment == "Control" | Treatment == "Irrigation" | Treatment == "Drought") %>%
    filter(!str_detect(QuadName, 'P1|P7')) %>%
    group_by(year, species, Treatment) %>%
    summarise(avg_area = mean(total_area)) %>%
    ungroup()
)




####
####  COMPARE COMMUNITY COMPOSITION AMONG TREATMENTS ----
####  USING STANDARDIZED ABUNDANCES FROM COVER AND DENSITY
####
suppressWarnings( # ignore factor to character warning. It's OK.
  cover_community_matrix <- read.csv("../data/vital_rates/allrecords_cover_v24.csv") %>%
    group_by(quad, year, species) %>%
    summarise(total_area = sum(area)) %>%
    left_join(plot_info, by = "quad") %>%
    filter(Treatment == "Control" | Treatment == "Irrigation" | Treatment == "Drought") %>%
    filter(!str_detect(QuadName, 'P1|P7')) %>%
    filter(year > 2010 & is.na(species)==F) %>%
    spread(species,total_area, fill = 0) %>%
    select(-QuadName,-Grazing,-paddock,-Group) %>%
    ungroup()
)

suppressWarnings( # ignore factor to character warning. It's OK.
  density_community_matrix <- read.csv("../data/vital_rates/allrecords_density_v24.csv") %>%
    mutate(ind_num = 1) %>%
    group_by(year, quad, species) %>%
    summarise(total_inds = sum(ind_num)) %>%
    left_join(plot_info, by = "quad") %>%
    filter(Treatment == "Control" | Treatment == "Irrigation" | Treatment == "Drought") %>%
    filter(!str_detect(QuadName, 'P1|P7')) %>%
    filter(year > 2010 & is.na(species)==F) %>%
    spread(species,total_inds, fill = 0) %>%
    select(-QuadName,-Grazing,-paddock,-Group) %>%
    ungroup()
)

density_spp <- colnames(density_community_matrix[4:ncol(density_community_matrix)])
cover_spp   <- colnames(cover_community_matrix[4:ncol(cover_community_matrix)])
overlap_spp <- density_spp[which(density_spp %in% cover_spp)]
rmids       <- which(colnames(density_community_matrix) %in% overlap_spp)
density_community_matrix <- density_community_matrix[,-rmids]

suppressWarnings( # ignore factor to character warning. It's OK.
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
    spread(species,abundance, fill = 0) %>%
    select(-QuadName,-Grazing,-paddock,-Group,-`Lappula occidentalis`,-`Tragopogon dubius`) %>%
    ungroup()
)

full_community_matrix <- cover_community_matrix %>%
  left_join(density_community_matrix, by = c("quad", "year", "Treatment")) %>%
  left_join(annual_community_matrix, by = c("quad", "year", "Treatment"))

# Set missing species abundances as 0, which == absent
full_community_matrix[is.na(full_community_matrix)] <- 0 

full_community_matrix_scaled <- cbind(full_community_matrix[,1:3,drop=F], 
                                      scale(full_community_matrix[,4:ncol(full_community_matrix)]))

spp_to_remove <- names(which(is.nan(colMeans(full_community_matrix_scaled[4:ncol(full_community_matrix_scaled)]))))
ids_to_remove <- which(colnames(full_community_matrix_scaled) %in% spp_to_remove)
full_community_matrix_scaled <- full_community_matrix_scaled %>%
  select(-ids_to_remove,
         -`Agropyron cristatum`,
         -`Antennaria rosea`,
         -`Linanthus pungens`)


nmds_df <- {}
out_stats <- {}
pdf("../figures/bray_raw.pdf",onefile = TRUE)
for(doyr in unique(full_community_matrix_scaled$year)){
  tmp <- filter(full_community_matrix_scaled, year == doyr) %>% select(-year)
 invisible( capture.output( # already checked for convergence
    tmp_bray <- metaMDS(tmp[3:ncol(tmp)]+2, "bray", trymax = 999)
  ))
  nmds_points <- as.data.frame(tmp_bray$points)
  nmds_points$Treatment <- tmp$Treatment
  nmds_points$Year <- doyr
  nmds_df <- rbind(nmds_df, nmds_points)
  plot(tmp_bray, main=doyr)
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
saveRDS(object = out_stats, file = "../results/sppcomp_stats.RDS")

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
####  RANK CLOCKS ON SCALED COMMUNITY DATA ----
####
scaled_comp_ts <- full_community_matrix_scaled %>%
  gather(key = species, value = scaled_abundance, -quad, -year, -Treatment) %>%
  group_by(year, Treatment, species) %>%
  summarise(mean_scaled_abundance = mean(scaled_abundance))

## Create the graph
g1 <- ggplot(scaled_comp_ts, aes(year, mean_scaled_abundance, color = species)) + 
  geom_line(size = 1, alpha = 0.8) + 
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


domspp <- c("Artemisia tripartita", "Poa secunda", "Pseudoroegneria spicata", "Hesperostipa comata")
scaled_dom_ts <- scaled_comp_ts %>%
  filter(species %in% domspp)

## Create the graph
g2 <- ggplot(scaled_dom_ts, aes(year, mean_scaled_abundance, color = species)) + 
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
  theme(legend.position="bottom", 
        legend.text=element_text(face = "italic")) +
  geom_segment(aes(x = 2011, y = 0, xend = 2011, yend = 0.4), color = "grey70")+
  scale_x_continuous(breaks=c(2011,2012,2013,2014,2015))+
  scale_color_brewer(palette = "Accent")+
  ggtitle("B. Dominant species")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(color="grey90"),
        axis.ticks.y=element_blank())

gout <- grid.arrange(g1,g2,nrow=2)
ggsave("../figures/rank_clocks_scaled_abundance.png", plot = gout, width = 9, height = 7.5, units="in", dpi=120)


