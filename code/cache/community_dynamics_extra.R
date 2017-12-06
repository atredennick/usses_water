####
####  EXTRACT COVER OF MOST DOMINANT SPECIES ----
####
cover_data <- {}
for(dotrt in unique(cover_community_matrix$Treatment)){
  tmp <- cover_community_matrix %>%
    gather(species, area, -quad, -year, -Treatment) %>%
    filter(Treatment == dotrt) %>%
    group_by(Treatment, species) %>%
    summarise(mean_area = mean(area)) %>%
    arrange(-mean_area) %>%
    mutate(mean_rank = seq_along(mean_area))
  
  cover_data <- rbind(cover_data, tmp)
}


ggplot(cover_data, aes(x = mean_rank, y = mean_area))+
  geom_line()+
  geom_point(size = 2)+
  geom_text(data=filter(cover_data, mean_rank < 11), aes(x = mean_rank, y = mean_area, label = species), 
            hjust = -0.05, nudge_x = 0.2, angle = 45)+
  scale_y_continuous(limits = c(0,0.25))+
  xlab("Mean rank")+
  ylab("Mean area (cm*cm)")+
  facet_wrap(~Treatment, ncol = 3)


####
####  LOOK AT PERENNIALS AND ANNUALS SEPARATELY ----
####
##  Perennials
# Set missing species abundances as 0, which == absent
# density_community_matrix[is.na(density_community_matrix)] <- 0 
# 
# density_community_matrix_scaled <- cbind(density_community_matrix[,1:3,drop=F], 
#                                       scale(density_community_matrix[,4:ncol(density_community_matrix)]))
# 
# spp_to_remove <- names(which(is.nan(colMeans(density_community_matrix_scaled[4:ncol(density_community_matrix_scaled)]))))
# ids_to_remove <- which(colnames(density_community_matrix_scaled) %in% spp_to_remove)
# density_community_matrix_scaled <- density_community_matrix_scaled %>%
#   select(-ids_to_remove)
# 
# 
# nmds_df_perennials <- {}
# out_stats <- {}
# for(doyr in unique(density_community_matrix_scaled$year)){
#   tmp <- filter(density_community_matrix_scaled, year == doyr) %>% select(-year)
#   invisible( capture.output( # already checked for convergence
#     tmp_bray <- metaMDS(tmp[3:ncol(tmp)]+2, "bray", trymax = 999)
#   ))
#   nmds_points <- as.data.frame(tmp_bray$points)
#   nmds_points$Treatment <- tmp$Treatment
#   nmds_points$Year <- doyr
#   nmds_df_perennials <- rbind(nmds_df_perennials, nmds_points)
#   adonis_stats <- as.data.frame(vegan::adonis(tmp[3:ncol(tmp)]+2~tmp$Treatment)$aov.tab)[1,]
#   betadisper_stats <- anova(vegan::betadisper(vegan::vegdist(tmp[3:ncol(tmp)]+2),tmp$Treatment))[1,]
#   tmp_out <- rbind(adonis_stats[,-which(colnames(adonis_stats)=="R2")],
#                    as.numeric(betadisper_stats))
#   row.names(tmp_out) <- c()
#   out_stats <- rbind(out_stats, data.frame(year = doyr,
#                                            test = c("adonis","betadisper"),
#                                            n = nrow(tmp),
#                                            tmp_out))
# }
# saveRDS(object = out_stats, file = "../results/sppcomp_perennials_stats.RDS")
# 
# 
# ##  Annuals
# # Set missing species abundances as 0, which == absent
# annual_community_matrix[is.na(annual_community_matrix)] <- 0 
# 
# annual_community_matrix_scaled <- cbind(annual_community_matrix[,1:3,drop=F], 
#                                          scale(annual_community_matrix[,4:ncol(annual_community_matrix)]))
# 
# spp_to_remove <- names(which(is.nan(colMeans(annual_community_matrix_scaled[4:ncol(annual_community_matrix_scaled)]))))
# ids_to_remove <- which(colnames(annual_community_matrix_scaled) %in% spp_to_remove)
# annual_community_matrix_scaled <- annual_community_matrix_scaled %>%
#   select(-ids_to_remove)
# 
# 
# nmds_df_annuals <- {}
# out_stats <- {}
# for(doyr in unique(annual_community_matrix_scaled$year)){
#   tmp <- filter(annual_community_matrix_scaled, year == doyr) %>% select(-year)
#   invisible( capture.output( # already checked for convergence
#     tmp_bray <- metaMDS(tmp[3:ncol(tmp)]+2, "bray", trymax = 999)
#   ))
#   nmds_points <- as.data.frame(tmp_bray$points)
#   nmds_points$Treatment <- tmp$Treatment
#   nmds_points$Year <- doyr
#   nmds_df_annuals <- rbind(nmds_df_annuals, nmds_points)
#   adonis_stats <- as.data.frame(vegan::adonis(tmp[3:ncol(tmp)]+2~tmp$Treatment)$aov.tab)[1,]
#   betadisper_stats <- anova(vegan::betadisper(vegan::vegdist(tmp[3:ncol(tmp)]+2),tmp$Treatment))[1,]
#   tmp_out <- rbind(adonis_stats[,-which(colnames(adonis_stats)=="R2")],
#                    as.numeric(betadisper_stats))
#   row.names(tmp_out) <- c()
#   out_stats <- rbind(out_stats, data.frame(year = doyr,
#                                            test = c("adonis","betadisper"),
#                                            n = nrow(tmp),
#                                            tmp_out))
# }
# saveRDS(object = out_stats, file = "../results/sppcomp_annuals_stats.RDS")
# 
# nmds_df_perennials$planttype <- "Perennial"
# nmds_df_annuals$planttype <- "Annual"
# nmds_df_combo <- rbind(nmds_df_perennials, nmds_df_annuals)
# 
# mycols <- c("#009E73", "#0072B2", "#D55E00")
# ggplot(nmds_df_combo, aes(x=MDS1, y=MDS2, color=Treatment))+
#   geom_point()+
#   geom_point(color="grey35",shape=1)+
#   scale_color_manual(values=mycols)+
#   ylab("NMDS 2")+
#   xlab("NMDS 1")+
#   facet_grid(planttype~Year)+
#   #scale_y_continuous(limits=c(-0.15,0.15))+
#   #scale_x_continuous(limits=c(-0.18,0.18))+
#   coord_fixed()+
#   theme_few()+
#   theme(panel.grid.major = element_line(color="grey90"))
