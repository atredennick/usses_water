rm(list = ls()) 

library( ggplot2 ) 
library(tidyr)
library(dplyr)
library(lme4)
library(zoo)

df <- readRDS('data/processed_data/decagon_data_with_station_data.RDS')

# ----------------------------------------------------------------------------- 

df <- df %>% mutate( v = ifelse(measure == 'VWC', v*100, v))

df <- df %>% 
  select( Treatment_label, season_label, depth_label, Treatment, PrecipGroup, plot, depth, season, unique_position, prcp_event, prerain, month, measure, new_date, total_rain, v, prerain, PRCP) %>% 
  filter( measure == 'VWC', month > 3, month < 11 , !is.na(prerain)) %>% 
  arrange( unique_position , new_date) 

postrain_VWC <- 
  df %>% 
  filter( measure == 'VWC', !is.na(v)) %>% 
  group_by( prcp_event) %>% 
  mutate( event_strt = min(new_date)) %>% 
  filter( !prerain) %>% 
  group_by(Treatment_label, Treatment, season_label, depth_label, depth, PrecipGroup, plot, season, unique_position, prcp_event, event_strt) %>% 
  summarise( cumul_rain = total_rain[which.max(v)], postrain_VWC = max(v) )

prerain_VWC <- 
  df %>% 
  filter( measure == 'VWC', !is.na(v)) %>% 
  group_by( prcp_event) %>% 
  filter( prerain) %>% 
  group_by( unique_position, prcp_event) %>% 
  summarise( prerain_VWC = max(v) )

rain_effects <- left_join(postrain_VWC, prerain_VWC, by = c('unique_position', 'prcp_event'))

rain_effects <- rain_effects %>% ungroup ( ) %>% mutate( change_VWC = postrain_VWC - prerain_VWC ) 

rain_effect_plot <- ggplot( rain_effects, aes( x = cumul_rain, y = change_VWC, color = Treatment_label) ) + 
  geom_point()  + 
  geom_smooth( method = 'lm', se = FALSE) + 
  facet_wrap( ~ season_label ) + 
  ylab ( 'Change in volumetric water content (%)') + 
  xlab ( 'Cumulative rainfall during event (mm)') 

rain_effect_plots <- rain_effects %>% 
  group_by ( depth_label) %>% 
  do( p =  rain_effect_plot %+% . + ggtitle( paste0( "Effect of rain on soil moisture at ",  .$depth_label )) )

print( rain_effect_plots$p ) 

m1 <- lmer(data = subset( rain_effects, depth == '5 cm deep', season = 'summer'), change_VWC ~ prerain_VWC*cumul_rain + cumul_rain*Treatment + factor( PrecipGroup ) + (1|plot) + (1|prcp_event) + (1|unique_position))

summary(m1)

pred_df <- expand.grid( prerain_VWC = mean(rain_effects$prerain_VWC, na.rm = TRUE), 
                        cumul_rain = seq(min(rain_effects$cumul_rain, na.rm = TRUE), 
                                         max(rain_effects$cumul_rain, na.rm = TRUE), 
                                         length.out = 100), 
                        Treatment = levels( factor(rain_effects$Treatment) ) , 
                        PrecipGroup = unique( rain_effects$PrecipGroup ), 
                        depth_label = '5 cm deep', 
                        season_label = 'summer')

pred_df$change_VWC <- predict( m1, pred_df, re.form = NA)

pred_df$Treatment_label <- factor(pred_df$Treatment, levels = c('Drought', 'Control', 'Irrigation'), ordered = TRUE)


pdf( 'figures/rain_effects_plots.pdf', height = 8, width = 10 ) 

print( rain_effect_plots$p ) 

print ( rain_effect_plot %+% pred_df + ggtitle( 'Predicted rain effects at 5 cm') ) 

dev.off()


