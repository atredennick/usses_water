
rm(list = ls()) 

library( ggplot2 ) 
library(tidyr)
library(dplyr)
library(lme4)
library(zoo)

df <- readRDS('data/processed_data/decagon_data_with_station_data.RDS')

df <- df %>% mutate( v = ifelse(measure == 'VWC', v*100, v)) # convert to percent 

# summarize treatment differences:  -----------------------------------------------------------------------------------

plot_vals <- df %>% 
  filter( !is.na(v), bad_values == 0 ) %>% 
  group_by(Treatment_label, season_label, depth_label, measure ) %>% 
  summarise( avg = mean(v), stddev = sd(v), max = max(v), min = min(v), n = n(), ci = 1.96*(stddev/sqrt(n)), uci = avg + ci, lci = avg - ci ) 

plot_vals_rainy <- df %>% 
  filter( !is.na(v), bad_values == 0, month > 3, month < 12, measure == 'VWC' , !is.na(rainfall)) %>% 
  group_by(Treatment_label, season_label, depth_label, measure, rainfall ) %>% 
  summarise( avg = mean(v), stddev = sd(v), max = max(v), min = min(v), n = n(), ci = 1.96*(stddev/sqrt(n)), uci = avg + ci, lci = avg - ci )

group_vals <- df %>% 
  filter( !is.na(v), bad_values == 0 ) %>% 
  group_by(PrecipGroup, Treatment_label, season_label, depth_label, measure) %>% 
  summarise( avg = mean(v), stddev = sd(v), max = max(v), min = min(v), n = n(), ci = 1.96*(stddev/sqrt(n)), uci = avg + ci, lci = avg - ci ) 

brplt <- ggplot(plot_vals, aes( x = season_label, y = avg, fill = Treatment_label)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  geom_errorbar( aes(ymax = uci, ymin = lci), position = 'dodge') + 
  facet_wrap( ~ depth_label ) 

brplt_rainy <- brplt + facet_wrap(  depth_label ~ rainfall) 

plot_T_vals <- df %>% 
  filter( !is.na(v), bad_values == 0, measure == 'C', depth_label != '25 cm deep') %>%
  group_by(Treatment_label, season_label, tod, depth_label) %>% 
  summarise( avg = mean(v), stddev = sd(v), max = max(v), min = min(v), n = n(), ci = 1.96*(stddev/sqrt(n)), uci = avg + ci, lci = avg - ci ) 

brplt_tod <- ggplot(plot_vals, aes( x = season_label, y = avg, fill = Treatment_label)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  geom_errorbar( aes(ymax = uci, ymin = lci), position = 'dodge') + 
  facet_wrap( depth_label ~  tod ) 

plot_groups <- ggplot( group_vals, aes ( x = factor( PrecipGroup ) , y = avg, fill = Treatment_label )) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  geom_errorbar( aes(ymax = uci, ymin = lci), position = 'dodge') + 
  facet_wrap( ~ depth_label ) 

brplt %+% subset(plot_vals, measure == 'VWC' ) + ylab( 'Soil Moisture')

brplt_rainy %+% subset(plot_vals_rainy , measure == 'VWC') + ylab( 'Volumetric Soil Moisture (%)')

brplt %+% subset(plot_vals, measure == 'C' & depth_label != '25 cm deep') + ylab( 'Temperature (C)')

brplt_tod %+% plot_T_vals + ylab( 'Temperature (C)')

plot_groups %+% subset( group_vals, measure == 'VWC' & season_label == 'spring') + ylab ( "average spring soil moisture") + xlab ( "plot group")
  
air_T_diffs <- 
  df %>% 
  filter( measure == 'C', depth == 'air temperature', stat == 'raw', bad_values == 0) %>% 
  select( PrecipGroup, simple_date, season, tod, Treatment, v ) %>% 
  group_by( PrecipGroup, Treatment, season, simple_date, tod ) %>%
  summarise( avg_T = mean(v, na.rm = TRUE), n = n()) %>% 
  filter( !is.na(avg_T), n == 6) 

ggplot(air_T_diffs, aes( x = tod, y = avg_T, fill = Treatment )) + 
  geom_boxplot( ) + 
  facet_grid(  ~ season )

# model formula and models ----------------------------------------------------------------------- 

df_soil_moist <- 
  df %>% 
  filter( bad_values == 0 , stat == 'raw', measure == 'VWC') %>% 
  group_by( PrecipGroup, Treatment, plot, season, simple_date, rainfall, unique_position, depth ) %>% 
  summarise( avg_VWC = mean(v), n = n() ) 

df_5cm_soil <- 
  df_soil_moist %>% 
  filter( depth == '5 cm deep' )

df_25cm_soil <- 
  df_soil_moist %>% 
  filter( depth == '25 cm deep')

df_air_temp <- 
  df %>% 
  filter( bad_values == 0 , stat == 'raw', measure == 'C', depth == 'air temperature') %>%
  group_by( season, simple_date, tod, PrecipGroup, Treatment, plot  ) %>% 
  summarise( avg_T = mean(v), n = n() ) %>% 
  filter( n == 6 ) 

df_soil_temp <- 
  df %>% 
  filter( bad_values == 0 , stat == 'raw', measure == 'C', depth == '5 cm deep') %>%
  group_by( season, unique_position, simple_date, tod, PrecipGroup, Treatment, plot  ) %>% 
  summarise( avg_T = mean(v), n = n() ) %>% 
  filter( n == 6 ) 

air_T_form <- formula(avg_T ~ (1|simple_date) + factor( PrecipGroup ) + (1|plot) + tod*Treatment*season )
air_T_null <- update( air_T_form, . ~ . - Treatment:tod:season - Treatment:season - Treatment:tod)

VWC_form <- formula( avg_VWC ~ (1|unique_position) + (1|simple_date) + factor( PrecipGroup) + (1|plot) +  Treatment*rainfall) 
basic_form <- formula( avg_VWC ~ (1|unique_position) + (1|simple_date) + factor( PrecipGroup) + (1|plot) + Treatment*season*rainfall)
basic_null <- update(basic_form, . ~ . - Treatment:season:rainfall - Treatment:season - Treatment:rainfall - Treatment ) 

m_air <- lmer(data =  df_air_temp, formula = air_T_form)

m_5cm <- lmer( data = df_5cm_soil, formula = basic_form, weights = n)
m_25cm <- lmer( data = df_25cm_soil, formula = basic_form, weights = n) 

m_5cm_spring <- lmer( data = subset(df_5cm_soil, season == 'spring'), formula = VWC_form, weights = n)
m_5cm_summer <- lmer( data = subset(df_5cm_soil, season == 'summer'), formula = VWC_form, weights = n ) 
m_5cm_fall <- lmer( data = subset( df_5cm_soil, season == 'fall'), formula = VWC_form, weights = n)
m_5cm_winter <- lmer( data = subset( df_5cm_soil, season == 'winter'), formula = VWC_form, weights = n)

m_25cm_spring <- lmer( data = subset(df_25cm_soil, season == 'spring'), formula = VWC_form, weights = n)
m_25cm_summer <- lmer( data = subset(df_25cm_soil, season == 'summer'), formula = VWC_form, weights = n ) 
m_25cm_fall <- lmer( data = subset( df_25cm_soil, season == 'fall'), formula = VWC_form, weights = n)
m_25cm_winter <- lmer( data = subset( df_25cm_soil, season == 'winter'), formula = VWC_form, weights = n)

summary(m_5cm_spring)
summary(m_5cm_summer)
summary(m_5cm_fall)
summary(m_5cm_winter)

summary(m_25cm_spring)
summary(m_25cm_summer)
summary(m_25cm_fall)
summary(m_25cm_winter)

summary(m_air)
summary(m_soil)

summary(m_5cm) 
summary(m_25cm)

forms <- list( basic_form, basic_null)
models_25cm <- lapply( forms, function(x )  lmer( data = df_25cm_soil, formula = x, weights = n ))
models_5cm <- lapply( forms, function( x) lmer( data = df_5cm_soil, formula = x, weights = n))
models_air <- lapply( list( air_T_form, air_T_null), function( x ) lmer( data = df_air_temp, formula = x))

AICs_m25 <- lapply( models_25cm, AIC )
AICs_m5 <- lapply( models_5cm, AIC )
AICs_air <- lapply( models_air, AIC)

unlist( forms ) 
unlist(AICs_m5)
unlist(AICs_m25)
unlist(AICs_air)


# ------------------------------------------------------------------------------------------------- 
# make prediction df to view lmer effects 

pred_df <- expand.grid( PrecipGroup = 1, Treatment = unique( df$Treatment), season = levels(df$season), rainfall = levels( factor(df$rainfall)), tod = levels(df$tod ) ) 
pred_df$Treatment_label <- factor( pred_df$Treatment, levels = c('Drought', 'Control', 'Irrigation'), order = TRUE) 
pred_df$season_label <- factor( pred_df$season, levels = c('spring', 'summer', 'fall', 'winter'), order = TRUE)

pred_df$air_temp_pred <- predict(m_air, pred_df, re.form = NA)

pred_df$air_temp_pred

pred_df$VWC_5cm_pred <- predict( m_5cm, pred_df, re.form = NA )
pred_df$VWC_25cm_pred <- predict( m_25cm, pred_df, re.form = NA)
pred_df$VWC_5_cm_spring <- predict( m_5cm_spring, pred_df, re.form = NA)

pred_stats_T <- 
  pred_df %>% 
  group_by( Treatment_label, tod, season_label) %>%
  distinct() %>%
  summarise( air_pred = mean(air_temp_pred)) 

pred_stats_VWC <- 
  pred_df %>% 
  group_by(Treatment_label, season_label, rainfall ) %>% 
  distinct() %>% 
  summarise( VWC_5cm_pred = mean(VWC_5cm_pred), VWC_25cm_pred = mean(VWC_25cm_pred))

pred_plot <- 
  ggplot( pred_stats_T, aes( x = tod, y = air_pred, fill = Treatment_label )) + 
  geom_bar( stat = 'identity', position = 'dodge') + 
  facet_wrap( ~ season_label , ncol = 2 ) + 
  ggtitle( 'Treatment effects predicted by lmer model') + 
  ylab( 'Air temperature (C)')

pred_plot_VWC_5 <-
  ggplot( pred_stats_VWC, aes( x = season_label, y = VWC_5cm_pred, fill = Treatment_label )) + 
  geom_bar( stat = 'identity', position = 'dodge') + 
  facet_wrap( ~ rainfall ) + 
  ggtitle( 'Treatment effects predicted by lmer model') + 
  ylab( '5 cm soil moisture (%)')
  
pred_plot_VWC_25 <- 
  ggplot( pred_stats_VWC, aes ( x = season_label, y = VWC_25cm_pred, fill = Treatment_label)) + 
  geom_bar( stat = 'identity', position = 'dodge') + 
  facet_wrap( ~ rainfall ) + 
  ggtitle( 'Treatment effects predicted by lmer model') + 
  ylab( '25 cm soil moisture (%)')

pred_plot_VWC_25
pred_plot_VWC_5

pred_plot

# print plots in one pdf ---------------------------------------------------------------------- 

pdf( "figures/summary_of_treatment_effects.pdf" , height = 8, width = 10 ) 

print( brplt %+% subset(plot_vals, measure == 'VWC' ) + ylab( 'Volumetric Soil Moisture (%)') ) 

print( brplt %+% subset(plot_vals, measure == 'C' & depth_label != '25 cm deep') + ylab( 'Temperature (C)') ) 

print( brplt_rainy %+% subset(plot_vals_rainy , measure == 'VWC') + ylab( 'Volumetric Soil Moisture (%)') )

print( brplt_tod %+% plot_T_vals + ylab( 'Temperature (C)') ) 

print ( plot_groups %+% subset( group_vals, measure == 'VWC' & season_label == 'spring') + ylab ( "Spring soil moisture (%)") + xlab ( "plot group") ) 

print( pred_plot ) 

print( pred_plot_VWC_5)

print( pred_plot_VWC_25)

dev.off() 


