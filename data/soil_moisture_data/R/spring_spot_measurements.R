##### Spring 2015 soil moisture
##### 

rm(list = ls())
library(ggplot2 )
library(dplyr)
library(tidyr)
library(lme4)

q_info <- read.csv('../quad_info.csv')

calibration <- read.csv('data/raw_soil_data/spot_measurements/2015-05-07_soil_probe_calibration.csv')

p1 <- read.csv('data/raw_soil_data/spot_measurements/2012-06-06_spot_measurements.csv', skip = 3)

p1$date <- '2012-06-06'
p1$Plot <- gsub( p1$Plot, pattern = '-', replacement = '_')
p1$rep <- c(1:2)
p1 <- p1 %>% rename( plot = Plot )

p2 <- read.csv('data/raw_soil_data/spot_measurements/2015-04-29_spot_measurements.csv', skip = 2)
p3 <- read.csv('data/raw_soil_data/spot_measurements/2015-05-07_spot_measurements.csv') 
p4 <- read.csv('data/raw_soil_data/spot_measurements/2016-05-10_spot_measurements.csv')
p5 <- read.csv('data/raw_soil_data/spot_measurements/2015-06-09_spot_measurements.csv')

p2$date <- '2015-04-29'

df <- rbind( p2, p3, p4, p5)

df <- df %>% gather( key = rep, PCT, E1:W3 )

df <- rbind( p1, df )

q_info$plot <- gsub( q_info$QuadName, pattern = 'X', replacement = '')

df <- merge (df, q_info , by = 'plot')

df$date <- as.POSIXct(df$date, tz = 'MST')
df <- df %>% rename(VWC = PCT)

soil_density <- read.csv('data/raw_soil_data/gravimetric_samples/exclosure_soil_samples.csv')

soil_density <- soil_density %>% 
  mutate( layer = ifelse( depth > 15, 'deep', 'shallow') ) %>% 
  mutate( wet_soil = wet_weight - bag_weight, soil = dry_weight - bag_weight ) %>% 
  mutate( water_weight = wet_soil - soil, soil_density = soil/soil_volume_cm3, GWC = water_weight/soil) %>% 
  group_by(layer) %>% 
  summarise( avg_soil_density = mean(soil_density))

soil_d_value <- soil_density$avg_soil_density[ soil_density$layer == 'shallow']
  
# run calibration ---------------------------------------------------------------------------------------------------- 

calibration <- 
  calibration %>% 
  mutate( wet_weight = (wet_weight- bag_weight) , dry_weight = (dry_weight - bag_weight), 
          water_weight = wet_weight - dry_weight, GWC = 100*water_weight/dry_weight) %>% 
  gather( rep, VWC, T1:T4 ) %>% 
  mutate ( GWC_calc = VWC/ soil_d_value) %>% 
  gather( type, val, VWC:GWC_calc)


calib_plot <- ggplot( calibration, aes( x = val, y = GWC, color = type, group = type ) )  + 
  geom_point(size = 2) + 
  geom_smooth( method = 'lm', se = FALSE, aes( linetype = type ), size = 1) + 
  ylab( 'Gravimetric water content from soil samples (g water/ g soil)' ) + 
  xlab( 'Volumetric water content from Decagon probes (cm^3 water/ cm^3 soil)') +  
  ylim ( 0, 30) + 
  xlim ( 0, 34)

calib_plot

calib_df <- calibration %>% filter( type == 'GWC_calc') %>% group_by( plot ) %>% summarise( GWC = unique(GWC), GWC_calc = mean(val)) 

m1 <- lm( data = calib_df, GWC ~ GWC_calc)
summary(m1)

# spot measurements --------------------------------------------------------------------------------------------------

plot_by_date <- ggplot( df , aes ( x = factor(date), y = VWC , color = Treatment )) + 
  geom_boxplot() + 
  ylab( 'Volumetric water content from decagon probe (%)') + 
  geom_point(position = position_dodge(width = 0.8)) 

plot_by_treat <- ggplot( df, aes( x = Treatment, y = VWC, color = Treatment)) + 
  geom_boxplot() + 
  ylab( 'Volumetric water content from decagon probe (%)') + 
  geom_point( position = position_dodge(width = 0.8))

plot_by_date + ggtitle( 'Spot measurements')
plot_by_treat + ggtitle( 'Spot measurements')

# analysis -------------------------------------------------------------------------------------------------------------

spot_form <- formula( VWC ~ (1|date) + (1|PrecipGroup) + (1|plot) + Treatment)

spot_m <- lmer( data = df , spot_form)
spot_m_2016 <- lmer(data = subset(df, date > '2016-01-01'), VWC ~ (1|plot) + Treatment)

summary(spot_m)
summary(spot_m_2016)

# print figures --------------------------------------------------------------------------------------------------------

pdf( 'figures/spot_measurements.pdf', height = 8 , width = 10 ) 

print( calib_plot ) 
print( plot_by_date ) 
print( plot_by_treat ) 

dev.off()

# Save spot measurements -----------------------------------------------------------------------------------------------

saveRDS( df, 'data/processed_data/spring_spot_measurements.RDS')
