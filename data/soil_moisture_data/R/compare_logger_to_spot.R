rm(list = ls()) 

library( ggplot2 ) 
library(tidyr)
library(dplyr)
library(lme4)
library(zoo)

df <- readRDS('data/processed_data/decagon_data_with_station_data.RDS')
df_spot <- readRDS('data/processed_data/spring_spot_measurements.RDS')

# ----------------------------------------------------------------------------- 

daily_VWC <- df %>% 
  filter ( measure == 'VWC', depth == '5 cm deep') %>% 
  group_by ( plot, simple_date, depth  ) %>% 
  summarise( logger_avg = mean( v )*100)

spot_avg <- 
  df_spot %>% 
  group_by( plot, date, Treatment, PrecipGroup) %>% 
  summarise( spot = mean(VWC) )

spot_avg$simple_date <- as.Date( spot_avg$date, tz = 'MST')

spot_avg <- left_join( spot_avg, daily_VWC, by = c('plot', 'simple_date'))

m1 <- lm(data = spot_avg, logger_avg ~ spot )
summary(m1)

#spot_avg <- subset( spot_avg, date > as.Date('2013-01-01') ) 
ggplot ( data = spot_avg, aes ( x = spot, y = logger_avg, color = factor(date) ) ) + geom_point()


groups <- df %>% 
  filter( measure== 'VWC', depth == '5 cm deep', !is.na( v), season == 'summer', Treatment != 'Irrigation') %>% 
  group_by( PrecipGroup ) %>% 
  summarise( avg_VWC = mean(v)*100, sd_VWC = sd(v ), n_VWC = n() )

groups_spot <- df_spot %>% 
  filter( Treatment != 'Irrigation') %>% 
  group_by( PrecipGroup ) %>% 
  summarise( avg_VWC = mean(VWC), sd_VWC = sd(VWC), n_VWC = n() ) 
         
groups$type <- 'decagon'
groups_spot$type <- 'spot'

ggplot( rbind ( groups, groups_spot), aes( x = factor( PrecipGroup), y = avg_VWC, fill = type )) + geom_bar( stat = 'identity', position = position_dodge())
