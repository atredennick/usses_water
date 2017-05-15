# plot corrected 
rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)

df <- readRDS('data/processed_data/decagon_data_corrected_values.RDS')

p <- ggplot( df, aes ( x = new_date, y = v, color = stat))  + 
  geom_point() 

p2 <- ggplot( df , aes ( x = new_date, y = v, color = bad_values, group = position)) + 
  geom_point() 

pp <- df %>% 
  group_by(plot, position, period, measure) %>% 
  do( pp =  p %+%  . + ylab ( unique(.$measure)) + ggtitle( paste( c('plot ', 'period '), c(unique(.$plot), unique(.$period) ), collapse = '; ') ) ) 

pp_all <- df %>%
  filter( stat == 'raw' ) %>% 
  group_by( plot, position, measure) %>% 
  do(pp = p2 %+% . + ylab( unique( .$measure)) + ggtitle( paste( c( 'treatment:', unique(levels(.$Treatment)[.$Treatment]), '; plot:', unique(.$plot), ';', unique(.$position)), collapse = ' ') ) )

pdf( 'figures/check_bad_windows_continuous.pdf', height = 8, width = 10 )
print( pp_all$pp ) 
dev.off() 

# plot all readings ------------------------------------------------------------------------------------- 

plot_ts <- function( x ) { 
  
  ggplot ( data = x, aes( x = new_date, y = v , group = position, color = Treatment )) + 
    geom_point( alpha = 0.2, size = 0.2)   + 
    facet_wrap( ~ depth, ncol = 1 ) + 
    ylab( unique(x$measure)) + 
    ggtitle( paste( 'Plot Group:', unique(x$PrecipGroup)) )
  
}

plot_ts_diff <- function( x ) { 
  ggplot ( data = x, aes( x = new_date, y = Irrigation_effect)) + 
    geom_point( alpha = 0.2, size = 0.2)   + 
    facet_wrap( ~ depth, ncol = 1 ) + 
    ylab( paste( unique(x$measure), 'Irrigation - Drought' )) + 
    ggtitle( paste( 'Plot Group:', unique(x$PrecipGroup)) )
}


p <- df %>% 
  filter( stat == 'raw', bad_values == 0 ) %>% 
  group_by(PrecipGroup, measure) %>% 
  do( p = plot_ts(x = . )  )


p_diff <- df %>% 
  filter( stat == 'raw', bad_values == 0 ) %>% 
  group_by( PrecipGroup, Treatment, measure, depth, new_date ) %>% 
  summarise( mean_v = mean( v ) ) %>% 
  select( PrecipGroup, Treatment, measure, depth, new_date, mean_v) %>% 
  spread( Treatment, mean_v) %>% 
  mutate ( Irrigation_effect = Irrigation - Drought ) %>% 
  group_by(PrecipGroup, measure) %>% 
  do( p = plot_ts_diff(x = . )  )


pdf('figures/corrected_decagon_time_series.pdf', width = 10, height = 7)
print( p$p )   
dev.off() 

pdf( 'figures/corrected_decagon_time_series_plot_differences.pdf', width = 10, height = 7) 
print ( p_diff$p ) 
dev.off() 



