rm(list = ls()) 

library( ggplot2 ) 
library(tidyr)
library(dplyr)

df <- readRDS(file = 'data/processed_data/decagon_data.RDS')

plot_ts <- function( x ) { 

  ggplot ( data = x, aes( x = date, y = value , group = port, color = Treatment )) + 
    geom_line()   + 
    facet_wrap( ~ depth, ncol = 1 ) + 
    ggtitle( paste( 'Plot Group:', unique(x$PrecipGroup)) )
  
}

p <- df %>% group_by(PrecipGroup, measure) %>% do( p = plot_ts(x = . )  )


pdf('figures/raw_decagon_time_series.pdf', width = 10, height = 7)

for ( i in p$p ) { 
  
  print( i )   
  
}
dev.off() 

