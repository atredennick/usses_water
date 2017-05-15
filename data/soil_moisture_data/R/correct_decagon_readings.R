rm(list = ls()) 

library( ggplot2 ) 
library(tidyr)
library(dplyr)
library(zoo)

df <- readRDS(file = 'data/processed_data/decagon_data_corrected_dates.RDS')

df <- df %>% ungroup() 

# Filter out bad readings ------------------------------------------------------------------------

get_run_lengths <- function( x ) { 
  unlist ( lapply( rle(x)$lengths, function(x) rep(x, x) ) ) 
}

# My complicated steps for data classification:  ---------------------------------------------------------------
#   calculate rolling mean at 5 values 
#   calculate standard deviations from rolling mean 
#   calculate rolling mean of standard deviations 
#   classify values as high variability or low variabilty based on rolling mean of standard deviations 
#   classify sections as bad windows when 40/100 values are high variability 
#   calculate run lengths of good windows
#   only keep good windows of run lengths > 50 values in a row 

low_VWC_cutoff <- 0.005
high_VWC_cutoff <- 0.01
C_cutoff <- 400

corrected <- 
  df %>% 
  group_by(plot, position, measure ) %>% 
  arrange( new_date) %>% 
  mutate( Tdiff = as.numeric( new_date - lag(new_date), tz = 'MST' ) ) %>% 
  mutate( frame_length = n() ) %>% 
  filter( frame_length > 100 ) %>% 
  mutate( rllm = rollapply( value, 100, mean, na.rm = TRUE, fill = NA, align = 'center')) %>% 
  mutate( dff = (value - rllm)^2) %>% 
  mutate( rllsd = rollapply( dff, 50, mean, na.rm = TRUE, fill = NA, align = 'center')) %>%
  mutate( highv = ifelse( measure == 'VWC' & !is.na(rllsd) & rllm < 0.10 & rllsd > low_VWC_cutoff, 1, 0)) %>% 
  mutate( highv = ifelse( measure == 'VWC' & !is.na(rllsd) & rllm >= 0.10 & rllsd > high_VWC_cutoff, 1, highv)) %>% 
  mutate( highv = ifelse( measure == 'C' & !is.na(rllsd) & rllsd > C_cutoff, 1, highv )) %>% 
  mutate( out_range = ifelse( measure == 'C' & !is.na(value) & (value < -30 | value > 65 ) , 1, 0 )) %>% 
  mutate( out_range = ifelse( measure == 'VWC' & !is.na(value) & (value < -0.125 | value > 0.75), 1, out_range )) %>%
  mutate( highv = ifelse( !is.na(value) & out_range == 1, 1, highv)) %>%
  mutate( total_bad = rollapply( highv, 100 , sum, na.rm = TRUE, fill = NA)) %>% 
  mutate( bad_window = ifelse( !is.na(total_bad) & total_bad > 50, 1, 0)) %>%
  mutate( bad_window = ifelse( is.na(bad_window) , 0, bad_window)) %>%
  mutate( window_lengths = get_run_lengths( bad_window ) ) %>% 
  mutate( bad_window = ifelse( bad_window == 0 & window_lengths < 20, 1, bad_window)) %>%   
  mutate( low_outlier = ifelse( !is.na(value) & measure == 'VWC' & value < -0.02 & (value - lag( rllm )) < -0.06, 1, 0)) %>%
  mutate( low_outlier = ifelse( !is.na(value) & measure == 'C' & value < -20 & (value - lag(rllm)) < -20, 1 , low_outlier)) %>% 
  mutate( bad_window = ifelse( low_outlier == 1, 1, bad_window)) %>%
  mutate( bad_window = ifelse( out_range == 1, 1 , bad_window )) %>% 
  group_by( plot, position, measure, period ) %>% 
  arrange( desc(new_date) ) %>% 
  mutate( bad_window = ifelse ( row_number() == min(which(!is.na(value))), 1, bad_window)) %>% 
  arrange( new_date ) %>% 
  mutate( bad_window = ifelse ( row_number() == min(which(!is.na(value))), 1, bad_window )) %>%
  gather( stat, v, value, rllm, rllsd) 

# manually remove  ---------------------------------------------------------------------------------------- 
corrected$bad_window <- as.numeric(corrected$bad_window)

corrected <- corrected %>% 
  mutate(bad_window = ifelse( plot == '7_8_C' & port == 'Port 4' & measure == 'VWC' & new_date > strptime( '2015-07-01', format = '%Y-%m-%d'), 1, bad_window ) ) %>% 
  mutate(bad_window = ifelse( plot == 1 & position == '5W' & measure == 'VWC' , 1, bad_window)) %>% 
  mutate(bad_window = ifelse( plot == '1_2_C' & position == '5E' & measure == 'VWC' & new_date > strptime( '2016-01-01', format = '%Y-%m-%d'), 1, bad_window ) ) %>% 
  mutate(bad_window = ifelse( plot == 15 & position == '5E' & measure == 'VWC' & new_date > strptime( '2016-01-01', format = '%Y-%m-%d'), 1, bad_window ) ) %>% 
  mutate(bad_window = ifelse( plot == 15 & position == 'air' & measure == 'C' & new_date > strptime( '2016-02-01', format = '%Y-%m-%d'), 1, bad_window ) ) %>% 
  mutate(bad_window = ifelse( plot == 2 & position == '25E' & measure == 'VWC' & new_date > strptime( '2016-01-01', format = '%Y-%m-%d'), 1, bad_window ) ) %>%
  mutate(bad_window = ifelse( plot == 8 & position == 'air' & measure == 'C' & new_date > strptime( '2016-03-01', format = '%Y-%m-%d'), 1, bad_window ) ) %>% 
  mutate(bad_window = ifelse( plot == '7_8_C' & position == '5W' & measure == 'VWC' & stat == 'value' & v < -0.01 , 1, bad_window )) %>% 
  mutate(bad_window = ifelse( plot == '7_8_C' & 
                                position == 'air' & 
                                measure == 'C' &  
                                new_date > strptime( '2013-03-01', format = '%Y-%m-%d') &
                                new_date < strptime( '2013-09-01', format = '%Y-%m-%d'), 1, bad_window )) 
    
# --------------------------------------------------------------------------------------------------------------------------------------

corrected$stat <- factor(corrected$stat, label = c('rolling mean', 'rolling sd', 'raw'))
corrected$plot <- as.character(corrected$plot)
corrected$bad_values <- factor(corrected$bad_window)

corrected <- 
  corrected %>% 
  group_by(plot, position, period, measure ) %>% 
  #filter( good_date ==  1) %>% 
  mutate( has_vals = sum(stat == 'raw' & !is.na(v) ) > 0 ) %>%
  filter( has_vals)

#
corrected$depth <- factor(corrected$depth, labels = c('25 cm deep', '5 cm deep', 'air temperature'))   
#

# 
# wide <- corrected %>% filter( plot == '7_8_C' & position == '5W' & measure == 'VWC') %>%  spread( stat, v ) 
# #
# ggplot( wide, aes( x = new_date, y = raw, color = bad_values )) +
#     geom_point() +
#     geom_line(data  = wide, aes( x = new_date, y =`rolling mean`), color = 'black', alpha = 0.5)  +
#     geom_point(data = wide,aes( x = new_date, y = `rolling sd`), color = 'blue') +
#     geom_point (data = subset(  wide, low_outlier == 1 ), aes( x = new_date, y = raw), color = 'green' ) +
#     geom_hline( aes( yintercept = high_VWC_cutoff), color = 'black') +
#     geom_hline( aes( yintercept = low_VWC_cutoff), color = 'gray')

# # 
# # wide <- corrected %>% spread( stat, v ) %>% filter( position == '5E' & measure == 'VWC')
# 
# ggplot( wide, aes( x = new_date, y = raw, color = bad_values )) + 
#   geom_point() +
#   geom_line(data  = wide, aes( x = new_date, y =`rolling mean`), color = 'black', alpha = 0.5) + 
#   geom_point (data = subset(  wide, low_outlier == 1 ), aes( x = new_date, y = raw), color = 'green' ) + 
#   xlim(strptime( c('2014-08-01', '2014-09-01'), '%Y-%m-%d', tz = 'MST'))
# 
#wide <- corrected %>% spread( stat, v ) %>% filter( position == 'air', measure == 'C')
# #  
#   ggplot( wide, aes( x = new_date, y = raw, color = factor( highv )  , shape = bad_values)) + 
#     geom_point() +
#     geom_point(data = subset( wide , bad_values == 1 ) , aes( x = new_date, y = raw), color = 'black' )  +
#     geom_line(data  = wide, aes( x = new_date, y =`rolling mean`), color = 'black', alpha = 0.5) + 
#     geom_point (data = subset(  wide, low_outlier == 1 ), aes( x = new_date, y = raw), color = 'green' ) + 
#     xlim( strptime( c('2012-04-01', '2012-09-01'), '%Y-%m-%d', tz = 'MST'))


# ------------------------------------------------------------------------------------------

saveRDS(corrected, 'data/processed_data/decagon_data_corrected_values.RDS')

# -------------------------------------------------------------------------------------------


