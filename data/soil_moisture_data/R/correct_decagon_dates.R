rm(list = ls()) 

library( ggplot2 ) 
library(tidyr)
library(dplyr)
library(zoo)

df <- readRDS(file = 'data/processed_data/decagon_data.RDS')

# correct bad dates  ------------------------------------------------------------------------------ 

find_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

fill_in_hours_skipped <- function( x ) { 
  
  hs = 0 
  
  for( i in 1:nrow(x)) {
    
    if (is.na( x$change[i] )) {
      
      x$hours_skipped[i] <- hs
      
    }else if(x$change[i] == 1 ){
      
      print(paste('old hs', hs ))
      
      hs <- x$hours_skipped[i] <- x$hours_skipped[i] + hs
      
      print(paste('new hs', hs))
      
    }else if(x$change[i] == 0 ){
      
      hs <- x$hours_skipped[i] <- 0 } 
  }
  
  return( x )
}

df$date_started <- as.character ( levels( df$date_started )[df$date_started] )
df$date_started <- as.POSIXct( df$date_started, tz = 'MST' ) 
df$date_uploaded <- as.character(levels(df$date_uploaded)[ df$date_uploaded ]) 
df$date_uploaded <- as.POSIXct( df$date_uploaded, tz = 'MST')

reading_list <- df %>% ungroup () %>% select( f, plot, id , period, new_date, reading ) %>% mutate( f = factor(f)) %>% distinct()

table( reading_list$f, reading_list$period ) # one file per period 

jumps <- reading_list %>% 
  group_by(f ) %>% 
  arrange( f , reading ) %>% 
  mutate( time_numeric = as.numeric(new_date )) %>% 
  mutate ( time_diff = c(NA, diff(time_numeric, 1 ))) %>%
  mutate( hours_skipped = time_diff/3600 - 2 ) %>% 
  mutate( reading_diff = c(NA, diff(reading, 1))) %>% 
  ungroup() %>% 
  mutate( jump = ifelse( reading_diff == 1 & (hours_skipped != 0 ), 1, 0 )) %>% 
  mutate( lead_jump = lead( jump, 1 )) 

jumps %>% group_by ( f ) %>% summarise( n_jumps =  sum(jump, na.rm = T)) %>% filter ( n_jumps > 0  )  

check <- 
  jumps %>% 
  select( f, new_date, reading, hours_skipped, reading_diff, jump ) %>% 
  filter( jump > 0 , hours_skipped != 0 & reading_diff == 1 ) %>% 
  filter( f != 'data/raw_soil_data/2015_2/EL5739 4Nov15-1838.txt') %>% 
  filter( f != 'data/raw_soil_data/2015_2/EL5742 4Nov15-1820.txt') %>% 
  filter( !( abs(hours_skipped) < 10000 & f == 'data/raw_soil_data/2015_2/EL5743 4Nov15-1828.txt')) %>% 
  filter( f != 'data/raw_soil_data/2013_1/EM20070.txt') %>% 
  filter( f != 'data/raw_soil_data/2013_1/EM20085.txt') %>% 
  filter( !(f== 'data/raw_soil_data/2014_2/15_reordered.txt' & hours_skipped < 4 )) %>% 
  arrange( new_date, f  ) 

#-----------------------------------------------------------------------------------------
write.csv(check, 'data/processed_data/check_dates.csv', row.names = FALSE) # write list of changes 
# ------------------------------------------------------------------------------------------- 

# determined for each jump whether it should be corrected or remain in place 
# change = 1  indicates jumps that should be changed 
# make changes on the csv file above 

check <- read.csv(file = 'data/check_dates_modified.csv') 

check$new_date <- as.POSIXct ( as.character( check$new_date ) , format = '%Y-%m-%d %H:%M:%S', tz = 'MST' ) 

df %>% filter( reading == 76) %>% select( date, new_date, Time, plot )  %>% distinct()

df <- left_join(df, check , by =c( 'f', 'new_date', 'reading' )) # join changes to main df 

df <- df %>% 
  ungroup () %>% 
  group_by(f, plot, port, measure ) %>% 
  arrange( reading ) %>% 
  mutate( hours_skipped = ifelse( row_number() == 1 & is.na(change), 0, hours_skipped ))

out <- df %>%  do ( fill_in_hours_skipped(. ) ) # apply fill in hours function to all measurement groups 

# actually make the date changes here ----------------------------------------------------------------------------------

out <- out %>% 
  mutate( new_date = as.POSIXct(new_date - 60*60*hours_skipped, origin = '1970-01-01 00:00:00', tz = 'MST'))

# ----------------------------------------------------------------------------------------------------------------------
out <- out %>% 
  mutate ( good_date = ifelse ( new_date >= date_started - 60*60*48 & new_date <= date_uploaded + 60*60*48 , 1, 0))

#out %>% ungroup() %>% distinct( f, new_date) %>% group_by(good_date) %>% summarise( n() )

#View( out %>% filter( good_date == 0 ) %>% group_by( f ) %>% distinct( f ) ) 

#out %>% filter( good_date == 0 ) %>% group_by(f ) %>% distinct(f) %>% select( date_started, new_date, date_uploaded) %>% mutate( new_date > date_started & new_date < date_uploaded  )

# check for readings from the same date, time and place # -------------------------------------------------------------- 

#out %>% group_by( plot, port, measure, new_date ) %>% mutate( n =  n() ) %>% filter( n > 1 ) 

# check earliest and latest dates -----------------------------------------------------------------

out %>% ungroup( ) %>% summarise ( max( new_date ), min( new_date ), which.min(new_date ), which.max(new_date ))

# ---------------------------------------------------------------------------- 

out <- out %>% 
  ungroup() %>%
  mutate( simple_date = as.Date(new_date, tz = 'MST'), 
          hour = strftime( new_date, '%H', tz = 'MST'), 
          year = strftime( new_date, '%Y', tz = 'MST'), 
          month = strftime( new_date, '%m', tz = 'MST'))

season <- read.csv('data/season_table.csv')
tod <- read.csv('data/tod_table.csv')

out$month <- as.numeric( out$month)
out$hour <- as.numeric( out$hour)

out <- merge( out, season, by = 'month')
out <- merge( out, tod, by = 'hour')

saveRDS( out , 'data/processed_data/decagon_data_corrected_dates.RDS')

# ----------------------------------------------------------------------------


