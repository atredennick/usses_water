##############################################################################################################
#
# This script reads in and exports the decagon soil moisture and temperature data from the raw data files. 
#
##############################################################################################################

rm(list = ls())

library(stringr)
library(dplyr)
library(tidyr)

make_col_names <- function( x ) { 
  
  port <- str_extract_all(x[1, ], pattern = '(Time)|(Port\\s[0-9])') 
  probe_type <- str_extract( x[1, ] , pattern = '(ECT)|(EC\\-5)|(5TM)')
  measure <- str_extract( x [ 1, ] , pattern = '(VWC$)|((C$|C\\sTemp$))')
  
  new_names <- paste( port, measure , sep = '_')
  new_names <-  str_replace(string = new_names, pattern = '\\sTemp$', replacement = '')
  str_replace_all(string = new_names, pattern = c('_NA'), replacement = c('')) 
  
}


rename_cols <- function(x ) { 
  
  names(x) <- make_col_names( x ) 
  
  return(   x[-1, ] ) 
}

assign_NAs <- function( x ) { 
  
  x [ x == '* * * '] <- NA 
  
  return( x )
}

convert_time <- function(x) { 

  return( strptime(x = x$Time, format = '%m/%d/%y %I:%M %p', tz = 'MST') ) 

}


make_date <- function(x) { 
  
  x$date <- convert_time( x )
  
  x$date <- as.POSIXct(x$date, format = '%Y-%m-%d %H:%m:%s' )
  
  return(x)
}

make_readings <- function( x ) { 
  
  m <- regexpr(row.names(x), pattern = '([0-9]+$)')
  
  x$reading <- as.numeric(regmatches(row.names(x), m))
  
  return( x ) 
}


gather_ports <- function ( test ) { 
  test %>% 
    gather( key = port, value = value ,  starts_with("Port") ) %>% 
    separate(col = port, into = c('port', 'measure') , sep = '_') 
} 


q_info <- read.csv('../quad_info.csv') 

folders <- dir('data/raw_soil_data', pattern = '20[0-9]{2}_[1-2]$', full.names = TRUE)

data_list <- list(NA)

for (i in 1:length(folders)) {
  
  record_file <- dir(folders[i] , pattern = 'logger_info.csv', full.names = TRUE) 
  
  record <- read.csv(record_file)
  
  f <- dir(folders[i], pattern = '^E[ML][0-9]+.*txt$', full.names = TRUE)

  f_raw <- dir(folders[i], pattern = '^E[ML][0-9]+.*csv$', full.names = TRUE, recursive = TRUE)
  
  f2 <- dir(folders[i], pattern = '^[0-9]+(_[0-9]+_C)?.*txt$', full.names = TRUE)
  
  f2_raw <- dir(folders[i], pattern = '^[0-9]+(_[0-9]+_C)?.*csv$', full.names = TRUE, recursive = TRUE)
  
  f <- c(f, f_raw, f2_raw, f2)
  
  m_date <- file.mtime(f)
  
  attributes(m_date)$tzone <- 'MST' 
  
  logger <- str_extract(basename(f), pattern = '(^E[ML][0-9]+)|(^[0-9]+(_[0-9]+_C)?)')
  
  file_df <-  data.frame( f, m_date, logger  )
  
  file_df$type <- str_extract( file_df$f, pattern = '.txt|.csv')
  
  file_df <- file_df %>% 
    group_by( logger ) %>% 
    mutate( modified_date = min(m_date )) %>% 
    filter( type == '.txt') %>% 
    select( - m_date )
  
  d <- lapply(as.character(file_df$f), read.table, sep = '\t', colClasses = 'character')  
  
  names(d) <- file_df$logger

  d <- lapply(d, rename_cols) 

  d <- lapply(d, assign_NAs)

  d <- lapply(d, make_date) 
  
  d <- lapply(d, make_readings )

  d <- lapply( d, gather_ports ) 

  df <- do.call(rbind, d)

  df$id <- gsub( pattern = '\\.[0-9]+$', replacement = '', x = row.names(df))
  
  df <- merge(df, record, by.x = 'id', by.y = 'logger' )
  
  df <- merge(df, file_df, by.x = 'id', by.y = 'logger')
  
  df$value <- as.numeric(df$value)
  
  df <-  df %>% 
    mutate( tail = ifelse ( is.na( tail ) , 2 , tail ), hours = ifelse(is.na(hours), 0, hours)) %>% 
    filter( reading > tail ) %>% 
    mutate( new_date = date - hours*60*60)
  
  data_list[[i]] <- df 
  
} 


df <- do.call( rbind, data_list )  # bind the data lists from each folder 

df  <- 
  df %>% 
  group_by(plot , port , measure, reading , date, value) %>% 
  arrange(period ) %>% filter( row_number() == 1  ) # when there are duplicate records get data from only the first file 

q_info$plot <- gsub( q_info$QuadName, pattern = 'X', replacement = '')

df <- merge( df, q_info, by = 'plot') 

port_depth <- read.csv('data/sensor_positions.csv')

port_depth <- port_depth %>% gather( port, position, `Port.1`:`Port.5`)

port_depth$depth <- str_extract( port_depth$position, pattern = '(air)|([0-9]+)')

port_depth$port <- str_replace(port_depth$port, pattern = '\\.', replacement = ' ')

df <- merge( df, port_depth, by = c('plot', 'period', 'port') ) 

saveRDS(df , 'data/processed_data/decagon_data.RDS')

