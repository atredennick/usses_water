# format for Caitlin Andrews and John Bradford:

rm(list = ls () )
library( dplyr ) 
library( tidyr )

if(!dir.exists('data/processed_data/weather_files')){ dir.create('data/processed_data/weather_files')}


station_dat <- read.csv('../climateData/USSES_climate.csv')
station_dat$date <- as.POSIXct( strptime( station_dat$DATE, format = '%Y%m%d', tz = 'MST')    )

station_dat <- station_dat %>% select( date, STATION, STATION_NAME, LATITUDE, LONGITUDE, ELEVATION, PRCP, SNWD, SNOW, TMAX, TMIN )  

station_dat <- station_dat %>% mutate( LONGITUDE = LONGITUDE[which.max(date)], LATITUDE = LATITUDE[ which.max(date )], ELEVATION = ELEVATION[ which.max(date )] )

head(station_dat)

station_dat$year <- strftime(station_dat$date, '%Y')
station_dat$DOY <- as.numeric( strftime( station_dat$date, '%j'))
station_dat$PPT <- station_dat$PRCP/10

station_dat %>% group_by(year)  %>% filter( year > 1980 , year < 2011, PPT >= 0 ) %>% summarise(AP = sum(PPT, na.rm = T)) %>% summarise( MAP = mean(AP))

df <- expand.grid( DOY = 1:365 , year = c(min(station_dat$year):max(station_dat$year)))

station_dat <- merge( df,station_dat, by = c('year', 'DOY'), all.x = T, all.y = T)

year_list <- split( station_dat[ , c('DOY', 'TMAX', 'TMIN', 'PPT') ], station_dat$year)

write_with_header <- function(x, file, header, f = write.table, ...){

  datafile <- file(file, open = 'wt')

  on.exit(close(datafile))

  if(!missing(header)) writeLines(header,con=datafile)

  f(x, datafile,...)
}

make_header <- function( prefix, df, station, year) { 
  
  paste0( '#', prefix, station, ' year = ', year, '\n#', 'DOY', ' ', 'Tmax(C)', ' ', 'Tmin(C)', ' ', 'PPT(cm)')  
  
}


for ( i in 1:length( year_list) ) { 
  
  temp_df <- year_list[[i]]
  temp_year <- names(year_list)[[i]]
  
  temp_fname <- file.path( 'data/processed_data/weather_files', paste0( 'weath.', temp_year) )
  temp_header <- make_header(prefix = 'weather for site ', df = temp_df, station = 'US Sheep Experiment Station', year = temp_year )
    
  write_with_header( x = temp_df, file = temp_fname, header = temp_header, f = write.table, sep = '\t', col.names = FALSE, row.names = FALSE)
  
}

