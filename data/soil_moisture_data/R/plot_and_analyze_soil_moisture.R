###############################################################################
# 
# Run all scripts for plotting and analysis of soil moisture data  
# 
###############################################################################


home <- '~'
setwd( file.path(home, 'driversdata', 'data', 'idaho_modern', 'soil_moisture_data'))

if(!dir.exists('figures')){ dir.create('figures')}

# - plotting and analysis ---------------------------------------------------------

source('R/plot_raw_decagon_data.R')

source('R/plot_corrected_decagon_data.R')

source('R/compare_Decagon_to_climate_station.R')

source('R/shelter_and_irrigation_efficiency.R')

source('R/compare_logger_to_spot.R')

source('R/analyze_decagon_differences.R')

