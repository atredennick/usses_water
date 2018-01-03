##  00_source_usses_water_scripts.R: script to source all code to repdroduce
##  analysis and results for Tredennick et al. This is the main code for the
##  'usses_water' repository.
##
##  Author: Andrew Tredennick (atredenn@gmail.com)
##  Date created: June 4, 2017


##  Clear the workspace
rm(list=ls(all.names = TRUE))

##  Set working directory to */usses_water/code/
# setwd(my/path/usses_water/code/)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # only for RStudio, sets to file location



####  1. LOAD NECESSARY PACKAGES ----
packages <- c(
  "tidyverse",
  "dplyr",
  "ggthemes",
  "ggalt",
  "ggjoy",
  "gridExtra",
  "stringr",
  "rstan",
  "vegan",
  "viridis",
  "RColorBrewer",
  "cowplot"
)

missing_packages <- packages[ !packages %in% installed.packages() ] 
if (length(missing_packages) > 0 ){
  stop(paste("You need to install the", missing_packages), " packages from CRAN.")
}

####  2. FIT ANPP-NDVI REGRESSIONS ----
source("01_calibrate_radiometer_by_year.R")

####  3. MAKE DATA FIGURE (Fig. 2) ----
source("02_plot_data.R")

####  4. FIT ANPP-PRECIP MODEL ----
source("03_anpp_randcoefs_model.R")

####  5. Sensitivity 
source("04_sensitivity_analysis.R")

####  6. PLOT MODEL RESULTS (Fig. 3, Table 1) ----
source("05_plot_model_results.R")

####  6. RUN NMDS ANALYSIS AND PLOTS (Fig. 4, Table 2) ----
source("06_community_dynamics.R")

####  7. FIT NDVI-PRECIP MODEL (Appendix 3) ----
source("07_ndvi_randcoefs_model.R")





