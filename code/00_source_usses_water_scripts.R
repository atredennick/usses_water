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
  "gridExtra",
  "stringr",
  "rstan",
  "vegan",
  "viridis"
)

for(pack in 1:length(packages)){
  if(packages[pack] %in% rownames(installed.packages())==FALSE)
  {stop(paste("You need to install the", packages[pack]), " package from CRAN.")}
}

####  2. FIT ANPP-NDVI REGRESSIONS ----
source("calibrate_radiometer_by_year.R")

####  3. MAKE DATA FIGURE (Fig. 1) ----
source("look_at_weather.R")

####  4. FIT ANPP-PRECIP MODEL ----
source("anpp_randcoefs_model.R")

####  5. PLOT MODEL RESULTS (Figs. 2 and 3) ----
source("plot_model_results.R")

####  6. RUN NMDS ANALYSIS AND PLOTS (Figs. 4 and 5, Table 1) ----
source("community_dynamics.R")





