################################################################################
##  Script to read in and format data for use in all analysis scripts. The raw
##  data are:
##    1. permanent_plots_estimated_biomass.RDS -- these estimates are produced by
##       the R script 01_calibrate_radiometer_by_year.R, and represent estimated
##       biomass (annual net primary productivity, ANPP) in each plot in each
##       year.
##    2. ClimateIPM.csv -- this is aggregated daily climate data from the MET
##       station in Dubois, ID, near the USSES. We only use the variable 'ppt1',
##       which is Fall-Spring precipitation for year t. E.g., if the year is 2012,
##       then ppt1 is Fall 2011 precip through Spring 2012 precip, cumulative.
##    3. SOILWAT_treatment_years.RDS -- these are model output from the SoilWat
##       model using parameters and precipitation data from the USSES. The estimates
##       are on daily time steps for the years of our treatment. We use these
##       estimates to fill in gaps where we lack observed soil moisture data.
##    4. average_seasonal_soil_moisture.csv -- This file contains observed and
##       statistically modeled daily soil moisture. See the manuscript file for
##       a description of our soil moisture data, the statistical model, and
##       our limited use of SoilWat estimates.
##
##  The main steps in this script are to summarize the soil moisture data into
##  yearly values and merge together the soil moisture and ANPP data for
##  statistical modeling.
##
## -----------------------------------------------------------------------------
##  Author: Andrew Tredennick
##  Date created: May 25, 2017
################################################################################

# THIS SCRIPT IS SOURCED FROM OTHER SCRIPTS THAT LOAD THE NECESSARY LIBRARIES.
# ALL OBJECTS SHOW UP IN GLOBAL ENVIRONMENT.

####
####  READ IN AND EXTRACT EXPERIMENT ANPP DATA ---------------------------------
####

# ANPP data
data_path              <- "../data/estimated_biomass/"
anpp_fname             <- "permanent_plots_estimated_biomass.RDS"
permanent_quad_biomass <- readRDS(paste0(data_path,anpp_fname))

# Weather station data
weather <- read.csv("../data/weather/ClimateIPM.csv") %>%
  select(-pptLag,-ppt2,-TmeanSpr1,-TmeanSpr2) # remove aggregates we don't use

# SoilWat estimates
soilwat <- readRDS("../data/soil_moisture_data/SOILWAT_treatment_years.RDS") %>%
  select(date, year, Treatment, VWC_raw, doy) %>%
  rename(julian_date = doy) %>%
  mutate(julian_date = as.integer(julian_date)) # rename and coerce doy for merging

# Observed and predicted soil moisture
soil_moisture <- read.csv("../data/soil_moisture_data/average_seasonal_soil_moisture.csv") %>%
  select(-year) %>% # remove what will become and extra year column
  separate(simple_date, c("year","month","day")) %>% # split up date col
  mutate(year = as.integer(year), # coerce for merging
         Treatment = as.character(Treatment)) %>% # coerce for merging
  filter(type=="predicted") %>% # just keep the statistically modeled VWC, more reliable than raw observations
  ungroup() %>%
  left_join(soilwat, by = c("year","Treatment","julian_date")) %>% # merge in SoilWat
  mutate(VWC_combo = ifelse(is.na(VWC)==TRUE, VWC_raw, VWC), # fill in NA gaps with SoilWat
         VWC_source = ifelse(is.na(VWC)==TRUE, "soilwat", "predicted")) %>%
  group_by(year, month, Treatment) %>%
  summarise(avg_vwc = mean(VWC_combo)) %>% # calc the mean by year, month, and treatment
  mutate(month_year = paste0(year,"-",month)) %>% # make a new col for IDs
  filter(month %in% c("03","04","05","06")) %>% # keep only March-June as growing season
  group_by(year, Treatment) %>%
  summarise(total_seasonal_vwc = sum(avg_vwc)) %>% # calc cumulative soil moisture over the months
  ungroup() %>%
  mutate(year = as.numeric(year)) # coerce for merging

# ANPP data; and merge in soil moisture
anpp_data <- permanent_quad_biomass %>% 
  filter(Treatment %in% c("Control","Drought","Irrigation")) %>% # exclude the removal treatments
  mutate(Treatment = as.character(Treatment)) %>% # coerce for merging
  filter(year > 2011) %>% # exclude years before treatment
  rename(anpp = biomass_grams_est) %>%
  left_join(weather, by = "year") %>% # add in precip data
  left_join(soil_moisture, by = c("year","Treatment")) %>% # add in soil moisture
  select(-QuadName,-quad,-Grazing,-paddock) %>% # remove extraneous information
  mutate(ppt1_scaled = as.numeric(scale(ppt1)), # scale precip
         vwc_scaled = as.numeric(scale(total_seasonal_vwc)), # scale soil moisture
         year_id = year - 2011) # make a year-of-treatment column

# Separate out the drought data
drought_data <- anpp_data %>%
  filter(Treatment != "Irrigation") %>%
  mutate(trt_id = as.numeric(as.factor(Treatment)) - 1)

# Separate out the irrigation data
irrigate_data <- anpp_data %>%
  filter(Treatment != "Drought") %>%
  mutate(trt_id = as.numeric(as.factor(Treatment)) - 1)





##########    OLD CODE     ##############
# ggplot(anpp_data, aes(total_seasonal_vwc, anpp, color=Treatment))+
#   geom_point()+
#   stat_smooth(se=F,method="lm")+
#   stat_smooth(se=F,color="black",method="lm")+
#   scale_color_brewer(palette = "Set2")



# soil_moisture2 <- read.csv("../data/soil_moisture_data/average_seasonal_soil_moisture.csv") %>%
#   dplyr::select(-year) %>%
#   separate(simple_date, c("year","month","day")) %>%
#   group_by(year,month,Treatment,type,year) %>%
#   summarise(avg_vwc = mean(VWC)) %>%
#   filter(type=="predicted") %>%
#   mutate(month_year = paste0(year,"-",month)) %>%
#   filter(month %in% c("03","04","05","06")) %>%
#   group_by(year,Treatment) %>%
#   summarise(total_seasonal_vwc = sum(avg_vwc)) %>%
#   ungroup() %>%
#   mutate(year = as.numeric(year))

