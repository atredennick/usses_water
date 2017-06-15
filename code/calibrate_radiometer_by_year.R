##  calibrate_radiometer_by_year.R: script to fit regressions between biomass
##  and NDVI for Idaho plots. Regressions are fit by year, and are then used
##  to estimate biomass from NDVI for the permanent plots.
##
##  Author: Andrew Tredennick
##  Email:  atredenn@gmail.com

##  Clear everything
rm(list=ls(all.names = TRUE))

##  Set path to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # only for RStudio

#### SCALING FACTORS ###########################################################
# recent years (2014 and beyond), biomass in 0.5m^2 plots (est biomass * 2)
# earlier years (2013 and earlier), biomass in 0.25m^2 (est biomass * 4)

####
####  FILE PATHS, OUTPUT PATHS, ETC. ----
####
data_cal_path  <- "../data/radiometer/calibration/"
data_quad_path <- "../data/radiometer/permanent_plots/"
out_path       <- "../data/estimated_biomass/"



####
####  PULL SOME PACKAGES OFF THE SHELF ----
####
library(tidyverse) # all sorts of data wrangling
library(dplyr)     # data summarizing/wrangling tools
library(broom)     # tidys up model output
library(stringr)   # tidy tools for strings



####
####  FUNCTIONS TO FIT AND OUTPUT CALIBRATION REGRESSSION ----
####
calculate_ndvi <- function(radiometer_data, col_starts = "ch", 
                           red_num = 2, nir_num = 4, 
                           sens_red = 0.96, sens_nir = 0.95, digits = 2){
  cols_i_want <- paste0(col_starts, c(red_num, nir_num))
  true_ones   <- cols_i_want %in% colnames(radiometer_data)
  
  if(length(which(true_ones == FALSE) > 0)) { 
    stop("can't find correct bands; check data and column names") 
  }
  
  col_ids     <- which((colnames(radiometer_data) %in% cols_i_want) == TRUE)
  band_mat    <- radiometer_data[,col_ids]
  b1          <- band_mat[,cols_i_want[2]]
  s1          <- sens_nir
  b2          <- band_mat[,cols_i_want[1]]
  s2          <- sens_red
  ndvi_vector <- (b1*s1 - b2*s2) / (b1*s1 + b2*s2)
  
  yearcheck <- which(colnames(radiometer_data) == "year")
  repcheck  <- which(colnames(radiometer_data) == "rep")
  if(length(yearcheck) != 1) { stop("no column named 'year' in data frame") }
  if(length(repcheck) != 1) { stop("no column named 'rep' in data frame") }
  
  ndvi_df <- data.frame(year = radiometer_data[,"year"],
                        rep  = radiometer_data[,"rep"],
                        ndvi = round(ndvi_vector, digits = digits))
  return(ndvi_df)
}

calculate_ndvi_quad <- function(radiometer_data, col_starts = "ch", 
                                red_num = 2, nir_num = 4, 
                                sens_red = 0.96, sens_nir = 0.95, digits = 2){
  cols_i_want <- paste0(col_starts, c(red_num, nir_num))
  true_ones   <- cols_i_want %in% colnames(radiometer_data)
  
  if(length(which(true_ones == FALSE) > 0)) { 
    stop("can't find correct bands; check data and column names") 
  }
  
  col_ids     <- which((colnames(radiometer_data) %in% cols_i_want) == TRUE)
  band_mat    <- radiometer_data[,col_ids]
  b1          <- band_mat[,cols_i_want[2]]
  s1          <- sens_nir
  b2          <- band_mat[,cols_i_want[1]]
  s2          <- sens_red
  ndvi_vector <- (b1*s1 - b2*s2) / (b1*s1 + b2*s2)
  
  yearcheck <- which(colnames(radiometer_data) == "year")
  quadcheck  <- which(colnames(radiometer_data) == "quad")
  if(length(yearcheck) != 1) { stop("no column named 'year' in data frame") }
  if(length(quadcheck) != 1) { stop("no column named 'quad' in data frame") }
  
  ndvi_df <- data.frame(year = radiometer_data[,"year"],
                        quad  = radiometer_data[,"quad"],
                        ndvi = round(ndvi_vector, digits = digits))
  return(ndvi_df)
}


get_regression <- function(ndvi_data, biomass_data, plots = FALSE){
  yearcheck <- which(colnames(ndvi_data) == "year")
  repcheck  <- which(colnames(ndvi_data) == "rep")
  if(length(yearcheck) != 1) { stop("no column named 'year' in ndvi data frame") }
  if(length(repcheck) != 1) { stop("no column named 'rep' in ndvi data frame") }
  
  yearcheck <- which(colnames(biomass_data) == "year")
  repcheck  <- which(colnames(biomass_data) == "rep")
  if(length(yearcheck) != 1) { stop("no column named 'year' in biomass data frame") }
  if(length(repcheck) != 1) { stop("no column named 'rep' in biomass data frame") }
  
  combo_data  <- dplyr::left_join(ndvi_data, biomass_data, by = c("year", "rep"))
  fit         <- lm(biomass_grams ~ ndvi, data = combo_data)
  fit_summary <- broom::tidy(fit)
  
  lmfit_df <- data.frame(year        = unique(combo_data$year),
                         intercept   = round(fit_summary[which(fit_summary$term=="(Intercept)"), "estimate"],2),
                         ndvi_slope  = round(fit_summary[which(fit_summary$term=="ndvi"), "estimate"],2),
                         rsquare     = round(glance(fit)$r.squared,2),
                         min_biomass = min(combo_data$biomass_grams),
                         max_biomass = max(combo_data$biomass_grams))
  
  if(plots){
    gg <- ggplot(combo_data, aes(x=ndvi, y=biomass_grams)) +
      geom_point()+
      stat_smooth(method="lm", se=FALSE)+
      ylab("biomass") +
      ggtitle(paste0("year: ", unique(combo_data$year)))
    
    return(list(lmfit_df, gg))
  }
  
  if(plots==FALSE) { return(lmfit_df) }
}

is_letter <- function(x) grepl("[[:alpha:]]", x)

check_reps <- function(x){
  ids <- which(is_letter(x$rep))
  if(length(ids) > 0) {
    x     <- x[-ids, ]
    x$rep <- as.integer(x$rep)
  }
  return(x)
}

estimate_biomass <- function(regression_params, ndvi_df){
  b0      <- regression_params$intercept
  b1      <- regression_params$ndvi_slope
  ndvi_df <- ndvi_df %>%
    mutate(biomass_grams_est = b0+b1*ndvi)
  return(ndvi_df)
}



####
####  LOOP OVER YEARS AND ESTIMATE REGRESSIONS ----
####
all_dirs  <- list.dirs(data_cal_path, full.names = FALSE, recursive = FALSE)
all_files <- list.files(data_cal_path)                # all files, includes directories
all_files <- all_files[-which(all_files == all_dirs)] # remove directories
all_files <- as.data.frame(all_files)
colnames(all_files) <- "file_name"

files_by_year  <- tidyr::separate(all_files, col = file_name, 
                                  into = c("year", "attribute"), sep = "_")
years_table    <- table(files_by_year$year)
missing_years  <- names(years_table)[which(years_table < 2)] # find years with 2 data frames (biomass and ndvi)
rows_to_remove <- which(files_by_year$year %in% missing_years)

if(length(rows_to_remove) != 0) {
  sub_files      <- as.character(all_files[-rows_to_remove, "file_name"])
  years_to_fit   <- unique(files_by_year[-rows_to_remove, "year"])
} else {
  sub_files      <- as.character(all_files[, "file_name"])
  years_to_fit   <- unique(files_by_year[, "year"])
}


all_year_params <- list()
for(i in 1:length(years_to_fit)){
  print(paste("Working on", years_to_fit[i]))
  do_year          <- years_to_fit[i]
  do_files         <- sub_files[grep(do_year, sub_files)]
  ndvi_file        <- do_files[grep("NDVI", do_files)]
  biomass_file     <- do_files[grep("biomass", do_files)]
  radiometer_data  <- read.csv(paste0(data_cal_path, ndvi_file))
  biomass_data     <- read.csv(paste0(data_cal_path, biomass_file))
  biomass_data     <- check_reps(biomass_data)
  modis_ndvi_data  <- calculate_ndvi(radiometer_data) # Defaults to MODIS calculation
  avhrr_ndvi_data  <- calculate_ndvi(radiometer_data,
                                     red_num = 1,
                                     nir_num = 3, 
                                     sens_red = 1, 
                                     sens_nir = 0.77) # Set args for AVHRR calculation
  modis_ndvi_data  <- check_reps(modis_ndvi_data) %>% filter(ndvi>0 | ndvi<1)
  avhrr_ndvi_data  <- check_reps(avhrr_ndvi_data) %>% filter(ndvi>0 | ndvi<1)
  modis_fit_ests   <- get_regression(modis_ndvi_data, biomass_data, plots=TRUE)
  avhrr_fit_ests   <- get_regression(avhrr_ndvi_data, biomass_data, plots=TRUE)
  rsquares         <- unlist(c(modis_fit_ests[[1]]["rsquare"], avhrr_fit_ests[[1]]["rsquare"]))
  touse            <- as.numeric(which(rsquares == max(rsquares)))
  
  ## Output
  if(touse == 1){
    modis_fit_ests[[1]]$algorithm <- "MODIS"
    all_year_params  <- rbind(all_year_params, modis_fit_ests[[1]])
    print(modis_fit_ests[[2]]) # spit out a plot
  }
  if(touse == 2){
    avhrr_fit_ests[[1]]$algorithm <- "AVHRR"
    all_year_params  <- rbind(all_year_params, avhrr_fit_ests[[1]])
    print(avhrr_fit_ests[[2]]) # spit out a plot
  }
}



####
####  USE REGRESSIONS TO ESTIMATE BIOMASS OF EACH PLOT ----
####
all_dirs  <- list.dirs(data_quad_path, full.names = FALSE, recursive = FALSE)
all_files <- list.files(data_quad_path)                # all files, includes directories
all_files <- all_files[-which(all_files == all_dirs)] # remove directories
all_files <- as.data.frame(all_files)
colnames(all_files) <- "file_name"

files_by_year  <- all_files %>%
  separate(col = file_name, into = c("x1", "x2", "year"), sep = "_") %>%
  separate(col = year, into = c("year", "extra"), sep = "\\.")
years_to_estimate  <- files_by_year$year
all_files          <- as.character(all_files$file_name)
fitted_years       <- unique(all_year_params$year)
years_can_estimate <- years_to_estimate[which(years_to_estimate %in% fitted_years)]

all_quad_biomass <- list()
for(i in 1:length(years_can_estimate)){
  do_year        <- years_can_estimate[i]
  temp_params    <- subset(all_year_params, year==do_year)
  do_file        <- all_files[grep(do_year, all_files)]
  quad_ndvi_data <- read.csv(paste0(data_quad_path, do_file)) %>%
    group_by(year, quad) %>%
    summarise(ch1 = mean(ch1),
              ch2 = mean(ch2),
              ch3 = mean(ch3),
              ch4 = mean(ch4))
  
  if(temp_params$algorithm=="MODIS"){
    quad_ndvi <- calculate_ndvi_quad(as.data.frame(quad_ndvi_data))
  }
  if(temp_params$algorithm=="AVHRR"){
    quad_ndvi <- calculate_ndvi_quad(as.data.frame(quad_ndvi_data),
                                     red_num = 1,
                                     nir_num = 3,
                                     sens_red = 1,
                                     sens_nir = 0.77)
  }
  quad_biomass     <- estimate_biomass(temp_params, quad_ndvi)
  all_quad_biomass <- rbind(all_quad_biomass, quad_biomass)
}

##  Merge in plot name information
plot_info <- read.csv("../data/estimated_biomass/quad_info.csv") %>%
  mutate(quadname = gsub(" ","",QuadName))

suppressWarnings(
  permanent_quad_biomass <- left_join(plot_info, all_quad_biomass, by=c("quadname"="quad")) %>%
    arrange(year,Treatment,quad)
) # suppress warning about character coercing
permanent_quad_biomass[which(permanent_quad_biomass$year < 2014), "biomass_grams_est"] <- permanent_quad_biomass[which(permanent_quad_biomass$year < 2014), "biomass_grams_est"] * 4 # 0.25 m^2 plots
permanent_quad_biomass[which(permanent_quad_biomass$year > 2013), "biomass_grams_est"] <- permanent_quad_biomass[which(permanent_quad_biomass$year > 2013), "biomass_grams_est"] * 2 # 0.5 m^2 plots



####
####  SAVE OUTPUT ----
####
write.csv(all_year_params, paste0(out_path,"ndvi_biomass_regression_parameters.csv"))
saveRDS(permanent_quad_biomass, paste0(out_path,"permanent_plots_estimated_biomass.RDS"))
#write.csv(permanent_quad_biomass, paste0(out_path,"permanent_plots_estimated_biomass.csv"))



####
####  INITIAL PLOTS ----
####
# permanent_quad_biomass <- permanent_quad_biomass %>% filter(Treatment %in% c("Control","Drought","Irrigation"))
# biomass_year_treatment <- permanent_quad_biomass %>%
#   group_by(Treatment,year) %>%
#   summarise(mean_biomass = mean(biomass_grams_est))
# 
# 
# ggplot()+
#   geom_line(data=permanent_quad_biomass, aes(x=year, y=biomass_grams_est, color=Treatment, group=quad),alpha=0.3,size=0.5, na.rm = T)+
#   geom_line(data=biomass_year_treatment, aes(x=year, y=mean_biomass, color=Treatment),size=1, na.rm = T)+
#   geom_point(data=biomass_year_treatment, aes(x=year, y=mean_biomass, color=Treatment),size=3, na.rm = T)+
#   geom_point(data=biomass_year_treatment, aes(x=year, y=mean_biomass),color="grey35",shape=1,size=3, na.rm = T)+
#   scale_color_brewer(palette = "Set2")+
#   scale_x_continuous(breaks=c(2008:2016))+
#   ylab(expression(paste("Estimated Biomass (g ", m^-2,")")))+
#   xlab("Year")+
#   theme_few()+
#   theme(legend.position=c(.1,.82))
# ggsave("../figures/estimated_biomass.png", width = 6, height = 4, units = "in", dpi = 120)
# 
# 
# biomass_yr_trt_summ <- permanent_quad_biomass %>%
#   filter(!str_detect(quadname, 'P1|P7')) %>%
#   group_by(Treatment,year) %>%
#   summarise(mean_biomass = mean(biomass_grams_est),
#             sd_biomass = sd(biomass_grams_est)) %>%
#   filter(year > 2011)
# 
# ggplot(biomass_yr_trt_summ, aes(x=year, y=mean_biomass, color=Treatment))+
#   geom_line()+
#   geom_errorbar(aes(ymin=mean_biomass-sd_biomass, ymax=mean_biomass+sd_biomass), width=0.05)+
#   geom_point(color="white", size=3)+
#   geom_point()+
#   geom_point(color="grey35", shape=1)+
#   scale_color_brewer(palette = "Set2", name=NULL)+
#   scale_x_continuous(breaks=c(2011:2016))+
#   scale_y_continuous(breaks=seq(50,350,50))+
#   ylab(expression(paste("Estimated ANPP (g ", m^-2,")")))+
#   xlab("Year")+
#   theme_few()+
#   theme(legend.position=c(.2,.75))
# ggsave("../figures/anpp_trt_trend.png", width = 4, height = 3, units = "in", dpi = 120)
# 
# 
# # Get sample sizes
# samp_size <- permanent_quad_biomass %>%
#   filter(!str_detect(quadname, 'P1|P7')) %>%
#   group_by(Treatment,year) %>%
#   summarise(num_plots = length(biomass_grams_est))
