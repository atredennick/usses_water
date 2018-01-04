library(tidyverse)
library(dplyr)
library(lubridate)

soilwat <- read_csv("daily_SOILWAT_VWC_treatments.csv") %>%
  filter(between(year,2012,2017)) %>%
  mutate(doy = strftime(date, format = "%j"))

ggplot(soilwat, aes(x = as.numeric(doy), y = VWC_raw, color = Treatment))+
  geom_line()+
  facet_wrap(~year, ncol = 1)

saveRDS(object = soilwat, "../soil_moisture_data/SOILWAT_treatment_years.RDS")
