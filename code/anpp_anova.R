permanent_quad_biomass <- readRDS("../data/estimated_biomass/permanent_plots_estimated_biomass.RDS")

permanent_quad_biomass <- permanent_quad_biomass %>% filter(Treatment %in% c("Control","Drought","Irrigation"))

biomass_yr_trt_summ <- permanent_quad_biomass %>%
  filter(!str_detect(quadname, 'P1|P7')) %>%
  group_by(Treatment,year) %>%
  filter(year > 2011)


library(nlme)

model = lm(log(biomass_grams_est) ~ Treatment, 
            data=filter(biomass_yr_trt_summ, year==2015))
summary(model)

library(car)

Anova(model)

