# Comparing the significance changes when stratum 16 is removed
library(here)
library(tidyverse)
library(gmRi)
library(stringi)

# Grab survey data ####
clean_survey<-gmri_survdat_prep(
  survdat_source ="most recent",
  box_location ="cloudstorage") %>%
  distinct(est_year, survey_area, stratum, tow, est_towdate, season, comname, catchsex, .keep_all = T) %>%
  group_by(est_year, survey_area, stratum, tow, est_towdate, season, 
           avgdepth, surftemp, bottemp, decdeg_beglat, decdeg_beglon, comname, abundance) %>% 
  summarise(biomass_kg = sum(biomass_kg, na.rm = T), .groups = "drop")


# Re-run species distributions without stratum 16 (1160) ####
survey_no16 <- clean_survey %>%
  filter(!stratum == "1160")

strata16_data <- grouped_center_bio(survey_no16, est_year, season) %>%
  mutate(decade = 10*est_year %/% 10)

annual_averages <- strata16_data %>%
  select(comname, est_year, decade, season, avg_depth, avg_bot_temp, avg_sur_temp, avg_lat, avg_lon) %>%
  filter(!est_year == 2017) %>% # Removed due to disproportionate sampling
  group_by(comname, est_year) %>%
  nest() %>%
  mutate( avg_depth       = map_dbl(data, function(x){mean(x$avg_depth,    na.rm = T)}),
          avg_bot_temp    = map_dbl(data, function(x){mean(x$avg_bot_temp, na.rm = T)}),
          avg_sur_temp    = map_dbl(data, function(x){mean(x$avg_sur_temp, na.rm = T)}),
          avg_lat         = map_dbl(data, function(x){mean(x$avg_lat,      na.rm = T)}),
          avg_lon         = map_dbl(data, function(x){mean(x$avg_lon,      na.rm = T)})) %>%
  group_by(comname) %>%
  nest() %>%
  filter(comname %in% species$comname)

# Revised T testing
t_test_revised <- annual_averages %>%
  unnest(data) %>%
  mutate(group = ifelse(est_year < 2010, 
                        "1970-2009", 
                        "2010-2019")) %>%
  select(!data) %>%
  pivot_longer(cols = 3:7, names_to = "variable", values_to = "measurement") %>%
  group_by(comname, variable) %>%
  nest() %>%
  mutate(t.test = map(data, function(x){
    t.test(measurement ~ group, data = x) %>% 
      tidy() %>% 
      select(
        estimate1970to2009 = estimate1, 
        estimate2010to2019 = estimate2, 
        method, 
        p.value) %>% 
      mutate(different = ifelse(p.value <= 0.05, T, F))
  }))

# Write out for comparison ####
write_rds(t_test_revised, here("Processed Data", "no_strata16_t_test.rds"))

rm(list=ls())
