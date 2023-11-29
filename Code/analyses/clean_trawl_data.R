# Species Distributions ####
## load libraries 
library(here)
library(tidyverse)
library(gmRi)
library(matrixStats)

# Load NEFSC Bottom Trawl Survey data ####
clean_survey <- gmri_survdat_prep(
  survdat_source ="most recent",
  box_location ="cloudstorage"
)

clean_survey  <- clean_survey %>% 
  distinct(est_year, survey_area, stratum, tow, est_towdate, season, comname, catchsex, .keep_all = T) %>%
  group_by(est_year, survey_area, stratum, tow, est_towdate, season, 
           avgdepth, surftemp, bottemp, decdeg_beglat, decdeg_beglon, comname, abundance) %>% 
  summarise(biomass_kg = sum(biomass_kg, na.rm = T), .groups = "drop")

# Weight by biomass
grouped_center_bio <- function(clean_survey, ...){
  clean_survey %>% 
    group_by(comname, ...) %>% 
    summarise(
      # Un-weighted averages
      total_biomass   = sum(biomass_kg),
      avg_biomass     = mean(biomass_kg),
      biomass_sd      = sd(biomass_kg),
      # All below are weighted by biomass
      avg_depth       = weightedMean(avgdepth, w = biomass_kg, na.rm = T),
      avg_bot_temp    = weightedMean(bottemp, w = biomass_kg, na.rm = T),
      avg_sur_temp    = weightedMean(surftemp, w = biomass_kg, na.rm = T),
      avg_lat         = weightedMean(decdeg_beglat, w = biomass_kg, na.rm = T),
      avg_lon         = weightedMean(decdeg_beglon, w = biomass_kg, na.rm = T),
      depth_sd        = weightedSd(avgdepth, w = biomass_kg, na.rm = T),
      temp_sd         = weightedSd(bottemp, w = biomass_kg, na.rm = T),
      lat_sd          = weightedSd(decdeg_beglat, w = biomass_kg, na.rm = T),
      lon_sd          = weightedSd(decdeg_beglon, w = biomass_kg, na.rm = T),
      .groups = "drop") 
}

weighted_data <- grouped_center_bio(clean_survey, est_year, season) %>%
  mutate(decade = 10*est_year %/% 10)

# Filter out species of interest ####
species <- read_csv(here("Data","species_list_final.csv"))

# Averages ####
decadal_data <- weighted_data %>%
  select(comname, est_year, decade, season, avg_depth, avg_bot_temp, avg_sur_temp, avg_lat, avg_lon) %>%
  group_by(comname, season) %>%
  nest() %>%
  filter(comname %in% species$comname)

## Aggregate averages 
annual_averages <- decadal_data %>% 
unnest(data)%>%
  group_by(comname, est_year)%>%
  nest() %>%
  mutate( avg_depth       = map_dbl(data, function(x){mean(x$avg_depth,    na.rm = T)}),
          avg_bot_temp    = map_dbl(data, function(x){mean(x$avg_bot_temp, na.rm = T)}),
          avg_sur_temp    = map_dbl(data, function(x){mean(x$avg_sur_temp, na.rm = T)}),
          avg_lat         = map_dbl(data, function(x){mean(x$avg_lat,      na.rm = T)}),
          avg_lon         = map_dbl(data, function(x){mean(x$avg_lon,      na.rm = T)})) %>%
  group_by(comname)%>%
  select(!data)

write_rds(annual_averages, here("Processed Data", "annual_averages.rds"))

