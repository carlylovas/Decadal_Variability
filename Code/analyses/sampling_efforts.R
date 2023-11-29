# Comparing sampling efforts across 3 time periods
# 1970-2009, 2010-2019, and 2010-2019 excluding 2017
## load libraries 
library(here)
library(tidyverse)
library(gmRi)

# Pull unique tows from bottom trawl survey ####
get_survdat_tows <- function(survdat_clean){
  survdat_tows <- survdat_clean %>%
    dplyr::distinct(id, est_towdate, est_year, est_month, est_day, season, svvessel, 
                    decdeg_beglat, decdeg_beglon, survey_area, avgdepth, 
                    surftemp, surfsalin, bottemp, botsalin) %>%
    dplyr::filter(!is.na(decdeg_beglat) & !is.na(decdeg_beglon))
  # Return it
  return(survdat_tows)}

# Grab survey data ####
survey_data <- gmri_survdat_prep(
  survdat_source ="most recent",
  box_location ="cloudstorage") 

survey_tows <- get_survdat_tows(survey_data)

# Define groups for comparison ####
group1 <- survey_tows %>%
  select(svvessel, est_year, decdeg_beglat, decdeg_beglon, surftemp, bottemp, avgdepth) %>%
  filter(est_year %in% seq("1970", "2009")) %>%
  mutate(group = "1970-2009") %>%
  select(!c(svvessel)) %>%
  group_by(group) %>%
  nest(lat   = decdeg_beglat,
       lon   = decdeg_beglon,
       depth = avgdepth,
       sst   = surftemp,
       bt    = bottemp)

group2 <-  survey_tows %>%
  select(svvessel, est_year, decdeg_beglat, decdeg_beglon, surftemp, bottemp, avgdepth) %>%
  filter(est_year %in% seq("2010", "2019")) %>%
  mutate(group = "2010-2019") %>%
  select(!c(svvessel)) %>%
  group_by(group) %>%
  nest(lat   = decdeg_beglat,
       lon   = decdeg_beglon,
       depth = avgdepth,
       sst   = surftemp,
       bt    = bottemp)

group3 <-  survey_tows %>%
  select(svvessel, est_year, decdeg_beglat, decdeg_beglon, surftemp, bottemp, avgdepth) %>%
  filter(est_year %in% seq("2010","2019")) %>%
  filter(!est_year == "2017") %>%
  mutate(group = "!2017") %>%
  select(!c(svvessel)) %>%
  group_by(group) %>%
  nest(lat   = decdeg_beglat,
       lon   = decdeg_beglon,
       depth = avgdepth,
       sst   = surftemp,
       bt    = bottemp)

sampling_efforts <- group1 %>%
  rbind(group2) %>%
  rbind(group3) %>%
  group_by(group) %>%
  nest()

# Save out for plotting 
write_rds(sampling_efforts, here("Processed Data", "sampling_efforts.RDS"))
