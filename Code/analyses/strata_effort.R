## Testing statistical significance of strata sampling efforts 
## load libraries 
library(here)
library(tidyverse)
library(gmRi)
library(stringi)

# Useful functions ####
get_survdat_tows <- function(survdat_clean){
  #### 1. Filter SURVDAT to unique tows and keep columns of interest   ####
  # Get unique tows
  survdat_tows <- survdat_clean %>%
    dplyr::distinct(id, est_towdate, est_year, est_month, est_day, season, svvessel, 
                    decdeg_beglat, decdeg_beglon, survey_area, avgdepth, 
                    surftemp, surfsalin, bottemp, botsalin) %>%
    dplyr::filter(!is.na(decdeg_beglat) & !is.na(decdeg_beglon))
  # Return it
  return(survdat_tows)}

# Grab survey data
clean_survey<-gmri_survdat_prep(
  survdat_source ="most recent",
  box_location ="cloudstorage"
)

# Review strata, regions & tows ####
unique(clean_survey$stratum) 
strata_check <- read_csv("Data/strata_check.csv") # groups strata by region

strata_effort <-read_csv("Data/strata_effort.csv", col_types = list(col_double())) %>%
  mutate_all(., as.numeric)

strata_effort <- strata_effort %>%
  pivot_longer(cols = 2:54, names_to = "stratum", values_to = "strata_effort_ratio") %>%
  group_by(stratum) %>%
  nest() %>%
  mutate(stratum = as.numeric(stratum))

# Join with regional groups 
strata_effort <- strata_effort %>% 
  left_join(strata_check) %>%
  unnest(data) %>% 
  drop_na() %>%
  filter(est_year %in% c(1970:2019)) %>% 
  mutate(group = ifelse(est_year < 2010, 
                        "1970-2009", 
                        "2010-2019")) %>% 
  rename("region" = 'general region') %>%
  group_by(region) %>%
  nest()

# T testing ####
strata_effort <- strata_effort %>%
  mutate(t.test = map(data, function(x){
    t.test(strata_effort_ratio ~ group, data = x) %>% 
      tidy() %>% 
      select(
        effort_fraction_1970to2009 = estimate1, 
        effort_fraction_2010to2019 = estimate2, 
        method, 
        p.value) %>% 
      mutate(different = ifelse(p.value <= 0.05, T, F))
  }))

# Save out for plotting ####
write_rds(strata_effort, here("Processed Data", "strata_effort.rds"))

# Clear environment ####
rm(list=ls())