# Comparison of significance changes when stratum 16 is removed 
## load libraries 
library(here)
library(tidyverse)
library(broom)
library(gmRi)

all_strata_t_test  <- read_rds("Processed Data/all_strata_t_test.rds")
no_strata16_t_test <- read_rds("Processed Data/no_strata16_t_test.rds")

# Compare significance values across both datasets
t_test_comparison <- all_strata_t_test %>%
  unnest(t.test) %>%
  select(comname, variable, p.value, estimate1970to2009, estimate2010to2019) %>%
  rename("all_strata" = "p.value",
         "all_strata_1970_2009" = "estimate1970to2009",
         "all_strata_2010_2019" = "estimate2010to2019") %>% # rename for ease of understanding when comparing
  mutate(all_strata = as.numeric(all_strata)) %>% # p.value 
  full_join(no_strata16_t_test %>% unnest(t.test) %>% # combining t tests without stratum 16
              select(comname, variable, p.value, estimate1970to2009, estimate2010to2019) %>%
              rename("no_strata16" = "p.value",
                     "no_strata16_1970_2009" = "estimate1970to2009",
                     "no_strata16_2010_2019" = "estimate2010to2019") %>%
              mutate(no_strata16 = as.numeric(no_strata16))) %>%
  #mutate(across(where(is.numeric), round, 3)) %>%
  mutate(change = ifelse((no_strata16 == all_strata), "No", "Yes"), # are the significant values different?
         significant_change = ifelse((all_strata >= 0.05 & no_strata16 <= 0.05 | 
                                        all_strata <= 0.05 & no_strata16 >= 0.05), "True", "False")) %>% # is there a change in signficance?
  relocate("all_strata", .after = "no_strata16_2010_2019") %>%
  relocate("no_strata16", .after = "all_strata")

# Save out significant changers for [Table S1.1]
significant_changers <- t_test_comparison %>% filter(significant_change == "True")
write_rds(significant_changers, here("Processed Data", "significant_changers.rds"))

# Clear environment to begin plotting
rm(list=ls())
