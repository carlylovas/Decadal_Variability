## Testing significance of difference of means pre- and post- 2010 with 2017 removed
## load libraries 
library(here)
library(tidyverse)
library(broom)
library(gmRi)

annual_averages <- read_rds("Processed Data/annual_averages.rds") %>% 
  filter(!est_year == "2017") %>% # Removed due to disproportionate sampling
  mutate(group = ifelse(est_year < 2010, 
                        "1970-2009", 
                        "2010-2019")) %>%
  group_by(comname) %>% 
  nest()

# T-Testing ####
all_strata_t_test <- annual_averages %>%
  unnest(data) %>% 
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

# Write out for plotting ####
write_rds(all_strata_t_test, here("Processed Data", "all_strata_t_test.rds"))
