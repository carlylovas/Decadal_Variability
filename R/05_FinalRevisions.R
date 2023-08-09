## Addressing reviewers comments on strata sampling efforts
# read in annual_averages <- read.csv("Data/decadal_averages.csv")
annual_averages <- annual_averages %>% 
  mutate(group = ifelse(est_year < 2010, 
                        "1970-2009", 
                        "2010-2019")) %>%
  group_by(comname) %>% 
  nest()

# distinct strata
unique(clean_survey$stratum) 
stratum <- clean_survey %>%
  select(stratum) %>%
  mutate(stratum = as.numeric(stratum)) %>%
  arrange(stratum) %>%
  distinct()

strata_check <- read_csv("Data/strata_check.csv")

# read in Kathy/Adam's strata effort
strata_effort <-read_csv("Data/strata_effort.csv", col_types = list(col_double())) %>%
  mutate_all(., as.numeric)

strata_effort <- strata_effort %>%
  pivot_longer(cols = 2:54, names_to = "stratum", values_to = "strata_effort_ratio") %>%
  group_by(stratum) %>%
  nest() %>%
  mutate(stratum = as.numeric(stratum))

# join with regional groups and remove 2017
strata_effort <- strata_effort %>% 
  left_join(strata_check) %>%
  unnest(data) %>% 
  drop_na() %>%
  filter(!est_year == "2017") %>%
  filter(!est_year == "2018") %>%
  filter(est_year %in% c(1970:2019)) %>% 
  mutate(group = ifelse(est_year < 2010, 
                         "1970-2009", 
                         "2010-2019")) %>% 
  rename("region" = 'general region') %>%
  group_by(region) %>%
  nest()

# T testing 
strata_effort <- strata_effort %>%
  #split(.$stratum) %>% 
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

library(gt)
library(webshot2)

gtsave((strata_effort %>% 
  select(!data) %>%
  unnest(t.test) %>% 
  gt(groupname_col = NULL) %>%
  cols_label(region = md("**Region**"),
             effort_fraction_1970to2009 = md("*1970-2009*"), 
             effort_fraction_2010to2019 = md("*2010-2019*"),
             method = md("**Method**"),
             p.value = md("**p value**"),
             different = md("**significance**")) %>%
  tab_spanner(label = md("**Average effort ratio**"), columns = c("effort_fraction_1970to2009", "effort_fraction_2010to2019"))), filename = "regional_means.png")

# Plots
all_strata_plot <- strata_effort %>%
  unnest(data) %>%
  unnest(t.test) %>%
  ggplot() +
  geom_line(aes(x = est_year, y = strata_effort_ratio, group = stratum, color = as.factor(stratum))) + 
  geom_segment(aes(x = 1970, xend = 2009, y = effort_fraction_1970to2009, yend = effort_fraction_1970to2009)) +
  geom_segment(aes(x = 2010, xend = 2019, y = effort_fraction_2010to2019, yend = effort_fraction_2010to2019)) +
  facet_wrap(~region) +
  theme_gmri() +
  scale_color_gmri()
ggsave("all_strata_plot.png", all_strata_plot, height = 8.5, width = 11, units = "in", bg = "white")

avg_strata_plot <- strata_effort %>%
  unnest(data) %>%
  drop_na() %>%
  group_by(region, est_year) %>%
  nest() %>%
  mutate(avg_effort_across_strata = map(data, function(x){
    mean(x$strata_effort_ratio)
  })) %>%
  mutate(avg_effort_across_strata = as.numeric(avg_effort_across_strata))%>%
  unnest(data) %>%
  unnest(t.test) %>%
  ggplot() +
  geom_line(aes(x = est_year, y = avg_effort_across_strata)) +
  geom_segment(aes(x = 1970, xend = 2009, y = effort_fraction_1970to2009, yend = effort_fraction_1970to2009), color = "#00608A") +
  geom_segment(aes(x = 2010, xend = 2019, y = effort_fraction_2010to2019, yend = effort_fraction_2010to2019), color = "#EA4F12") +
  facet_wrap(~region) +
  theme_gmri()
ggsave("avg_effort_plot.png", avg_strata_plot, height = 8.5, width = 11, units = "in", bg = "white")


# re-run t-test for all parameters
# trying Adam's code for center of lat

lat_test_data <- annual_averages %>%
  unnest(data) %>%
  select(comname, est_year, avg_lat, group)

lat_test_data %>%
  split(.$comname) %>% 
  map_dfr(function(x){
    t.test(avg_lat ~ group, data = x) %>% 
      tidy() %>% 
      select(
        lat_1970to2009 = estimate1, 
        lat_2010to2019 = estimate2, 
        method, 
        p.value) %>% 
      mutate(different = ifelse(p.value <= 0.05, T, F))
  }, .id = "comname")

# compare with original 
rev_t_test %>%
  select(comname, est_year, avg_lat, group) %>%
  split(.$comname) %>% 
  map_dfr(function(x){
    t.test(avg_lat ~ group, data = x) %>% 
      tidy() %>% 
      select(
        lat_1970to2009 = estimate1, 
        lat_2010to2019 = estimate2, 
        method, 
        p.value) %>% 
      mutate(different = ifelse(p.value <= 0.05, T, F))
  }, .id = "comname") # this matches 

# using original code to calculate quickly and comparatively (although messier) ####
revised_welch <- annual_averages %>%
mutate(num_obs      = map(data, possibly(count, NA)))%>%
  mutate(bart_sst     = map(data, possibly(bart_sst, NA)),
         bart_bt      = map(data, possibly(bart_bt, NA)),
         bart_depth   = map(data, possibly(bart_depth, NA)),
         bart_lat     = map(data, possibly(bart_lat, NA)),
         bart_lon     = map(data, possibly(bart_lon, NA)))%>%
  mutate(welch_sst    = map(data, possibly(welch_sst, NA)),
         welch_bt     = map(data, possibly(welch_bt,NA)),
         welch_depth  = map(data, possibly(welch_depth, NA)),
         welch_lat    = map(data, possibly(welch_lat, NA)),
         welch_lon    = map(data, possibly(welch_lon, NA)))%>%
  mutate(tidy_bart_sst     = map(bart_sst,   broom::tidy),
         tidy_bart_bt      = map(bart_bt,    broom::tidy),
         tidy_bart_depth   = map(bart_depth, broom::tidy),
         tidy_bart_lat     = map(bart_lat,   broom::tidy),
         tidy_bart_lon     = map(bart_lon,   broom::tidy))%>%
  mutate(tidy_welch_sst     = map(welch_sst,   broom::tidy),
         tidy_welch_bt      = map(welch_bt,    broom::tidy),
         tidy_welch_depth   = map(welch_depth, broom::tidy),
         tidy_welch_lat     = map(welch_lat,   broom::tidy),
         tidy_welch_lon     = map(welch_lon,   broom::tidy))%>%
  mutate(bart_sst_p        = map(tidy_bart_sst,   p),
         bart_bt_p         = map(tidy_bart_bt,    p),
         bart_depth_p      = map(tidy_bart_depth, p),
         bart_lat_p        = map(tidy_bart_lat,   p),
         bart_lon_p        = map(tidy_bart_lon,   p))%>%
  mutate(welch_sst_p       = map(tidy_welch_sst, p),
         welch_bt_p        = map(tidy_welch_bt, p),
         welch_depth_p     = map(tidy_welch_depth, p),
         welch_lat_p       = map(tidy_welch_lat, p),
         welch_lon_p       = map(tidy_welch_lon, p))%>%
  nest(bartlett_test  = c(bart_sst:bart_lon),
       welch_t_test   = c(welch_sst:welch_lon),
       tidy           = c(tidy_bart_sst:tidy_welch_lon),
       bart_p_values  = c(bart_sst_p:bart_lon_p),
       welch_p_values = c(welch_sst_p:welch_lon_p)) %>%
  unnest(c(bart_p_values, welch_p_values))%>%
  drop_na()%>%
  summarise(bart_sst_p        = as.numeric(bart_sst_p),
         welch_sst_p       = as.numeric(welch_sst_p),
         bart_bt_p         = as.numeric(bart_bt_p),
         welch_bt_p        = as.numeric(welch_bt_p),
         bart_depth_p      = as.numeric(bart_depth_p),
         welch_depth_p     = as.numeric(welch_depth_p),
         bart_lat_p        = as.numeric(bart_lat_p),
         welch_lat_p       = as.numeric(welch_lat_p),
         bart_lon_p        = as.numeric(bart_lon_p),
         welch_lon_p       = as.numeric(welch_lon_p))%>%
  relocate(welch_sst_p, .after =bart_sst_p)%>%
  relocate(welch_bt_p, .after=bart_bt_p)%>%
  relocate(welch_depth_p, .after=bart_depth_p)%>%
  relocate(welch_lat_p, .after=bart_lat_p)%>%
  relocate(welch_lon_p, .after=bart_lon_p) #%>%
  mutate(across(where(is.numeric), round, 3))

write_csv(revised_welch, "revised_ttest_values_no2017.csv") 

# means
means <- annual_averages %>% 
  unnest(data) %>%
  group_by(comname, group) %>%
  nest() %>%
  summarise(mean_sst   = as.numeric(map(data, avg_sst)),
       mean_bt    = as.numeric(map(data, avg_bt)),
       mean_depth = as.numeric(map(data, mean_depth)),
       mean_lat   = as.numeric(map(data, mean_lat)),
       mean_lon   = as.numeric(map(data, mean_lon)))
write_csv(means, "revised_means_no2017.csv")

# check for difference
check_old <- welch.T.revised %>%
  select(comname, welch_sst_p, welch_bt_p, welch_lat_p, welch_lon_p, welch_depth_p) %>%
  ungroup() %>%
  rename(sst   = welch_sst_p,
         bt    = welch_bt_p,
         depth = welch_depth_p,
         lat   = welch_lat_p,
         lon   = welch_lon_p) %>%
  mutate(version = "original")

check <- revised_welch %>% 
  select(comname, welch_sst_p, welch_bt_p, welch_lat_p, welch_lon_p, welch_depth_p) %>%
  ungroup() %>%
  rename(sst  = welch_sst_p,
         bt   = welch_bt_p,
         depth = welch_depth_p,
         lat   = welch_lat_p,
         lon   = welch_lon_p) %>%
  mutate(version = "revised")

check <- check %>%
  rbind(check_old) %>%
  pivot_longer(cols= 2:6, names_to = "variable", values_to = "p-value") %>%
  pivot_wider(names_from = "version", values_from = "p-value")

check <- check  %>%
  mutate(significant = ifelse((revised & original <= 0.05), T, F)) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  mutate(significant_change = (original >= 0.05 & revised <= 0.05 | original <= 0.05 & revised >= 0.05))
 #mutate(change = ifelse((revised != original), T, F))

changers <- check %>%
  filter(significant_change == "TRUE") %>%
  mutate(direction = ifelse((revised > original), "+", "-"))
write_csv(changers, "changers.csv")

change <- check %>%
  left_join(changers)
write_csv(change, "p_values_change.csv")

# changers and their means 
changers_means <- annual_averages %>%
  filter(comname %in% changers$comname) %>%
  unnest(data) %>%
  group_by(comname, group) %>%
  nest() %>%
  summarise(sst   = map(data, mean_sst),
            bt    = map(data, mean_bt),
            depth = map(data, mean_depth),
            lat   = map(data, mean_lat),
            lon   = map(data, mean_lon)) %>%
  pivot_longer(cols = sst:lon, names_to = "variable", values_to = "mean") %>%
  mutate(mean = as.numeric(mean)) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  pivot_wider(names_from = "group", values_from = "mean") %>%
  rename("group1" = "1970-2009",
         "group2" = "2010-2019") %>%
  mutate(difference = (group2 - group1),
         direction = ifelse((group1 < group2), "+", "-")) %>%
  rename("1970-2009" = "group1",
         "2010-2019" = "group2")
write_csv(changers_means, "changers_means.csv")

# read back in for directionality
changers_means <-read_csv("Temp_Results/Revised/changers_means.csv")

significant_changers <- changers %>%
  select(!direction) %>%
  full_join(changers_means) %>%
  filter(significant_change == "TRUE")
write_csv(significant_changers, "Temp_Results/Revised/significant_changers.csv")

significant_changers %>%
  filter(revised >= 0.05)
