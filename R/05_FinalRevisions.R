## Addressing reviewers comments on strata sampling efforts
# read in annual_averages <- read.csv("Data/decadal_averages.csv")
annual_averages <- annual_averages %>% 
  filter(!est_year == "2017") %>%
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
  #filter(!est_year == "2017") %>%
  #filter(!est_year == "2018") %>%
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
  mutate(stratum = stri_sub(stratum, 2,3)) %>%
  ggplot() +
  geom_line(aes(x = est_year, y = strata_effort_ratio, group = stratum, color = as.factor(stratum)), linewidth = 1.5) + 
  ylim(c(0.000, 0.100)) +
  guides(col = guide_legend(title = "Stratum", nrow = 4, byrow = TRUE)) +
  theme_gmri(legend.title = element_text(size = 20, face = "bold"),
             legend.text  = element_text(size = 20),
             legend.position = "bottom",
             axis.title   = element_text(size = 25, face = "bold"),
             axis.text    = element_text(size = 25))+
  xlab("Year") +
  ylab("Proportion of Annual Tows") +
  scale_color_gmri()

ggsave("all_strata_plot.png", all_strata_plot, height = 15, width = 15, units = "in", bg = "white")

avg_strata_plot <- strata_effort %>%
  unnest(data) %>%
  mutate(stratum = stri_sub(stratum, 2,3)) %>%
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

# Histograms #### 
write.csv(survey_tows, "survey_tows.csv")
group1 <- survey_tows %>%
  filter(est_year %in% seq("1970", "2009")) %>%
  ggplot() +
  geom_histogram(aes(x=decdeg_beglat), color = "white", fill = "#00608A")+
  ggtitle("1970-2009") +
  ylab("Count") +
  xlab("Latitude") + 
  theme_gmri()

group2 <-survey_tows %>%
  filter(est_year %in% seq("2010", "2019")) %>%
  ggplot() +
  geom_histogram(aes(x=decdeg_beglat))+
  ggtitle("2010-2019")

group2_no17 <- survey_tows %>%
  filter(est_year %in% seq("2010", "2019")) %>%
  filter(!est_year == "2017") %>%
  ggplot() +
  geom_histogram(aes(x=decdeg_beglat)) +
  ggtitle("2010-2016, 2018-2019")

group2_no17_18 <- survey_tows %>%
  filter(est_year %in% seq("2010", "2019")) %>%
  filter(!est_year == c("2017", "2018")) %>%
  ggplot() +
  geom_histogram(aes(x=decdeg_beglat)) +
  ggtitle("2010-2016, 2019")

plot <- (ggarrange(group1, group2, group2_no17, group2_no17_18, ncol = 2, nrow =2))
plot <- annotate_figure(plot, top = text_grob("Latitude"))

# plotting loop ?
group1 <- survey_tows %>%
  select(svvessel, est_year, decdeg_beglat, decdeg_beglon, surftemp, bottemp, avgdepth) %>%
  filter(est_year %in% seq("1970", "2009")) %>%
  pivot_longer(cols = 3:7, names_to = "variable", values_to = "measurement") %>%
  ggplot(aes(x = measurement)) +
  geom_histogram(aes(y = after_stat(density)), color = "white") +
  facet_wrap(~variable, scales = "free_x") +
  ggtitle("1970-2009")

group2 <- survey_tows %>%
  select(svvessel, est_year, decdeg_beglat, decdeg_beglon, surftemp, bottemp, avgdepth) %>%
  filter(est_year %in% seq("2010", "2019")) %>%
  pivot_longer(cols = 3:7, names_to = "variable", values_to = "measurement") %>%
  ggplot(aes(x = measurement)) +
  geom_histogram(aes(y = after_stat(density)), color = "white") +
  facet_wrap(~variable, scales = "free_x") +
  ggtitle("2010-2019")

group2_no17 <- survey_tows %>%
  select(svvessel, est_year, decdeg_beglat, decdeg_beglon, surftemp, bottemp, avgdepth) %>%
  filter(est_year %in% seq("2010", "2019")) %>%
  filter(!est_year == "2017") %>%
  pivot_longer(cols = 3:7, names_to = "variable", values_to = "measurement") %>%
  ggplot(aes(x = measurement)) +
  geom_histogram(aes(y = after_stat(density)), color = "white") +
  facet_wrap(~variable, scales = "free_x") +
  ggtitle("2010-2016, 2018-2019")

group2_no17_18 <- survey_tows %>%
  select(svvessel, est_year, decdeg_beglat, decdeg_beglon, surftemp, bottemp, avgdepth) %>%
  filter(est_year %in% seq("2010", "2019")) %>%
  filter(!est_year %in% c("2017", "2018")) %>%
  pivot_longer(cols = 3:7, names_to = "variable", values_to = "measurement") %>%
  ggplot(aes(x = measurement)) +
  geom_histogram(aes(y = after_stat(density)), color = "white") +
  facet_wrap(~variable, scales = "free_x") +
  ggtitle("2010-2016, 2019")

print(group1)
print(group2)
print(group2_no17)
print(group2_no17_18)

ggsave("1970-2009_hist.png", group1)
ggsave("2010-2019_hist.png", group2)
ggsave("2010-2019_no17_hist.png", group2_no17)
ggsave("2010-2019_no17-18_hist.png", group2_no17_18)

# hist()
survey_tows %>%
  select(svvessel, est_year, decdeg_beglat, decdeg_beglon, surftemp, bottemp, avgdepth) %>%
  filter(est_year %in% seq("2010", "2019")) %>%
  filter(!est_year %in% c("2017", "2018")) %>%
  pivot_longer(cols = 3:7, names_to = "variable", values_to = "measurement") %>%
  ggplot(aes(x = measurement)) +
  geom_histogram(aes(y = after_stat(density)), color = "white") +
  facet_wrap(~variable, scales = "free") +
  ggtitle("2010-2016, 2019")

# create nested data frame to optimize t-testing and plotting
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

group4 <-  survey_tows %>%
  select(svvessel, est_year, decdeg_beglat, decdeg_beglon, surftemp, bottemp, avgdepth) %>%
  filter(est_year %in% seq("2010", "2019")) %>%
  filter(!est_year %in% c("2017", "2018")) %>%
  mutate(group = "!2017-2018") %>%
  select(!c(svvessel)) %>%
  group_by(group) %>%
  nest(lat   = decdeg_beglat,
       lon   = decdeg_beglon,
       depth = avgdepth,
       sst   = surftemp,
       bt    = bottemp)

df <- group1 %>%
  rbind(group2) %>%
  rbind(group3) %>%
  #rbind(group4) %>%
  group_by(group) %>%
  nest()

###########
# T testing 
# 1970-2009 vs 2010-2019 
t.test.1 <- df %>%
  filter(group %in% c("1970-2009", "2010-2019")) %>%
  unnest(data) %>%
  unnest(lat:bt) %>%
  pivot_longer(cols = 3:7, names_to = "variable", values_to = "measurement") %>% 
  group_by(variable) %>%
  nest() %>% 
  mutate(t.test = map(data, function(x){
    t.test(measurement ~ group, data = x) %>% 
      tidy() %>% 
      select(
        estimate1, 
        estimate2, 
        method, 
        p.value) %>% 
      mutate(different = ifelse(p.value <= 0.05, T, F))
  }))
  
t.test.1 %>% select(!data) %>%
  unnest(t.test)

# 1970-2009 vs 2010-2016, 2018-2019
t.test.2 <- df %>%
  filter(group %in% c("1970-2009", "!2017")) %>%
  unnest(data) %>%
  unnest(lat:bt) %>%
  pivot_longer(cols = 3:7, names_to = "variable", values_to = "measurement") %>% 
  group_by(variable) %>%
  nest %>%
  mutate(t.test = map(data, function(x){
    t.test(measurement ~ group, data = x) %>% 
      tidy() %>% 
      select(
        estimate1, 
        estimate2, 
        method, 
        p.value) %>% 
      mutate(different = ifelse(p.value <= 0.05, T, F))
  }))

t.test.2 %>%
  select(!data) %>%
  unnest(t.test)

# 1970-2009 vs 2010-2016, 2019
t.test.3 <- df %>%
  filter(group %in% c("1970-2009", "!2017-2018")) %>%
  unnest(data) %>%
  unnest(lat:bt) %>%
  pivot_longer(cols = 3:7, names_to = "variable", values_to = "measurement") %>% 
  group_by(variable) %>%
  nest() %>%
  mutate(t.test = map(data, function(x){
    t.test(measurement ~ group, data = x) %>% 
      tidy() %>% 
      select(
        estimate1, 
        estimate2, 
        method, 
        p.value) %>% 
      mutate(different = ifelse(p.value <= 0.05, T, F))
  }))

t.test.3 %>% 
  select(!data) %>%
  unnest(t.test)

## Re-run species distributions without stratum 16 (1160)
survey_no16 <- clean_survey %>%
  filter(!stratum == "1160")

strata16_data <- grouped_center_bio(survey_no16, est_year, season) %>%
  mutate(decade = 10*est_year %/% 10)

revised_averages <- strata16_data %>%
  select(comname, est_year, season, avg_depth, avg_bot_temp, avg_sur_temp, avg_lat, avg_lon)%>%
  filter(!est_year == 2017) %>%
  mutate(decade = 10*est_year %/% 10)%>%
  filter(comname %in% species_list$comname) %>%
  group_by(comname, est_year)%>%
  nest() %>%
  mutate( avg_depth       = as.numeric(map(data, possibly(mean_depth, NA))),
          avg_bot_temp    = as.numeric(map(data, possibly(avg_bt, NA))),
          avg_sur_temp    = as.numeric(map(data, possibly(avg_sst, NA))),
          avg_lat         = as.numeric(map(data, possibly(mean_lat, NA))),
          avg_lon         = as.numeric(map(data, possibly(mean_lon, NA)))) %>%
  group_by(comname)%>%
  nest()

# t.test with Adam's code because of the stupid tidyverse update 
t.test.revised <- revised_averages %>%
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

revised_no_stratum16 <- t.test.revised %>%
  unnest(t.test) %>%
  select(comname, variable, p.value, estimate1970to2009, estimate2010to2019) %>%
  rename("revised" = "p.value",
         "no_16_strata_1970_2009" = "estimate1970to2009",
         "no_16_strata_2010_2019" = "estimate2010to2019") %>%
  mutate(revised = as.numeric(revised))
 
# compare these with previous results 
all_strata_t.test <- annual_averages %>%
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

write_rds(all_strata_t.test, "all_strata.RDS")
  
t.test.comparison <- all_strata_t.test %>%
  unnest(t.test) %>%
  select(comname, variable, p.value, estimate1970to2009, estimate2010to2019) %>%
  rename("all_strata" = "p.value",
         "all_strata_1970_2009" = "estimate1970to2009",
         "all_strata_2010_2009" = "estimate2010to2019") %>%
  mutate(all_strata = as.numeric(all_strata)) %>%
  full_join(revised_no_stratum16) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  mutate(change = ifelse((revised == all_strata), "No", "Yes"),
         significant_change = ifelse((all_strata >= 0.05 & revised <= 0.05 | 
                               all_strata <= 0.05 & revised >= 0.05), "True", "False")) %>%
  relocate("all_strata", .after = "no_16_strata_2010_2019") %>%
  relocate("revised", .after = "all_strata")

# write out csv for Kathy
write.csv(t.test.comparison, "Temp_Results/CSVs/strata_t_test_comparison.csv")
write.csv((t.test.comparison %>%
  filter(significant_change == "True")), "Temp_Results/CSVs/significant_changers!17.csv")

# Histograms w/out strata 16
no_strat16_hist <- get_survdat_tows(clean_survey %>% filter(!stratum == "1160")) %>%
  filter(!est_year == "2017") %>%
  mutate(group = ifelse(est_year < 2010, 
                        "1970-2009", 
                        "2010-2019"))
no_strat16_hist <- no_strat16_hist %>%
  select(svvessel, est_year, decdeg_beglat, decdeg_beglon, surftemp, bottemp, avgdepth, group) %>%
  pivot_longer(cols = 3:7, names_to = "variable", values_to = "measurement") %>%
  group_by(variable) %>%
  nest() %>%
  mutate(plot = map2(data, variable, function(df, variable){
    ggplot(data = df, aes(x = measurement)) +
    geom_histogram(aes(y = after_stat(density)), color = "white") +
    facet_wrap(~group, scales = "free_x") +
    ggtitle(paste(variable))}))

no_strata_16_plot <- patchwork::wrap_plots(no_strat16_hist$plot[1:5], ncol = 2)
ggsave("no_stratum_16.png", no_strata_16_plot, width = 11, height = 8.5, units = "in")

# Histograms with 1160 but without 2017
all_strata_hist <- survey_tows %>%
  filter(!est_year == "2017") %>%
  mutate(group = ifelse(est_year < 2010, 
                        "1970-2009", 
                        "2010-2019")) %>%
  select(svvessel, est_year, decdeg_beglat, decdeg_beglon, surftemp, bottemp, avgdepth, group) %>%
  pivot_longer(cols = 3:7, names_to = "variable", values_to = "measurement") %>%
  group_by(variable) %>%
  nest() %>%
  mutate(plot = map2(data, variable, function(df, variable){
    ggplot(data = df, aes(x = measurement)) +
      geom_histogram(aes(y = after_stat(density)), color = "white") +
      facet_wrap(~group, scales = "free_x") +
      ggtitle(paste(variable))}))

all_strata_plot <- patchwork::wrap_plots(all_strata_hist$plot[1:5], ncol = 2)
ggsave("all_strata.png", all_strata_plot, width = 11, height = 8.5, units = "in")

# 3 panel histograms
group_labels <- c("1970-2009", "2010-2019", "2010-2019 (2017 removed)")
names(group_labels) <- c("1970-2009","2010-2019","!2017")

revised_histograms <- df %>%
  unnest(data) %>%
  unnest(lat:bt) %>% 
  rename("Latitude"            = "decdeg_beglat",
         "Longitude"           = "decdeg_beglon",
         "Depth (m)"           = "avgdepth",
         "Surface Temperature" = "surftemp",
         "Bottom Temperature"  = "bottemp") %>% 
  pivot_longer(cols = 3:7, names_to = "variable", values_to = "measurement") %>%
  group_by(variable) %>%
  nest() 

# test plot
revised_histograms %>% 
  filter(variable == "Latitude") %>%
  unnest(data) %>% 
  mutate(across(group, factor, levels=c("1970-2009","2010-2019","!2017"))) %>%
  ggplot(aes(x = measurement)) +
  geom_histogram(aes(y = after_stat(density)), color = "white", fill = "darkgray") +
  facet_wrap(~group, scales = "free_x", labeller = labeller(group = group_labels)) +
  theme_gmri(legend.position = "none",
             axis.title = element_text(size = 12, face = "bold"),
             strip.background = element_rect(fill = "#00608A"),
             strip.text = element_text(color = "white", face = "bold", size =11),
             panel.border = element_rect(fill = NA, linetype = 1, linewidth = 1, color = "lightgray")) +
  xlab(paste(revised_histograms$variable)) +
  ylab("Frequency") +
  ggtitle("Latitude")

# apply to all 
revised_histograms <- revised_histograms %>% 
  mutate(plot = map2(data, variable, function(df, variable){
    df <- df %>% mutate(across(group, factor, levels=c("1970-2009","2010-2019","!2017")))
    ggplot(data = df, aes(x = measurement)) +
      geom_histogram(aes(y = after_stat(density)), color = "white", fill = "darkgray") +
      facet_wrap(~group, labeller = labeller(group = group_labels)) +
      theme_gmri(legend.position = "none",
                 plot.title  = element_text(size = 15, face = "bold"),
                 #axis.title = element_text(size = 18, face = "bold"),
                 axis.title = element_blank(),
                 axis.text  = element_text(size = 15),
                 strip.background = element_rect(fill = "#00608A"),
                 strip.text = element_text(color = "white", face = "bold", size = 15),
                 panel.border = element_rect(fill = NA, linetype = 1, linewidth = 1, color = "lightgray")) +
      xlab(paste(variable)) +
      #ylab("Frequency (proportion)") +
      ggtitle(paste(variable))}))

revised_histograms$plot[1]
histograms <- revised_histograms$plot[1:5]

all_hists <- marrangeGrob(histograms, layout_matrix = matrix(1:5, nrow = 5, ncol = 1, byrow = TRUE), top = NULL, left = textGrob(
  expression(bold("Frequency (proportion)")), rot = 90,
  gp = gpar(col = "black", fontsize = 16)) )
ggsave("Temp_Results/Plots/All_Variables_Histograms.png", all_hists, width = 12.5, height = 15, units = "in", bg = "white")

all_hist_test <- patchwork::wrap_plots(histograms, ncol = 1)
all_hi