## Table 1 with all significant values for all species and variables
library(here)
library(tidyverse)
library(stringr)
library(gmRi)
library(gt)

# Read in all_strata RDS for ANOVA/Welch's t-test results
all_strata <- read_rds(here("Processed Data", "all_strata_t_test.rds")) %>% 
  unnest(t.test) %>%
  rename("mean70_09" = "estimate1970to2009",
         "mean10_19" = "estimate2010to2019") %>%
  mutate(comname = stringr::str_to_sentence(comname))

# Individual tables showing significant p-values and means ####
depth_gt <- all_strata %>%
  filter(p.value <= 0.05) %>%
  filter(variable == "avg_depth") %>%
  ungroup(variable) %>%
  select(comname, mean70_09, mean10_19, p.value) %>%
  rename("avgdepth1970" = "mean70_09",
         "avgdepth2010" = "mean10_19",
         "avgdepthp"    = "p.value")
sst_gt <-  all_strata %>%
  filter(p.value <= 0.05) %>%
  filter(variable == "avg_sur_temp") %>%
  ungroup(variable) %>%
  select(comname, mean70_09, mean10_19, p.value) %>%
  rename("avgsst1970" = "mean70_09",
         "avgsst2010" = "mean10_19",
         "avgsstp"    = "p.value")
bt_gt <- all_strata %>%
  filter(p.value <= 0.05) %>%
  filter(variable == "avg_bot_temp") %>%
  ungroup(variable) %>%
  select(comname, mean70_09, mean10_19, p.value) %>%
  rename("avgbot1970" = "mean70_09",
         "avgbot2010" = "mean10_19",
         "avgbotp"    = "p.value")
lat_gt <- all_strata %>%
  filter(p.value <= 0.05) %>%
  filter(variable == "avg_lat") %>%
  ungroup(variable) %>%
  select(comname, mean70_09, mean10_19, p.value) %>%
  rename("avglat1970" = "mean70_09",
         "avglat2010" = "mean10_19",
         "avglatp"    = "p.value")
lon_gt <- all_strata %>%
  filter(p.value <= 0.05) %>%
  filter(variable == "avg_lon") %>%
  ungroup(variable) %>%
  select(comname, mean70_09, mean10_19, p.value) %>%
  rename("avglon1970" = "mean70_09",
         "avglon2010" = "mean10_19",
         "avglonp"    = "p.value")

# Combine all ####
all_strata_gt <- lat_gt %>%
  full_join(lon_gt) %>%
  full_join(depth_gt) %>%
  full_join(sst_gt) %>%
  full_join(bt_gt) %>%
  mutate(comname = stringr::str_to_sentence(comname)) %>%
  arrange(comname)

# make gt table
tab <- all_strata_gt %>%
  gt(groupname_col = NULL) %>%
  tab_spanner(label = md("**Latitude (\u00B0N)**"), columns = c("avglat1970", "avglat2010", "avglatp")) %>%
  tab_spanner(label = md("**Longitude (\u00B0W)**"), columns = c("avglon1970", "avglon2010", "avglonp")) %>%
  tab_spanner(label = md("**Depth (m)**"), columns = c("avgdepth1970", "avgdepth2010", "avgdepthp")) %>%
  tab_spanner(label = md("**Surface Temperature (\u00B0C)**"), columns = c("avgsst1970", "avgsst2010", "avgsstp")) %>%
  tab_spanner(label = md("**Bottom Temperature (\u00B0C)**"), columns = c("avgbot1970", "avgbot2010", "avgbotp")) %>%
  cols_label(
    comname    = md("**Species**"),
    avglat1970 = md("*1970-2009*"),
    avglat2010 = md("*2010-2019*"),
    avglatp    = md("*p*"),
    avglon1970 = md("*1970-2009*"),
    avglon2010 = md("*2010-2019*"),
    avglonp    = md("*p*"),
    avgdepth1970 = md("*1970-2009*"),
    avgdepth2010 = md("*2010-2019*"),
    avgdepthp    = md("*p*"),
    avgsst1970 = md("*1970-2009*"),
    avgsst2010 = md("*2010-2019*"),
    avgsstp    = md("*p*"),
    avgbot1970 = md("*1970-2009*"),
    avgbot2010 = md("*2010-2019*"),
    avgbotp    = md("*p*")) %>%
  fmt_number(columns = c(avglatp, avglonp, avgdepthp, avgsstp, avgbotp), decimals = 3) %>%
  fmt_number(columns = c(avglat1970, avglat2010, avglon1970, avglon2010, avgdepth1970, avgdepth2010, avgsst1970, avgsst2010,
                         avgbot1970, avgbot2010), decimals = 2) %>%
  sub_missing(columns = everything(), rows = everything(), missing_text = "") %>%
  tab_footnote(footnote = md("*2017 excluded from 2010-2019 means*"))

gt::gtsave(tab, here("Figures", "Table_1.docx"))
