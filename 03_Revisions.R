###Post-draft revisions
#load in clean_survey

rev_data<-weighted_data%>%
  filter(comname %in% c("northern searobin", "striped searobin"))%>%
  group_by(comname, est_year)%>%
  nest() %>%
  mutate( avg_depth       = as.numeric(map(data, possibly(mean_depth, NA))),
          avg_bot_temp    = as.numeric(map(data, possibly(avg_bt, NA))),
          avg_sur_temp    = as.numeric(map(data, possibly(avg_sst, NA))),
          avg_lat         = as.numeric(map(data, possibly(mean_lat, NA))),
          avg_lon         = as.numeric(map(data, possibly(mean_lon, NA))))%>%
  group_by(comname)%>%
  nest()

##T-TEST
sub_1<-rev_data%>%
  unnest(data)%>%
  filter(est_year %in% c(1970:2009))%>%
  mutate(group = as.character("group_1"))
sub_2<-rev_data%>%
  unnest(data)%>%
  filter(est_year %in% c(2010:2019))%>%
  mutate(group = as.character("group_2"))

rev_t_test<-bind_rows(sub_1, sub_2)

##Welch's T-Test
rev.stats<-rev_t_test%>%
  filter(group %in% c("group_1", "group_2"))%>%
  group_by(comname)%>%
  nest()%>%
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
       welch_p_values = c(welch_sst_p:welch_lon_p))

welch.T.revised<-rev.stats%>%
  select(comname, bart_p_values, welch_p_values, num_obs)%>%
  unnest(c(bart_p_values, welch_p_values))%>%
  drop_na()%>%
  mutate(bart_sst_p        = as.numeric(bart_sst_p),
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
  relocate(welch_lon_p, .after=bart_lon_p)%>%
  mutate(across(where(is.numeric), round, 3))

searobin_p<-welch.T.revised%>%
  select(welch_lat_p, welch_lon_p, welch_sst_p, welch_bt_p, welch_depth_p)
write.csv(searobin_p, "searobin_p_values.csv")

##means
searobin_means<-rev_t_test%>%
  group_by(comname, group)%>%
  nest()%>%
  mutate(mean_sst   = as.numeric(map(data, avg_sst)),
         mean_bt    = as.numeric(map(data, avg_bt)),
         mean_depth = as.numeric(map(data, mean_depth)),
         mean_lat   = as.numeric(map(data, mean_lat)),
         mean_lon   = as.numeric(map(data, mean_lon)))
searobin_means<-searobin_means%>%
  select(!data)
write.csv(searobin_means, "searobin_means.csv")

##Richard's revisions
grouped_data<-weighted_data%>%
  group_by(comname)%>%
  nest()%>%
  mutate(num_obs = map_dbl(data, count))%>%
  arrange(desc(num_obs))%>%
  rowid_to_column()

well_rep<-grouped_data%>%
  filter(rowid %in% seq(1,50))%>%
  select(!data)%>%
  select(!rowid)
write.csv(well_rep, "well_represented_spp.csv")

current_sp<-dec_data%>%
  unnest(data)%>%
  group_by(comname)%>%
  nest()%>%
  mutate(num_obs = map_dbl(data, count))%>%
  select(!data)

all_spp<-well_rep%>%
  rbind(current_sp)%>%
  distinct()%>%
  arrange(comname)

top_spp<-weighted_data%>%
  filter(comname %in% all_spp$comname)%>%
  group_by(comname, season)%>%
  nest()%>%
  mutate(num_obs = map_dbl(data, count))%>%
  select(!data)

write.csv(top_spp, "top_species.csv")

grouped_survey<-clean_survey%>%
  group_by(comname, season)%>%
  nest()%>%
  mutate(num_obs = map_dbl(data, count))%>%
  arrange(desc(num_obs))%>%
  rowid_to_column()%>%
  filter(rowid %in% seq(1,50))
  
