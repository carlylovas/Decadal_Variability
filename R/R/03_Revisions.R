###Post-draft revisions
#load in clean_survey

rev_data<-weighted_data%>%
  filter(comname %in% c("american plaice",
                        "atlantic cod",
                        "atlantic herring",
                        "black sea bass",
                        "butterfish",
                        "haddock",
                        "pollock",
                        "scup",
                        "silver hake",
                        "white hake",
                        "winter flounder",
                        "yellowtail flounder"))%>%
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

#searobin_p<-welch.T.revised%>%
  #select(welch_lat_p, welch_lon_p, welch_sst_p, welch_bt_p, welch_depth_p)
#write.csv(searobin_p, "searobin_p_values.csv")

##means
rev_means<-rev_t_test%>%
  group_by(comname, group)%>%
  nest()%>%
  mutate(mean_sst   = as.numeric(map(data, avg_sst)),
         mean_bt    = as.numeric(map(data, avg_bt)),
         mean_depth = as.numeric(map(data, mean_depth)),
         mean_lat   = as.numeric(map(data, mean_lat)),
         mean_lon   = as.numeric(map(data, mean_lon)))

#searobin_means<-searobin_means%>%
  #select(!data)
#write.csv(searobin_means, "searobin_means.csv")

##Richard's revisions
grouped_data<-weighted_data%>%
  group_by(comname)%>%
  nest()%>%
  mutate(num_obs = map_dbl(data, count))%>%
  arrange(desc(num_obs))%>%
  rowid_to_column()

well_rep<-grouped_data%>%
  filter(rowid %in% seq(1,66))%>%
  unnest(data)%>%
  select(!rowid)%>%
  group_by(comname, season)%>%
  nest()%>%
  rowid_to_column()%>%
  mutate(num_obs = map_dbl(data, count))%>%
  select(!data)
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

##most observed species per season 
top_spp<-weighted_data%>%
  filter(comname %in% all_spp$comname)%>%
  group_by(comname, season)%>%
  nest()%>%
  mutate(num_obs = map_dbl(data, count))%>%
  select(!data)

write.csv(top_spp, "top_species.csv")

#total observations by tow 
grouped_survey<-clean_survey%>%
  group_by(comname, season)%>%
  distinct()%>%
  nest()%>%
  mutate(num_obs = unique(map_dbl(data, count)))%>%
  arrange(desc(num_obs))

##missing years 
missing_years<-grouped_data%>%
  filter(rowid %in% seq(58,66))%>%
  unnest(data)%>%
  select(comname, season, est_year)%>%
  group_by(comname, season)%>%
  nest()%>%
  mutate(season_obs = map_dbl(data, count),
         missed_years = map(data, missed_years))

missed_years<-function(x){
    setdiff(1970:2019, x$est_year)
}

missed_years_data<-missing_years%>%
  select(!data)

write.matrix(missed_years_data, "missing_years.csv", sep=",")

##catch abundance
biomass<-function(df){
  sum(df$biomass_kg)
}
abundance<-function(df){
  sum(df$abundance)
}

abd<-clean_survey%>%
  filter(comname %in% well_rep$comname)%>%
  select(tow, season, est_year, comname)%>%
  distinct()%>%
  group_by(comname)%>%
  nest()%>%
  mutate(num_tows = map_dbl(data, count))

season_abd<-clean_survey%>%
  filter(comname %in% well_rep$comname)%>%
  select(season, est_year, comname, abundance)%>%
  group_by(comname, season)%>%
  nest()%>%
  mutate(total_catch = map_dbl(data, abundance),
         total_tows = map_dbl(data, count))%>%
  arrange(comname)

##bluefish
bluefish<-clean_survey%>%
  filter(comname == "bluefish")%>%
  select(season, est_year, comname, abundance)%>%
  group_by(comname, season)%>%
  nest()%>%
  mutate(total_catch = map_dbl(data, abundance),
         total_tows = map_dbl(data, count))%>%
  select(!data)

bluefish_missing<-grouped_data%>%
  filter(comname == "bluefish")%>%
  unnest(data)%>%
  select(comname, season, est_year)%>%
  group_by(comname, season)%>%
  nest()%>%
  mutate(season_obs = map_dbl(data, count),
         missed_years = map(data, missed_years))%>%
  select(!data)

bluefish<-bluefish%>%
  left_join(bluefish_missing)%>%
  relocate(comname, .before=season)

write.matrix(bluefish, "bluefish.csv", sep=",")

##time series comparison
abd_species<-well_rep%>%
  filter(rowid %in% seq(1,132))%>%
  select(!rowid)

abd_species<-season_abd%>%
  filter(comname %in% abd_species$comname)%>%
  unnest(data)%>%
  group_by(comname, season, total_catch, total_tows)%>% 
  nest()

abnd_species <- abd_species%>%
  relocate(season, .after = comname)%>%
  select(!data)
write.csv(abnd_species, "species_abundance.csv")

nrow(abnd_species)
abnd<-vector("list", length = 24)
names(abnd)=paste(unique(abd_species$comname))

for(i in 1:24){
  print(i)
  loop_df<-abnd_species[i,]%>%
    unnest(data)%>%
    group_by(comname, est_year, season)%>%
    nest()%>%
    mutate(total =map_dbl(data, count))
  
  abnd[[i]]<-ggplot(data=loop_df, aes(x=est_year, y=total))+
                  geom_line(color = "#E9E9E9", linewidth = 0.5)+
                  geom_point(size=0.5)+
                  theme_gmri(plot.title = element_text(size = 11),
                             axis.text.y = element_text(size=10))+
                  ggtitle(toupper(names(abnd)[i]))+
                  xlab("Year")+
                  ylab("Total Catch Abundance")+
                  facet_wrap(~season, nrow=2, scales = "free_y")
}

install.packages("patchwork")
library(patchwork)

abnd_list<-abnd[c(1:24)]
abnd_1 <- wrap_plots(abnd[1:8])
print(abnd_1)

abnd_2 <- wrap_plots(abnd[9:16], ncol = 2, nrow = 4, heights = 4, widths = 3)
abnd_3 <- wrap_plots(abnd[17:24], ncol = 2, nrow = 4, heights = 4, widths = 3)
ggsave("abnd_1.pdf", abnd_1, height = 11, width = 8.5, units = "in")
ggsave("abnd_2.pdf", abnd_2, height = 11, width = 8.5, units = "in")
ggsave("abnd_3.pdf", abnd_3, height = 11, width = 8.5, units = "in")

#abnd_time_series<-marrangeGrob(abnd_list, nrow=4, ncol=2, top=NULL)
#ggsave("abnd_time_series.pdf", abnd_time_series, height = 11, width = 8.5, units = "in")

##filter by growth/productivity species
#re-run decadal plots with rev_data
#jk Kathy says no
#re-run rev_data code with new species list 
species_list<-read_csv("Data/species_list_final.csv")

rev_data<-weighted_data%>%
  filter(comname %in% species_list$comname)%>% 
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
  relocate(welch_lon_p, .after=bart_lon_p) #%>%
  mutate(across(where(is.numeric), round, 3))

##means
rev_means<-rev_t_test%>%
  group_by(comname, group)%>%
  nest()%>%
  mutate(mean_sst   = as.numeric(map(data, avg_sst)),
         mean_bt    = as.numeric(map(data, avg_bt)),
         mean_depth = as.numeric(map(data, mean_depth)),
         mean_lat   = as.numeric(map(data, mean_lat)),
         mean_lon   = as.numeric(map(data, mean_lon)))
group_1_rev<-rev_means%>%
  filter(group == "group_1")%>%
  select(!data)
group_2_rev<-rev_means%>%
  filter(group == "group_2")%>%
  select(!data)

#revised CSVs
write.csv(group_1_rev, "group_1_rev.csv")
write.csv(group_2_rev, "group_2_rev.csv")
write.matrix(welch.T.revised, "revised_welch.csv", sep=",")

##for plotting
saveRDS(rev_data, file = here("Data", "revised_plot_data.rds"))
