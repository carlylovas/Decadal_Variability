## Revised ANOVA table and plots
#  All distributions run without 2017 across all strata

# Read in all_strata RDS for ANOVA/Welch's t-test results
all_strata <- readRDS("Data/all_strata.RDS") %>% 
  unnest(t.test) %>%
  rename("mean70_09" = "estimate1970to2009",
         "mean10_19" = "estimate2010to2019") %>%
  mutate(comname = stringr::str_to_sentence(comname))

# gt table showing significant p-values and means ####
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
  fmt_number(columns = everything(), decimals = 2) %>%
  sub_missing(columns = everything(), rows = everything(), missing_text = "") %>%
  tab_footnote(footnote = md("*2017 excluded from 2010-2019 means*"))
gt::gtsave(tab, "table1.docx")

## Time series for each species and variable ####
#  Read in annual_averages 
all_strata_dist <- all_strata %>%
  unnest(data) %>%
  mutate(comname = stringr::str_to_sentence(comname)) %>%
  select(comname, variable, est_year, group, measurement, mean70_09, mean10_19) %>%
  group_by(comname, variable) %>%
  nest()

all_strata_dist <- all_strata_dist %>%
  mutate(plot = map2(data, comname, function(df, comname){
    plot <- ggplot(data = df) +
      geom_line(aes(x=est_year, y=measurement), color = "#E9E9E9", linewidth = 0.5)+
      geom_point(aes(x = est_year, y = measurement), size=0.5)+
      geom_segment(aes(x = 1970, xend = 2009, y = mean70_09, yend = mean70_09), color = "#00608A") +
      geom_segment(aes(x = 2010, xend = 2019, y = mean10_19, yend = mean10_19), color = "#EA4F12") +
      ggtitle(comname) +
      theme_gmri(axis.title = element_blank(),
                 #axis.title.y = paste(variable),
                 plot.title = element_text(size = 15),
                 axis.text.y = element_text(size=10),
                 strip.background = element_blank(),
                 strip.text = element_text(color = "black"))
      
    return(plot)})) %>%
  group_by(variable) %>%
  nest()

depth <- all_strata_dist[[2]][[1]][[3]]
bt    <- all_strata_dist[[2]][[2]][[3]]
sst   <- all_strata_dist[[2]][[3]][[3]]
lat   <- all_strata_dist[[2]][[4]][[3]]
lon   <- all_strata_dist[[2]][[5]][[3]]

# depth multipanel
species_depth <- marrangeGrob(depth, layout_matrix = matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE), top = NULL, left = quote(paste("Depth (m)")))
ggsave("Temp_Results/Plots/Depth_multipanel.pdf", species_depth, height =15, width = 12.5, units = "in")

# bt multipanel
species_bt <- marrangeGrob(bt, layout_matrix = matrix(1:20,  nrow = 5, ncol=4, byrow=TRUE), top = NULL, left = quote(paste("Bottom Temperature (\u00B0C)")))
ggsave("Temp_Results/Plots/BT_multipanel.pdf", species_bt, height = 15, width = 12.5, units ="in")

# sst multipanel 
species_sst <- marrangeGrob(sst, layout_matrix = matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE), top = NULL, left = quote(paste("Surface Temperature (\u00B0C)")))
ggsave("Temp_Results/Plots/SST_multipanel.pdf", species_sst, height = 15, width = 12.5, units = "in")

# lat multipanel 
species_lat <- marrangeGrob(lat, layout_matrix = matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE), top = NULL, left = quote(paste("Latitude (\u00B0N)")))
ggsave("Temp_Results/Plots/Lat_Multipanel.pdf", species_lat, height = 15, width = 12.5, units = "in")

#lon multipanel
species_lon <-  marrangeGrob(lon, layout_matrix = matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE), top = NULL, left = quote(paste("Longitude (\u00B0W)")))
ggsave("Temp_Results/Plots/Lon_Multipanel.pdf", species_lon, height = 15, width = 12.5, units = "in")

## Significant Movers Map ####
significant_movers <- all_strata %>%
  filter(!variable %in% c("avg_depth", "avg_sur_temp", "avg_bot_temp")) %>%
  filter(different == "TRUE") %>%
  select(!data & !method)

significant_movers <- significant_movers %>% 
  filter(variable == "avg_lat") %>% 
  mutate(direction = ifelse(mean10_19 > mean70_09, "North", "South")) %>%
  full_join(significant_movers %>% 
              filter(variable == "avg_lon") %>%
              mutate(direction = ifelse(mean10_19 < mean70_09, "West", "East"))) %>%
  filter(!comname %in% c("Atlantic cod", "Atlantic hagfish", "Atlantic herring","Fourspot flounder", "Gulf stream flounder", "Haddock", "Jonah crab", "Longhorn sculpin", 
                         "Northern shortfin squid", "Pollock", "Red hake", "Spotted hake", "Thorny skate", "Winter flounder", "Winter skate", "Witch flounder", 
                         "Yellowtail flounder")) %>% arrange(comname)

movers_plot <- significant_movers %>% 
  ungroup(comname, variable, mean70_09, mean10_19, p.value, different, direction) %>%
  select(comname, variable, mean70_09, mean10_19) %>%
  pivot_longer(cols = 3:4, names_to = "group", values_to = "means") %>%
  pivot_wider(names_from = "variable", values_from = "means")

plot_names <- movers_plot %>%
  filter(group == "mean70_09") %>%
  rowid_to_column()

# Map 
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggrepel)

world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data=world)+
  geom_sf()+
  coord_sf(xlim=c(-76, -66), ylim=c(37.5,47))+
  geom_line(data=movers_plot, aes(x=avg_lon, y=avg_lat, group = comname), color="#535353", linewidth = 0.5)+
  geom_point(data=movers_plot, aes(x=avg_lon, y=avg_lat, color = group), size=2)+
  #scale_color_gmri() +
  scale_color_manual(name = "Years", labels = c("2010-2019", "1970-2009"), values=c("#EA4f12", "#00608A"))+
  theme_gmri(legend.text = element_text(size = 12),
             legend.title = element_text(size = 12, face = "bold"),
             axis.line = element_blank(),
             axis.title = element_text(size = 12, face = "bold"))+
  ggtitle("Mean Center of Biomass")+
  ylab("Center of Latitude")+
  xlab("Center of Longitude")+
  geom_text(data= plot_names, aes(x=avg_lon, y=avg_lat, label= rowid)) +
  scale_y_continuous(breaks = c(36,40,44)) + scale_x_continuous(breaks = c(-78,-72,-66))
ggsave("movement.png", movement, height = 10, width = 10, units="in")

# Map Legend
legend <- plot_names %>%
  select(rowid, comname) %>%
  gt(groupname_col = NULL) %>%
  cols_label(comname = md("**Species**"),
             rowid = "") 
gtsave(legend, "legend.png")

# Decadal Maps
decadal_maps <- all_strata %>%
  filter(comname %in% c("Acadian redfish", "American lobster", "Blackbelly rosefish", "Summer flounder")) %>%
  filter(variable %in% c("avg_lat", "avg_lon")) %>%
  select(comname, variable, data) %>%
  unnest(data) %>%
  mutate(Decade = 10*est_year %/% 10) %>%
  pivot_wider(names_from = "variable", values_from = "measurement")

gradient_map <- ggplot(data=world)+
  geom_sf()+
  coord_sf(xlim=c(-76, -66), ylim=c(37.5,47)) +
  geom_point(data = decadal_maps, aes(x=avg_lon, y= avg_lat, color = Decade), size = 3) +
  scale_color_continuous()+
  facet_wrap(~comname, nrow = 1) +
  theme_gmri(legend.title = element_text(face = "bold", size = 11),
             legend.text = element_text(size = 8.5), 
             axis.title = element_blank(),
             axis.text = element_text(size = 12), 
             strip.text = element_text(size = 12))
ggsave("Temp_Results/Maps/Decadal_Gradient.pdf", width = 15, height = 7, units = "in")

# try with means?
dec_means_gradient <- decadal_maps %>% 
  group_by(comname, Decade) %>%
  nest() %>%
  mutate(decadal_lat = map(data, mean_lat),
         decadal_lon = map(data, mean_lon),
         decadal_lat = as.numeric(decadal_lat),
         decadal_lon = as.numeric(decadal_lon))

ggplot(data=world)+
  geom_sf()+
  coord_sf(xlim=c(-76, -66), ylim=c(37.5,47)) +
  geom_point(data = dec_means_gradient, aes(x=decadal_lon, y= decadal_lat, color = Decade), size = 3) +
  scale_color_continuous()+
  facet_wrap(~comname, nrow = 1) +
  theme_gmri(legend.title = element_text(face = "bold", size = 11),
             legend.text = element_text(size = 8.5), 
             axis.title = element_blank(),
             axis.text = element_text(size = 12), 
             strip.text = element_text(size = 12))

## Quadrant ####
nontrackers <- all_strata %>%
  ungroup(variable) %>%
  select(comname) %>%
  distinct() %>%
  filter(comname %in% c("Acadian redfish", "American shad", "Atlantic rock crab", "Butterfish", "Northern sand lance")) %>%
  mutate(Quad = "Non-trackers")
effective <-  all_strata %>%
  ungroup(variable) %>%
  select(comname) %>%
  distinct() %>%
  filter(comname %in% c("American lobster", "Atlantic mackerel", "Black sea bass", "Blackbelly rosefish", "Jonah crab", 
                        "Northern searobin", "Rosette skate", "Scup", "Sea scallop", "Smooth dogfish")) %>%
  mutate(Quad = "Effective trackers")
ineffective <- all_strata %>% 
  ungroup(variable) %>%
  select(comname) %>%
  distinct() %>%
  filter(!comname %in% c("Acadian redfish", "American shad", "Atlantic rock crab", "Butterfish", "Northern sand lance")) %>%
  filter(!comname %in% c("American lobster", "Atlantic mackerel", "Black sea bass", "Blackbelly rosefish", "Jonah crab", 
                        "Northern searobin", "Rosette skate", "Scup", "Sea scallop", "Smooth dogfish")) %>%
  mutate(Quad = "Ineffective trackers")
quad <- nontrackers %>%
  full_join(effective) %>%
  full_join(ineffective) %>%
  arrange(comname)
  
# add to gt ?
tab2 <- all_strata_gt %>%
  full_join(quad) %>%
  gt(groupname_col = NULL) %>%
  tab_spanner(label = md("**Latitude (\u00B0N)**"), columns = c("avglat1970", "avglat2010", "avglatp")) %>%
  tab_spanner(label = md("**Longitude (\u00B0W)**"), columns = c("avglon1970", "avglon2010", "avglonp")) %>%
  tab_spanner(label = md("**Depth (m)**"), columns = c("avgdepth1970", "avgdepth2010", "avgdepthp")) %>%
  tab_spanner(label = md("**Surface Temperature (\u00B0C)**"), columns = c("avgsst1970", "avgsst2010", "avgsstp")) %>%
  tab_spanner(label = md("**Bottom Temperature (\u00B0C)**"), columns = c("avgbot1970", "avgbot2010", "avgbotp")) %>%
  tab_spanner(label = md("**Classification**"), columns = "Quad") %>%
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
    avgbotp    = md("*p*"),
    Quad       = ("")) %>%
  fmt_number(columns = everything(), decimals = 2) %>%
  sub_missing(columns = everything(), rows = everything(), missing_text = "") %>%
  tab_footnote(footnote = md("*2017 excluded from 2010-2019 means*"))
gtsave(tab2, "Temp_Results/table1_w_movement_class.docx")
