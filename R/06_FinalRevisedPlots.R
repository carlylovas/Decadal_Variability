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
write.csv(all_strata_gt, "Data/table1.csv")

# make gt table
tab<- all_strata_gt %>%
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
      geom_line(aes(x=est_year, y=measurement), color = "#E9E9E9", linewidth = 0.45)+
      geom_point(aes(x = est_year, y = measurement), size=0.2)+
      geom_segment(aes(x = 1970, xend = 2009, y = mean70_09, yend = mean70_09), linewidth = 0.7, color = "#00608A") +
      geom_segment(aes(x = 2010, xend = 2019, y = mean10_19, yend = mean10_19), linewidth = 0.7, color = "#EA4F12") +
      ggtitle(comname) +
      # theme_gmri(axis.title = element_blank(),
      #            plot.title = element_text(size = 8),
      #            axis.text = element_text(size=5),
      #            strip.background = element_blank(),
      #            strip.text = element_text(color = "black"))
      theme_gmri(axis.title = element_blank(),
            plot.title = element_text(size = 8),
            axis.text=element_text(size = 6),
            panel.grid=element_line(size = 0.2),
            axis.line.x=element_line(size = 0.1),
            axis.ticks.x=element_line(size = 0.1),
            plot.margin   = margin(t = 8, b = 4, r = 8, l = 4))
    return(plot)})) %>%
  group_by(variable) %>%
  nest()

#depth <- all_strata_dist[[2]][[1]][[3]]
bt    <- all_strata_dist[[2]][[2]][[3]]
sst   <- all_strata_dist[[2]][[3]][[3]]
lat   <- all_strata_dist[[2]][[4]][[3]]
lon   <- all_strata_dist[[2]][[5]][[3]]

# depth multipanel
# species_depth <- marrangeGrob(depth, layout_matrix = matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE), top = NULL, left = quote(paste("Depth (m)")))
# ggsave("Temp_Results/Plots/Depth_multipanel.pdf", species_depth, height = 15, width = 12.5, units = "in")

# bt multipanel
species_bt <- marrangeGrob(bt, layout_matrix = matrix(1:20,  nrow = 5, ncol=4, byrow=TRUE), top = NULL, left = textGrob(
  expression(bold("Bottom Temperature (\u00B0C)")), rot = 90, gp = gpar(col = "black", fontsize = 8)))
ggsave("Temp_Results/Revised/Figure_S7_BotTemp.pdf", species_bt, width = 170, height = 225, units ="mm", dpi = 500)

# sst multipanel 
species_sst <- marrangeGrob(sst, layout_matrix = matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE), top = NULL, left = textGrob(
  expression(bold("Surface Temperature (\u00B0C)")), rot = 90, gp = gpar(col = "black", fontsize = 8)))
ggsave("Temp_Results/Revised/Figure_S6_SurfTemp.pdf", species_sst, height = 225, width = 170, units = "mm")

# lat multipanel 
species_lat <- marrangeGrob(lat, layout_matrix = matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE), top = NULL, left = textGrob(
  expression(bold("Latitude (\u00B0N)")), rot = 90, gp = gpar(col = "black", fontsize = 8)))
ggsave("Temp_Results/Revised/Figure_S3_Latitude.pdf", species_lat, height = 225, width = 170, units = "mm")

#lon multipanel
species_lon <-  marrangeGrob(lon, layout_matrix = matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE), top = NULL, left = textGrob(
  expression(bold("Longitude (\u00B0W)")), rot = 90, gp = gpar(col = "black", fontsize = 8)))
ggsave("Temp_Results/Revised/Figure_S4_Longitude.pdf", species_lon, height = 225, width = 170, units = "mm")

## 8/24 realized the y-axis on the depth plots should be reversed so that larger (more negative) values are on the bottom
revised_depth <- all_strata_dist %>%
  filter(variable == "avg_depth") %>%
  unnest(data) %>% 
  mutate(plot_revised = map2(data, comname, function(df, comname){
    plot <- ggplot(data = df) +
      geom_line(aes(x=est_year, y=measurement), color = "#E9E9E9", linewidth = 0.45)+
      geom_point(aes(x = est_year, y = measurement), size=0.2)+
      geom_segment(aes(x = 1970, xend = 2009, y = mean70_09, yend = mean70_09), linewidth = 0.7, color = "#00608A") +
      geom_segment(aes(x = 2010, xend = 2019, y = mean10_19, yend = mean10_19), linewidth = 0.7, color = "#EA4F12") +
      ggtitle(comname) +
      scale_y_reverse() +
      theme_gmri(axis.title = element_blank(),
                 plot.title = element_text(size = 8),
                 axis.text=element_text(size = 6),
                 panel.grid=element_line(size = 0.2),
                 axis.line.x=element_line(size = 0.1),
                 axis.ticks.x=element_line(size = 0.1),
                 plot.margin   = margin(t = 8, b = 4, r = 8, l = 4))
    return(plot)})) %>%
  group_by(variable) %>%
  nest()
depth <- revised_depth[[2]][[1]][[4]]

# depth multipanel
species_depth <- marrangeGrob(depth, layout_matrix = matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE), top = NULL, left = textGrob(
  expression(bold("Depth (m)")), rot = 90, gp = gpar(col = "black", fontsize = 8)))
ggsave("Temp_Results/Revised/Figure_S5_Depth.pdf", species_depth, height =225, width = 170, units = "mm")

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

world <- ne_states(returnclass = "sf")
movement <- 
  ggplot(data=world)+
  geom_sf()+
  coord_sf(xlim=c(-74.5, -67.5), ylim=c(37.5, 43))+
  geom_point(data=movers_plot, aes(x=avg_lon, y=avg_lat, fill = group), pch = 21, size = 12)+
  geom_line(data=movers_plot, aes(x=avg_lon, y=avg_lat, group = comname), alpha = 0.5, color="#535353", linewidth = 1.5)+
  scale_fill_manual(name = "Years", labels = c("2010-2019", "1970-2009"), values=c("#EA4f12", "#00608A"))+
  theme_gmri(legend.text = element_text(size = 35),
             legend.title = element_text(size = 40, face = "bold"),
             axis.line = element_blank(),
             axis.title = element_text(size = 40, face = "bold"),
             axis.text  = element_text(size = 35),
             plot.title = element_text(size = 35), 
             panel.border = element_rect(fill = NA, linetype = 1, linewidth = 1, color = "lightgray"))+
  ylab("Latitude")+
  xlab("Longitude")+
  guides(fill = guide_legend(reverse=TRUE)) +
  #geom_text(data = plot_names, aes(x=avg_lon, y=avg_lat, label= rowid), fontface = "bold", color = "white", size = 6) +
  scale_y_continuous(breaks = c(38,40,42)) + scale_x_continuous(breaks = c(-74,-71,-68))
ggsave("movement.png", movement, height = 18, width = 27, units="in", bg = "white")

# Map Legend
legend <- plot_names %>%
  select(rowid, comname) %>%
  gt(groupname_col = NULL) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = rowid)) %>%
  tab_style(
    style = cell_text(size = "x-large"),
    locations = cells_body(columns = everything(), rows = everything())) %>% 
  tab_style(
    style = cell_text(size = "xx-large"),
    locations = cells_column_labels(columns = everything()))%>%
  cols_label(comname = md("**Species**"),
             rowid = "") 
gtsave(legend, "legend.png")

patchwork::wrap_ggplot_grob(movement, legend)

# Decadal Maps
decadal_maps <- all_strata %>%
  filter(comname %in% c("Acadian redfish","Summer flounder",  "American lobster" )) %>%
  filter(variable %in% c("avg_lat", "avg_lon")) %>%
  select(comname, variable, data) %>%
  unnest(data) %>%
  mutate(Decade = 10*est_year %/% 10) %>%
  pivot_wider(names_from = "variable", values_from = "measurement")

gradient_map <- ggplot(data=world)+
  geom_sf()+
  coord_sf(xlim=c(-76, -66), ylim=c(37.5,45)) +
  geom_point(data = decadal_maps, aes(x=avg_lon, y= avg_lat, color = as.factor(Decade)), size = 3) +
  scale_color_manual(values = icon_cols)+
  facet_wrap(~factor(comname, levels = c("Acadian redfish", "Summer flounder", "American lobster")), nrow = 1) +
  scale_y_continuous(breaks = c(36,40,44)) + scale_x_continuous(breaks = c(-78,-72,-66)) +
  theme_gmri(legend.title = element_text(face = "bold", size = 12),
             legend.text = element_text(size = 12), 
             axis.title = element_blank(),
             axis.text = element_text(size = 12), 
             strip.text = element_text(size = 12),
             strip.background = element_rect(fill = "#00608A"), 
             panel.border = element_rect(fill = NA, linetype = 1, linewidth = 1, color = "lightgray")) +
  guides(color = guide_legend(title = "Decade", override.aes = list(size = 5)))
print(gradient_map)
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
  #guides(color = guide_legend(override.aes = list(size = 5)))

# try with icons 
icon_cols <- c("#7D7C7C", "#22444B", "#0E6686", "#028ABD", "#02D2FF")
rosefish_1970 <- paste(readLines("Data/Rosefish/1970.svg", warn=F), collapse = "\n")
rosefish_1980 <- paste(readLines("Data/Rosefish/1980.svg", warn=F), collapse = "\n")
rosefish_1990 <- paste(readLines("Data/Rosefish/1990.svg", warn=F), collapse = "\n")
rosefish_2000 <- paste(readLines("Data/Rosefish/2000.svg", warn=F), collapse = "\n")
rosefish_2010 <- paste(readLines("Data/Rosefish/2010.svg", warn=F), collapse = "\n")

rosefish_icon <- as.data.frame(c(rosefish_1970, rosefish_1980, rosefish_1990, rosefish_2000, rosefish_2010)) %>%
  rename("Icons" = "c(rosefish_1970, rosefish_1980, rosefish_1990, rosefish_2000, rosefish_2010)")

icon_test <- decadal_maps %>% filter(comname == "Blackbelly rosefish") %>%
  group_by(Decade) %>%
  nest() %>%
  cbind(rosefish_icon) %>%
  unnest(data)
                                  
rosefish_plot <- ggplot(data=world)+
  geom_sf()+
  coord_sf(xlim=c(-76, -66), ylim=c(37.5,47)) +
  geom_point(data = icon_test, aes(x = avg_lon, y= avg_lat, color = as.factor(Decade)), size = 0.05) +
  geom_point_svg(data    = icon_test, 
                 mapping = aes(x=avg_lon, y= avg_lat), 
                 size    = 8, 
                 svg     = icon_test$Icons) +
  scale_color_manual(values = icon_cols) + 
  scale_y_continuous(breaks = c(36,40,44)) + scale_x_continuous(breaks = c(-78,-72,-66)) +
  guides(color = guide_legend(title = "Decade", override.aes = list(size = 5))) +
  ggtitle("Blackbelly rosefish") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_gmri(legend.title = element_text(face = "bold", size = 12),
             legend.text = element_text(size = 12), 
             #axis.title = element_blank(),
             axis.text = element_text(size = 12), 
             strip.text = element_text(size = 12),
             strip.background = element_rect(fill = "#00608A"), 
             panel.border = element_rect(fill = NA, linetype = 1, linewidth = 1, color = "lightgray")) 
ggsave("Temp_Results/Rosefish.png", rosefish_plot, height = 10, width = 10, units = "in", bg = "white")

# For all four example species
read_svg_func <- function(file_name) {
  out <- paste(readLines(paste0(file_name), warn=F), collapse = "\n")
  return(out)
}
Decade <- as.data.frame(c("1970", "1980", "1990", "2000", "2010"))
colnames(Decade) <- "Decade"

all_icons <- tibble("File_Path" = list.files("Data/Icons", pattern = ".svg", full.names = TRUE)) %>%
  mutate(., "Data" = map(File_Path, read_svg_func)) %>%
  rowid_to_column()

all_icons <- all_icons %>%
  filter(rowid %in% seq(1,5)) %>% mutate(comname = "Summer flounder") %>%
  cbind(Decade) %>%
  full_join(all_icons %>% filter(rowid %in% seq(6,10)) %>% mutate(comname  = "American lobster") %>% cbind(Decade)) %>%
  full_join(all_icons %>% filter(rowid %in% seq(11,15)) %>% mutate(comname = "Acadian redfish")  %>% cbind(Decade)) %>%
  full_join(all_icons %>% filter(rowid %in% seq(16,20)) %>% mutate(comname = "Blackbelly rosefish") %>% cbind(Decade)) 

decadal_maps <- decadal_maps %>%
  mutate(Decade = as.character(Decade)) %>%
  group_by(comname, Decade) %>%
  nest() %>%
  full_join(all_icons) %>%
  unnest(data) %>%
  unnest(Data)

icon_map <- ggplot(data=world)+
  geom_sf()+
  coord_sf(xlim=c(-76, -66), ylim=c(37.5,47)) +
  geom_point(data = decadal_maps, aes(x = avg_lon, y= avg_lat, color = as.factor(Decade)), size = 0.05) +
  geom_point_svg(data    = decadal_maps, 
                 mapping = aes(x=avg_lon, y= avg_lat), 
                 size    = 5, 
                 svg     = decadal_maps$Data) +
  scale_color_manual(values = icon_cols) + 
  scale_y_continuous(breaks = c(36,40,44)) + scale_x_continuous(breaks = c(-78,-72,-66)) +
  guides(color = guide_legend(title = "Decade", override.aes = list(size = 5))) +
  facet_wrap(~comname, nrow = 1) + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_gmri(legend.title = element_text(face = "bold", size = 12),
             legend.text = element_text(size = 12), 
             #axis.title = element_blank(),
             axis.text = element_text(size = 12), 
             strip.text = element_text(size = 12),
             strip.background = element_rect(fill = "#00608A"), 
             panel.border = element_rect(fill = NA, linetype = 1, linewidth = 1, color = "lightgray")) 

ggsave("Temp_Results/Maps/Decadal_Gradient_Icons.pdf", icon_map, width = 15, height = 7, units = "in")


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

# make a gt quad?
quad %>%
  gt(groupname_col = NULL) %>%
  cells_row_groups(groups = everything())

## strata comparison table for the weirdos
# read in strata_t_test_comparison.csv
var <- as.data.frame(c('Depth', 'Bottom Temperature', 'Surface Temperature', 'Latitude', 'Longitude')) 
colnames(var) <- "var"

strata_comp_dt <- t.test.comparison %>% 
  group_by(variable) %>%
  nest() %>%
  cbind(var) %>%
  unnest(data) %>%
  relocate(var, .after = comname) %>%
  filter(significant_change == "True") %>%
  mutate(comname = str_to_sentence(comname)) %>% 
  arrange(comname) %>%
  gt(groupname_col = NULL) %>%
  tab_spanner(label = md("**All Strata**"), columns = c("all_strata_1970_2009", "all_strata_2010_2009", "all_strata")) %>%
  tab_spanner(label = md("**Stratum 16 removed**"), columns = c("no_16_strata_1970_2009","no_16_strata_2010_2019", "revised")) %>%
  cols_label(
    comname = md("**Species**"),
    var = md("**Variable**"), 
    all_strata_1970_2009 = md("*1970-2009*"),
    all_strata_2010_2009 = md("*2010-2019*"),
    all_strata = md("*p*"), 
    no_16_strata_1970_2009 = md("*1970-2009*"),
    no_16_strata_2010_2019 = md("*2010-2019*"),
    revised = md("*p*"))  %>%
  tab_style(
    style = cell_text(size = "x-large"),
    locations = cells_body(columns = everything(), rows = everything())) %>% 
  tab_style(
    style = cell_text(size = "x-large"),
    locations = cells_column_labels(columns = everything())) %>%
  tab_style(
    style = cell_text(size = "x-large"),
    locations = cells_column_spanners()) %>%
  cols_hide(columns = c("change", "significant_change", "variable")) %>%
  fmt_number(columns = c("all_strata_1970_2009", "all_strata_2010_2009","no_16_strata_1970_2009","no_16_strata_2010_2019"), decimals = 2) %>%
  fmt_number(columns = c("all_strata", "revised"), decimals = 3)

gtsave(strata_comp_dt, "strata_comparison.png")

# looking at stuff
View(all_strata %>%
  filter(variable == "avg_depth") %>%
  filter(different == "TRUE") %>%
  filter(mean70_09 > mean10_19))

all_strata %>%
  filter(variable == "avg_sur_temp") %>%
  filter(different == "TRUE") %>%
  #filter((mean10_19 - mean70_09) > 25) %>%
  mutate(mean_diff = (mean10_19 - mean70_09)) %>%
  arrange(desc(mean_diff))

quad %>%
  filter(Quad == "Effective trackers")


# R/SB Table
r_sb_table <- read_csv("Data/r_sb_table.csv") %>%
  mutate(sig = ifelse(p < 0.05, 'Yes', 'No'))

r_sb_table %>%
  gt() %>%
  tab_spanner(columns = c("pre-2010", "post-2010", "% change"), label = md("**R/S**")) %>%
  cols_hide(sig) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = c(`p`, `pre-2010`, `post-2010`, `% change`),
                                   rows = p <0.05)) %>%
  cols_label(
    `Species`   = md("**Species**"),
    `Stock`     = md("**Stock**"),
    `pre-2010`  = md("*Pre-2010*"),
    `post-2010` = md("*Post-2010*"),
    `% change`  = md("*Percent Change*"),
    `p`         = md("*p*")) %>%
  fmt_number(columns = !p, decimals = 2) -> rs_table
gtsave(file = "RS_Table.png", rs_table)
