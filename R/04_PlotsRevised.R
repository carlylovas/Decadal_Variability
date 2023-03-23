##revised plots
library(rnaturalearth)
library(rnaturalearthdata)
library(gridExtra)
library(sf)
library(patchwork)
library(gmRi)

##use rev_data from 03_Revisions.R
rev_data<-readRDS(here("Data", "revised_plot_data.RDS"))

#average lat####
nrow(rev_data)
lat<-vector("list", length = 49)
names(lat)=paste(unique(rev_data$comname))

for(i in 1:49){
  print(i)
  loop_df<-rev_data[i,]%>%
    unnest(data)%>%
    select(comname, est_year, avg_lat)%>%
    group_by(comname)
  
  group1<-rev_data[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(1970:2009))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_lat = map(data, possibly(mean_lat, NA)))%>%
    unnest(data)
  
  group2<-rev_data[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(2010:2019))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_lat = map(data, possibly(mean_lat, NA)))%>%
    unnest(data)
  
  lat[[i]]<- ggplot(data=loop_df, aes(x=est_year, y=avg_lat))+
    geom_line(color = "#E9E9E9", linewidth = 0.5)+
    geom_point(size=0.5)+
    theme_gmri(axis.title = element_blank(),
               plot.title = element_text(size = 11),
               axis.text.y = element_text(size=10))+
    ggtitle(toupper(names(lat)[i]))+
    geom_line(data=group1, aes(x=est_year, y=as.numeric(overall_lat)), color="#00608A", linewidth=0.75)+
    geom_line(data=group2, aes(x=est_year, y=as.numeric(overall_lat)), color="#EA4F12", linewidth=0.75)
}
print(lat[2])

#average lon####
lon<-vector("list", length = 49)
names(lon)=paste(unique(rev_data$comname))

for(i in 1:49){
  print(i)
  loop_df<-rev_data[i,]%>%
    unnest(data)%>%
    select(comname, est_year, avg_lon)%>%
    group_by(comname)
  
  group1<-rev_data[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(1970:2009))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_lon = map(data, possibly(mean_lon, NA)))%>%
    unnest(data)
  
  group2<-rev_data[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(2010:2019))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_lon = map(data, possibly(mean_lon, NA)))%>%
    unnest(data)
  
  lon[[i]]<- ggplot(data=loop_df, aes(x=est_year, y=avg_lon))+
    geom_line(color = "#E9E9E9", linewidth = 0.5)+
    geom_point(size=0.5)+
    theme_gmri(axis.title = element_blank(),
               plot.title = element_text(size = 11),
               axis.text.y = element_text(size=10))+
    ggtitle(toupper(names(lon)[i]))+
    geom_line(data=group1, aes(x=est_year, y=as.numeric(overall_lon)), color="#00608A", linewidth=0.75)+
    geom_line(data=group2, aes(x=est_year, y=as.numeric(overall_lon)), color="#EA4F12", linewidth=0.75)
}
print(lon[2])

##average surface temp
sst<-vector("list", length=49)
names(sst)=paste(unique(rev_data$comname))

for(i in 1:49){
  print(i)
  loop_df<-rev_data[i,]%>%
    unnest(data)%>%
    select(comname, est_year, avg_sur_temp)%>%
    group_by(comname)
  
  group1<-rev_data[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(1970:2009))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_sst = map(data, possibly(avg_sst, NA)))%>%
    unnest(data)
  
  group2<-rev_data[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(2010:2019))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_sst = map(data, possibly(avg_sst, NA)))%>%
    unnest(data)
  
  sst[[i]]<- ggplot(data=loop_df, aes(x=est_year, y=avg_sur_temp))+
    geom_line(color = "#E9E9E9", linewidth = 0.5)+
    geom_point(size=0.5)+
    theme_gmri(axis.title = element_blank(),
               plot.title = element_text(size = 11),
               axis.text.y = element_text(size=10))+
    ggtitle(toupper(names(lon)[i]))+
    geom_line(data=group1, aes(x=est_year, y=as.numeric(overall_sst)), color="#00608A", linewidth=0.75)+
    geom_line(data=group2, aes(x=est_year, y=as.numeric(overall_sst)), color="#EA4F12", linewidth=0.75)
}

#bottom temp####
bt<-vector("list", length=49)
names(bt)=paste(unique(rev_data$comname))

for(i in 1:49){
  print(i)
  loop_df<-rev_data[i,]%>%
    unnest(data)%>%
    select(comname, est_year, avg_bot_temp)%>%
    group_by(comname)
  
  group1<-rev_data[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(1970:2009))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_bt = map(data, possibly(avg_bt, NA)))%>%
    unnest(data)
  
  group2<-rev_data[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(2010:2019))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_bt = map(data, possibly(avg_bt, NA)))%>%
    unnest(data)
  
  sst[[i]]<- ggplot(data=loop_df, aes(x=est_year, y=avg_bot_temp))+
    geom_line(color = "#E9E9E9", linewidth = 0.5)+
    geom_point(size=0.5)+
    theme_gmri(axis.title = element_blank(),
               plot.title = element_text(size = 11),
               axis.text.y = element_text(size=10))+
    ggtitle(toupper(names(lon)[i]))+
    geom_line(data=group1, aes(x=est_year, y=as.numeric(overall_bt)), color="#00608A", linewidth=0.75)+
    geom_line(data=group2, aes(x=est_year, y=as.numeric(overall_bt)), color="#EA4F12", linewidth=0.75)
}


#depth###
depth<-vector("list", length = 49)
names(depth)=paste(unique(rev_data$comname))

for(i in 1:49){
  print(i)
  loop_df<-rev_data[i,]%>%
    unnest(data)%>%
    select(comname, est_year, avg_depth)%>%
    group_by(comname)
  
  group1<-rev_data[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(1970:2009))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_depth = map(data, possibly(mean_depth, NA)))%>%
    unnest(data)
  
  group2<-rev_data[i,]%>%
    unnest(data)%>%
    filter(est_year %in% c(2010:2019))%>%
    group_by(comname)%>%
    nest()%>%
    mutate(overall_depth = map(data, possibly(mean_depth, NA)))%>%
    unnest(data)

  depth[[i]]<- ggplot(data=loop_df, aes(x=est_year, y=avg_depth))+
  geom_line(color = "#E9E9E9", linewidth = 0.5)+
  geom_point(size=0.5)+
  theme_gmri(axis.title = element_blank(),
             plot.title = element_text(size = 11),
             axis.text.y = element_text(size=10))+
  ggtitle("Average Depth")+
  scale_y_reverse()+
  geom_line(data=group1, aes(x=est_year, y=as.numeric(overall_depth)), color="#00608A", linewidth=0.75)+
  geom_line(data=group3, aes(x=est_year, y=as.numeric(overall_depth)), color="#EA4F12", linewidth=0.75)
}

###print plots alphabetically in multipage layouts
lat_list   <-   lat[c(1:49)]
lon_list   <-   lon[c(1:49)]
sst_list   <-   sst[c(1:49)]
bt_list    <-    bt[c(1:49)]
depth_list <- depth[c(1:49)]

##lat plots
species_lat<-marrangeGrob(lat_list, layout_matrix = matrix(1:20,  nrow = 5, ncol=4, byrow=TRUE), top=NULL)
ggsave("lat_multipanel_v3.pdf", species_lat, height = 15, width = 12.5, units ="in")

#lon plots
species_lon<-marrangeGrob(lon_list, layout_matrix = matrix(1:20,  nrow = 5, ncol=4, byrow=TRUE), top=NULL)
ggsave("lon_multipanel_v3.pdf", species_lon, height = 15, width = 12.5, units ="in")

#sst plots
species_sst<-marrangeGrob(sst_list, layout_matrix = matrix(1:20,  nrow = 5, ncol=4, byrow=TRUE), top=NULL)
ggsave("sst_multipanel_v3.pdf", species_sst, height = 15, width = 12.5, units ="in")

#bt plots
species_bt<-marrangeGrob(bt_list, layout_matrix = matrix(1:20,  nrow = 5, ncol=4, byrow=TRUE), top=NULL)
ggsave("bt_multipanel_v3.pdf", species_bt, height = 15, width = 12.5, units ="in")

#depth plots
species_depth<-marrangeGrob(depth_list, layout_matrix = matrix(1:20,  nrow = 5, ncol=4, byrow=TRUE), top=NULL)
ggsave("depth_multipanel_v3.pdf", species_depth, height = 15, width = 12.5, units ="in")
