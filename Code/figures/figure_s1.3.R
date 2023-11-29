## Sampling effort histograms
library(here)
library(tidyverse)
library(gmRi)

# Read in plotting data ####
revised_histograms <- read_rds(here("Processed Data", "sampling_efforts.rds")) %>%
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

# Plot ####
group_labels <- c("1970-2009", "2010-2019", "2010-2019 (2017 removed)")
names(group_labels) <- c("1970-2009","2010-2019","!2017")

revised_histograms <- revised_histograms %>% 
  mutate(plot = map2(data, variable, function(df, variable){
    df <- df %>% mutate(across(group, factor, levels=c("1970-2009","2010-2019","!2017")))
    ggplot(data = df, aes(x = measurement)) +
      geom_histogram(aes(y = after_stat(density)), color = "white", fill = "darkgray") +
      facet_wrap(~group, labeller = labeller(group = group_labels)) +
      theme_gmri(legend.position  = "none",
                 plot.title       = element_text(size = 8, face = "bold"),
                 axis.title       = element_blank(),
                 axis.text        = element_text(size = 8),
                 axis.line.x      = element_line(size = 0.1),
                 axis.ticks.x     = element_line(size = 0.1),
                 strip.background = element_rect(fill = "#00608A"),
                 strip.text       = element_text(color = "white", face = "bold", size = 8),
                 panel.border     = element_rect(fill = NA, linetype = 1, linewidth = 0.5, color = "lightgray"),                 
                 panel.grid       = element_line(size = 0.2),
                 plot.margin      = margin(t = 8, b = 4, r = 8, l = 4)) +
      xlab(paste(variable)) +
      ggtitle(paste(variable))}))

histograms <- revised_histograms$plot[1:5]

all_hists <- marrangeGrob(histograms, layout_matrix = matrix(1:5, nrow = 5, ncol = 1, byrow = TRUE), top = NULL, left = textGrob(
  expression(bold("Frequency (proportion)")), rot = 90,
  gp = gpar(col = "black", fontsize = 8)) )

ggsave(here("Figures", "Figure_S1.3.pdf"), all_hists, width = 170, height = 225, units = "mm", bg = "white")
