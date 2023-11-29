# Plotting strata effort 
library(here)
library(tidyverse)
library(gmRi)

# Reading strata effort data ####
strata_effort <- read_rds(here("Processed Data", "strata_effort.RDS"))

# Plot ####
Figure_S1 <- strata_effort %>%
  unnest(data) %>%
  mutate(stratum = stri_sub(stratum, 2,3)) %>%
  ggplot() +
  geom_line(aes(x = est_year, y = strata_effort_ratio, group = stratum, color = as.factor(stratum)), linewidth = 1.0) + 
  ylim(c(0.000, 0.100)) +
  guides(col = guide_legend(title = "Stratum", nrow = 4, byrow = TRUE)) +
  theme_gmri(legend.title = element_text(size = 8, face = "bold"),
             legend.text  = element_text(size = 6),
             legend.position = "bottom",
             axis.title   = element_text(size = 8, face = "bold"),
             axis.text    = element_text(size = 7))+
  xlab("Year") +
  ylab("Proportion of Annual Tows") +
  scale_color_gmri()

ggsave(here("Figures","Figure_S1.pdf"), Figure_S1, height = 170, width = 170, units = "mm", bg = "white")
