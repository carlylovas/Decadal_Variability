# what on earth is herring doing
weighted_data %>%
  filter(comname == "atlantic herring") %>%
  unnest(data) %>%
  ggplot() +
  geom_point(aes(x = avg_biomass, y = avg_sur_temp))

herring_test <- grouped_center_bio(clean_survey, est_year, season, survey_area) %>% 
  filter(comname == "atlantic herring")

ggplot(data = herring_test) +
  geom_point(aes(x = avg_biomass, y = avg_sur_temp, color = season)) +
  xlim(c(0,90)) +
  facet_wrap(~survey_area)

ggplot(data = herring_test) +
  geom_point(aes(x = avg_biomass, y = avg_bot_temp, color = season)) +
  xlim(c(0,90)) +
  facet_wrap(~survey_area)

ggplot(data = herring_test) +
  geom_point(aes(x = avg_biomass, y = avg_depth, color = season)) +
  xlim(c(0,90)) +
  scale_y_reverse() +
  facet_wrap(~survey_area)

p1 <- ggplot(data = herring_test) +
  geom_point(aes(x = avg_biomass, y = avg_depth, color = season)) +
  scale_y_reverse() +
  facet_wrap(~survey_area, ncol = 1)

p2 <- ggplot(data = herring_test) +
  #geom_point(aes(x = est_year, y = avg_depth, color = season)) +
  geom_point(aes(x = avg_biomass, y = avg_sur_temp, color = season)) +
  #scale_y_reverse() +
  facet_wrap(~survey_area, ncol = 1)

ggpubr::ggarrange(p1, p2, ncol =2, common.legend = T)
