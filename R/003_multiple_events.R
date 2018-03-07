library(ggplot2);library(dplyr)
# library(g6tr); theme_set(theme_g6tr())

head(ebmt4, 40) %>%
  tail(6) %>%
  select(c(1:5, 8:9)) %>%
  set_colnames(c('id', 'Time 1', 'Status 1', 'Time 2', 'Status 2', 'Time 3', 'Status 3')) %>%
  group_by(id) %>%
  mutate(maxtime = max(`Time 1`, `Time 2`, `Time 3`)) %>%
  ggplot() +
  geom_linerange(aes(ymin = 0, ymax = maxtime, x = id), size = 2, colour = g6tr_pal(palette = 'core')(2)[2]) +
  coord_flip() +
  geom_point(aes(x = id, y = `Time 1`, shape = as.factor(`Status 1`)),
             size = 3,
             colour = g6tr_pal(palette = 'core')(3)[3]) +
  geom_point(aes(x = id, y = `Time 2`, shape = as.factor(`Status 2`)),
             size = 3,
             colour = g6tr_pal(palette = 'core')(4)[4]) +
  geom_point(aes(x = id, y = `Time 3`, shape = as.factor(`Status 3`)),
             size = 3,
             colour = g6tr_pal(palette = 'core')(1)[1]) +
  
  
  #scale_x_continuous(labels = 1:size, breaks = 1:size) +
  # scale_y_date(breaks = seq(as.Date(min(dt$start_date)),
  #                           as.Date(max(dt$end_date)),
  #                           by = 'month')) +
  labs(x = "Observation", y = "Days") +
  guides(
    color  = guide_legend(show = FALSE),
    shape  = guide_legend(show = FALSE)
  ) +
  theme(axis.text.x = element_text(family = 'Anonymous Pro', angle = 0, hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = 'none',
        legend.background = element_rect(fill = '#DDDDDA', colour = NA),
        legend.box.background = element_rect(colour = '#DDDDDA', fill = '#DDDDDA')
  ) 

g6tr_save('plots/004_multiple_events.pdf')