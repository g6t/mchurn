# simple states' plots
library(ggplot2);library(dplyr)
# library(g6tr); theme_set(theme_g6tr())

calendar <- seq(as.Date(Sys.Date()-100), as.Date(Sys.Date()), by = "day")
size <- 12
events <- c('Censoring', 'Event')

survdata <- 
  data.frame(
  id = 1:size,
  start_date = sample(calendar, size = size),
    end_date = sample(calendar, size = size),
       event = sample(events  , size = size, replace = TRUE),
  stringsAsFactors = FALSE
) %>%
  
{
  df <- .
  # when a start date is smaller (equal) the end date
  when <- df$start_date        <= df$end_date
          df$start_date[!when] <- df$end_date[!when]
            df$end_date[!when] <-.$start_date[!when]
  df
}

survdata %>% {
  dt <- .
  dt %>%
  ggplot(aes(ymin = start_date, ymax = end_date, x = id)) +
  geom_linerange(size = 2, colour = g6tr_pal(palette = 'core')(2)[2]) +
  coord_flip() +
  geom_point(aes(x = id, y = end_date, shape = event),
             size = 3,
             colour = g6tr_pal(palette = 'core')(3)[3]) +
  scale_x_continuous(labels = 1:size, breaks = 1:size) +
  scale_y_date(breaks = seq(as.Date(min(dt$start_date)),
                                  as.Date(max(dt$end_date)),
                                  by = 'month')) +
  labs(x = "Observation", y = "Calendar dates") +
  guides(
    color  = guide_legend(override.aes = list(fill = NA)),
    shape  = guide_legend(title = 'Type of the event')
  ) +
    theme(axis.text.x = element_text(family = 'Anonymous Pro', angle = 0, hjust = 0.5),
          panel.grid.minor.y = element_blank(),
          legend.background = element_rect(fill = '#DDDDDA', colour = NA),
          legend.box.background = element_rect(colour = '#DDDDDA', fill = '#DDDDDA')
    ) 
}
g6tr_save('plots/001_censoring.pdf')




survdata %>%
  mutate(time = difftime(end_date, start_date, units = "days")) %>%
  {
    dt <- .
    dt %>%
      ggplot(aes(ymin = 0, ymax = time, x = id)) +
      geom_linerange(size = 2, colour = g6tr_pal(palette = 'core')(2)[2]) +
      coord_flip() +
      geom_point(aes(x = id, y = time, shape = event),
                 size = 3,
                 colour = g6tr_pal(palette = 'core')(3)[3]) +
      scale_x_continuous(labels = 1:size, breaks = 1:size) +
      # scale_y_date(breaks = seq(as.Date(min(dt$start_date)),
      #                           as.Date(max(dt$end_date)),
      #                           by = 'month')) +
      labs(x = "Observation", y = "Days") +
      guides(
        color  = guide_legend(override.aes = list(fill = NA)),
        shape  = guide_legend(title = 'Type of the event')
      ) +
      theme(axis.text.x = element_text(family = 'Anonymous Pro', angle = 0, hjust = 0.5),
            panel.grid.minor.y = element_blank(),
            legend.background = element_rect(fill = '#DDDDDA', colour = NA),
            legend.box.background = element_rect(colour = '#DDDDDA', fill = '#DDDDDA')
      ) 
  }

g6tr_save('plots/001_censoring_normalized.pdf')
