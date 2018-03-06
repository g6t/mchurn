# 002
library(survminer);library(g6tr)
require("survival")
fit <- survfit(Surv(time, status) ~ sex, data = lung)
survplot <- function(fit, 
                     censor = TRUE,
                     risk.table.y.text.col = T, # colour risk table text annotations.
                     risk.table.y.text = FALSE, # show bars instead of names in text annotations
                     risk.table = FALSE,        # present the risk table under survival curves
                     font.family = "Anonymous Pro",
                     palette = g6tr_pal()(length(names(fit$strata))),
                     ...,
                     ggtheme = 
                       theme_g6tr() %+replace%
                       theme(axis.text.x = element_text(family = 'Anonymous Pro',
                                                        angle = 0,
                                                        hjust = 0.5,
                                                        colour = '#53565A'))
){
  plt <-
    ggsurvplot(fit = fit,
               censor = censor,
               risk.table.y.text.col = risk.table.y.text.col,
               risk.table.y.text = risk.table.y.text,
               risk.table = risk.table,
               palette = palette,
               ggtheme = ggtheme,
               font.family = font.family,
               ...)
  background_color <- '#DDDDDA'
  if(length(plt) > 1) {
    plt$plot <- 
      plt$plot +
      theme(legend.key = element_rect(fill = background_color, colour = NA),
            legend.position = 'bottom')
  } else {
    plt <- 
      plt +
      theme(legend.key = element_rect(fill = background_color, colour = NA),
            legend.position = 'bottom')
  }
  plt
}


survplot(fit, legend.labs = c('Male', 'Female'), conf.int = TRUE, xlim = c(0, 750)) +
  labs(x = 'Time in days') +
  guides(
     fill = guide_legend(title = 'Gender'),
    color = guide_legend(title = 'Gender')
  )

g6tr_save('plots/002_survival_curves.pdf')

survplot(fit, legend.labs = c('Male', 'Female'), conf.int = TRUE, xlim = c(0, 750),
         break.x.by = 150,
         risk.table = TRUE,
         risk.table.y.text = TRUE # show names instead of bars
         ) %>%
  extract2(2) +
  labs(x = 'Time in days', y = 'Gender', title = 'Observations at risk at a specific time')
  # guides(
  #   fill = guide_legend(title = 'Gender'),
  #   color = guide_legend(title = 'Gender')
  # )
g6tr_save('plots/003_risk_table.pdf', width = 7, height = 4)
