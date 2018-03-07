library(ggplot2);library(dplyr);library(survminer)
# library(g6tr); theme_set(theme_g6tr())
?ggcoxzph

library(survival)
fit <- coxph(Surv(futime, fustat) ~ age + ecog.ps + rx, data=ovarian)
cox.zph.fit <- cox.zph(fit)
ggcoxzph(cox.zph.fit, var = c("ecog.ps"),
         ggtheme =  
           theme_g6tr() %+replace%
           theme(axis.text.x = element_text(family = 'Anonymous Pro',
                                            angle = 0,
                                            hjust = 0.5,
                                            colour = '#53565A')))
g6tr_save('plots/005_shoenfeld_residuals.pdf')

ggcoxdiagnostics(fit, type = "deviance", title = "Diagnostic plot",
                 ggtheme =  
                   theme_g6tr() %+replace%
                   theme(axis.text.x = element_text(family = 'Anonymous Pro',
                                                    angle = 0,
                                                    hjust = 0.5,
                                                    colour = '#53565A')))
g6tr_save('plots/006_deviance_residuals.pdf')

fit2 <- coxph(Surv(futime, fustat) ~ age, data=ovarian)
ggcoxfunctional(fit2, ggtheme =  
                  theme_g6tr() %+replace%
                  theme(axis.text.x = element_text(family = 'Anonymous Pro',
                                                   angle = 0,
                                                   hjust = 0.5,
                                                   colour = '#53565A')))
g6tr_save('plots/007_martingale_residuals.pdf')
