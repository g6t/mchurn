
# rename and filter data --------------------------------------------------


# recreate ebm4
library(mstate)
data(ebmt4)

msdata <- ebmt4

# let's say: year of registration in the subscription based business
levels(msdata$year) <- c('2002-2007', '2008-2012', '2013-2017')

library(dplyr)
# age
msdata <- msdata %>% rename(age = agecl)
# in a discount program
msdata <- msdata %>% rename(discount = proph)
# gender
msdata <- msdata %>% rename(gender = match)
levels(msdata$gender) <- c('male', 'female')
# let's simplify the graph
msdata <- msdata %>% filter(rel.s != 1) %>% select(-rel, -rel.s)
# one state less

# History
# 1) A user starts with a subscription of > 1 product                                   - initial state 1)
# 2) A user can delay the subcription (users that delayed subscription at leas once)    - 1 -> 2
# 3) A user can also stop ordering products so that he is left with one last product    - 1 -> 3
# 4) A user can be a user that delayed once and has one last product                    - 2 -> 4 / 3 -> 4
# 5) A user can have 0 products in hist subscription after he resigned of every product - 1 -> 5 / 2 -> 5 / 3 -> 5 / 4 -> 5
# for simplicity we don't allow situations in which user can extend the current subscription size (3 -> 1 is not allowed)

msdata <- msdata %>% 
  rename(st2  = rec,
         st2.s= rec.s,
         st3  = ae,
         st3.s= ae.s,
         st4  = recae,
         st4.s= recae.s,
         st5  = srv,
         st5.s= srv.s)

head(msdata)

library(tidyr)
count(msdata, year, age, discount, gender) %>% spread(year, n) 

save(msdata, file = 'data/msdata.rda')


# transition matrix -------------------------------------------------------

trans_mat <- 
  transMat(names = c('st1', 'st2', 'st3', 'st4', 'st5'),
           x = list(c(2,3,5),
                    c(4,5),
                    c(4,5),
                    c(5),
                    c()))

# graph of possible states  -----------------------------------------------

library(igraph)
as.data.frame(trans_mat) %>%
  cbind(names = c('st1', 'st2', 'st3', 'st4', 'st5')) %>%
  gather(key = "key", value = "value", -names) %>%
  filter(!is.na(value)) %>%
  graph_from_data_frame %>% 
  plot(edge.label = c('tr1', 'tr2', 'tr4', 'tr6', 'tr3', 'tr5', 'tr7', 'tr8'))

# data encoding -----------------------------------------------------------

msdata_enc <-
  msprep(
    data  = msdata,
    trans = trans_mat,
    time  = c(NA,   "st2",   "st3",   "st4",   "st5"),
    status= c(NA, "st2.s", "st3.s", "st4.s", "st5.s"),
    keep  = c('year', 'age', 'discount', 'gender')
  )

save(msdata_enc, file = 'data/msdata_enc.rda')

msdata_enc[msdata_enc$id %in% c(1, 3, 6, 1909), c(1:12)]


msdata_exp <-
  expand.covs(
    msdata_enc,
    covs = c('year', 'age', 'discount', 'gender'),
    longnames = TRUE
  )

msdata_exp[msdata_enc$id == 1, c(1:8, 11, 45:52)]

msdata_exp[msdata_enc$id == 1, c(1:8, 10, 29:44)]


# calculate events --------------------------------------------------------

events(msdata_enc) %>%
{
  .$Proportions <- round(.$Proportions,3)
  .
}

# fit a model -------------------------------------------------------------

c0 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), data = msdata_exp, method = "breslow")
msf0 <- msfit(object = c0, vartype = "greenwood", trans = trans_mat)


# results - cumulative hazards distributions

head(msf0$Haz)
tail(msf0$Haz)


# cumulative hazard plots / opposite of the survival curves --------------------------------------------------------

plot(msf0, las = 1, lty = rep(1:2, c(8, 4)),
     xlab = "Days since registration", legend.pos = 'bottomright')


# cumulative hazard plots / opposite of the survival curves in ggplot ----------------------------------------------



# in which states are we during time rises? -------------------------------

pt0 <- probtrans(msf0, predt = 0, method = "greenwood")
capture.output(summary(pt0, from = 1)) -> x
save(x, file = 'data/x.rda')
x[-c(1:4)] %>% strsplit(split=" ") %>% 
  lapply(function(y){ y[y != ""]}) %>%
  .[c(2:7, 12:17)] %>%
  lapply(as.data.frame) %>% 
  lapply(t) %>%
  do.call(rbind, .) %>%
  apply(MARGIN = 2, as.numeric) %>%
  apply(MARGIN = 2, round, 3) %>%
  as.data.frame() %>%
  select(2:7) %>%
  set_colnames(c('time', 'State 1', 'State 2', 'State 3', 'State 4', 'State 5'))
capture.output(summary(pt0, from = 2)) -> y
save(y, file = 'data/y.rda')


# state distribution over a time ------------------------------------------

plot(pt0, xlab = "Days since registration",  las = 1, type = "filled")


# a model with covariates -------------------------------------------------
# cat(colnames(msdata_exp)[-c(1:8)], sep = " + ")
cfull <- coxph(Surv(Tstart, Tstop, status) ~ year + age + discount + gender + 
                 year2008.2012.1 + year2008.2012.2 + year2008.2012.3 + year2008.2012.4 + year2008.2012.5 + year2008.2012.6 + year2008.2012.7 + #year2008.2012.8 +
                 year2013.2017.1 + year2013.2017.2 + year2013.2017.3 + year2013.2017.4 + year2013.2017.5 + year2013.2017.6 + year2013.2017.7 + #year2013.2017.8 +
                 age20.40.1 + age20.40.2 + age20.40.3 + age20.40.4 + age20.40.5 + age20.40.6 + age20.40.7 + #age20.40.8 +
                 age.40.1 + age.40.2 + age.40.3 + age.40.4 + age.40.5 + age.40.6 + age.40.7 + #age.40.8 +
                 discountyes.1 + discountyes.2 + discountyes.3 + discountyes.4 + discountyes.5 + discountyes.6 + discountyes.7 + #discountyes.8 +
                 genderfemale.1 + genderfemale.2 + genderfemale.3 + genderfemale.4 + genderfemale.5 + genderfemale.6 + genderfemale.7 + #genderfemale.8 + 
                 strata(trans),
               data = msdata_exp, method = "breslow")

# results of model with covariates ----------------------------------------
results <- coefficients(cfull) 
save(results, file = 'data/results.rda')

data.frame(
  names = names(results),
  values = results,
  stringsAsFactors = FALSE
) %>%
  dplyr::filter(grepl(".", names, fixed = TRUE)) %>%
  separate(names, sep = "\\.[0-9]{1}$", into = c('variable', 'transition')) %>%
  mutate(transition = rep(1:7, length(unique(variable)))) %>%
  spread(variable, values)

# some predictions for 2 various clients

clientA <- 
  msdata_exp %>%
  filter(
    discount == "yes", 
    gender   == "female",
    year     == "2013-2017",
    age      == "<=20"
  ) %>% 
  magrittr::extract(1:8, ) %>% # 8 rows / 8 transitions + we keep all factor levels
  {
    attr(., "trans") <- trans_mat
    .
  } %>%
  # select only covariates for prediction
  select(contains('year'), contains('age'), contains('discount'), contains('gender')) %>%
  mutate(trans = 1:8) %>% 
  mutate(strata = trans) %>%
  msfit(cfull, newdata = ., trans = trans_mat) %>%
  probtrans(pred = 0)

clientB <- 
  msdata_exp %>%
  filter(
    discount == "no", 
    gender   == "male",
    year     == "2002-2007",
    age      == "20-40"
  ) %>% 
  magrittr::extract(1:8, ) %>% # 8 rows / 8 transitions 
  {
    attr(., "trans") <- trans_mat
    .
  } %>%
  # select only covariates for prediction
  select(contains('year'), contains('age'), contains('discount'), contains('gender')) %>%
  mutate(trans = 1:8) %>% 
  mutate(strata = trans) %>%
  msfit(cfull, newdata = ., trans = trans_mat) %>%
  probtrans(pred = 0)

# compare future fate of 2 various clients
par(mfrow = c(1,2))
plot(clientA, main = "Client A", las = 1, xlab = "Days since registration", type = "filled",
     xlim = c(0, 3650)) # 10 years
plot(clientB, main = "Client B", las = 1, xlab = "Days since registration", type = "filled",
     xlim = c(0, 3650)) # 10 years
par(mfrow = c(1,1))
