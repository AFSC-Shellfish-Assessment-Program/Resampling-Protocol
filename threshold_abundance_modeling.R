## compare difference in mature female abundance between 
## sampling events vs proportion not molted-mated on leg1

library(tidyverse)
library(mgcv)
library(rstan)
library(brms)
library(bayesplot)
library(bayesdfa)
source("./Script/stan_utils.R")

theme_set(theme_bw())

# load data 
dat <- read.csv("./Output/res_timeseries.csv", row.names = 1)

head(dat)



## fit a brms model ------------------------------------------

## Define model formula

formula <-  bf(PERC_CHANGE ~ s(thres_total, k = 4))

## fit 
abundance_brm <- brm(formula,
                  data = dat,
                  cores = 4, chains = 4, iter = 3000,
                  save_pars = save_pars(all = TRUE),
                  control = list(adapt_delta = 0.99, max_treedepth = 10))


saveRDS(abundance_brm, file = "Output/abundance_brm.rds")

abundance_brm <- readRDS("./output/abundance_brm.rds")
check_hmc_diagnostics(abundance_brm$fit)
neff_lowest(abundance_brm$fit)
rhat_highest(abundance_brm$fit)
summary(abundance_brm)
bayes_R2(abundance_brm) # 8.4%!

plot(conditional_effects(abundance_brm), ask = FALSE)

# plot with points
## 95% CI
ce1s_1 <- conditional_effects(abundance_brm, effect = "thres_total", re_formula = NA,
                              probs = c(0.025, 0.975))
## 90% CI
ce1s_2 <- conditional_effects(abundance_brm, effect = "thres_total", re_formula = NA,
                              probs = c(0.05, 0.95))
## 80% CI
ce1s_3 <- conditional_effects(abundance_brm, effect = "thres_total", re_formula = NA,
                              probs = c(0.1, 0.9))
dat_ce <- ce1s_1$thres_total
dat_ce[["upper_95"]] <- dat_ce[["upper__"]]
dat_ce[["lower_95"]] <- dat_ce[["lower__"]]
dat_ce[["upper_90"]] <- ce1s_2$thres_total[["upper__"]]
dat_ce[["lower_90"]] <- ce1s_2$thres_total[["lower__"]]
dat_ce[["upper_80"]] <- ce1s_3$thres_total[["upper__"]]
dat_ce[["lower_80"]] <- ce1s_3$thres_total[["lower__"]]



g1 <- ggplot(dat_ce) +
  aes(x = effect1__, y = estimate__) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = "grey90") +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90), fill = "grey85") +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "grey80") +
  geom_line(size = 1, color = "red3") +
  labs(x = "% not completed molt-mate", y = "% change in abundance on resample") +
  geom_text(data = dat, aes(x=thres_total, y=PERC_CHANGE, label = YEAR))

print(g1)


ggsave("./Figures/resample_abundance_change_brms.png", width = 6, height = 4, units = 'in')

## compare dates with % changed

dates <- read.csv("./Output/bbrkc_dates.csv")

dates <- dates %>%
  select(year, mean_samp_date) %>%
  rename(YEAR = year)

dat <- left_join(dat, dates)

ggplot(dat, aes(mean_samp_date, thres_total)) +
  geom_point()

ggplot(dat, aes(mean_samp_date, PERC_CHANGE)) +
  geom_point()
