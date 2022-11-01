### NOTES ----
#Purpose: 
#1) Calculate mean sample date (julian day) by year and make plots of start date and mean sample date by year

# Author: Emily Ryznar 

### LOAD PACKAGES  -----------------------------------------------------------------------

library(RColorBrewer)
library(patchwork)
library(tidyverse)
library(lubridate)

### LOAD DATA
catch_rkc <- read.csv("./Data/CRABHAUL_RKC.csv")
sta <- read.csv("./Data/stock_stations_updated.csv")

# Pull out BBRKC stations only
sta %>% 
  pull(BBRKC) %>%
  na.omit() -> BBonly

catch_rkc %>%
  filter(HAUL_TYPE == 3, GIS_STATION %in% BBonly) -> bbrkc


# Extract Julian day and year by start date
bbrkc$jday_start <- yday(mdy(bbrkc$START_DATE))
bbrkc$jday_start <- yday(mdy(bbrkc$START_DATE))
bbrkc$year <- year(mdy(bbrkc$START_DATE))


# Calculate mean sampling day and CI by year
bbrkc %>%
  group_by(year) %>%
  summarise(start_date = min(jday_start),
            mean_samp_date = mean(jday_start),
            n = length(unique(HAULJOIN)), 
            CI_samp = (sqrt((var(jday_start)/n)))*1.96) -> bbrkc_dates


# Plot scatter plot and save
ggplot() +
  geom_point(bbrkc_dates, mapping = aes(start_date, mean_samp_date), size=3) +
  geom_text_repel(bbrkc_dates, mapping = aes(start_date, mean_samp_date, label = year), max.overlaps = 22)+
  geom_text(hjust=.5, vjust=1.8, size=3) +
  theme_classic() +
  xlab("Start date (Julian day)") +
  ylab("Mean sampling date (Julian day)")

ggsave("./Figures/bbrkc_dates_scatter.png")


# Plot timeseries and save
ggplot() +
  geom_line(bbrkc_dates, mapping = aes(year, start_date, col = "#009966"))+
  geom_line(bbrkc_dates, mapping = aes(year, mean_samp_date, col = "#006699"))+
  geom_point(bbrkc_dates, mapping = aes(year, start_date, col = "#009966"))+
  geom_point(bbrkc_dates, mapping = aes(year, mean_samp_date, col = "#006699"))+
  geom_errorbar(bbrkc_dates, mapping = aes(x = year, ymin = mean_samp_date - CI_samp, ymax = mean_samp_date + CI_samp, col = "#006699"), 
                size = 0.5)+
  geom_text(hjust=.5, vjust=1.8, size=3) +
  scale_colour_manual(name = element_blank(), values = c("#009966", "#006699"), labels = c("Mean sampling date", "Start date"))+
  scale_x_continuous(breaks= seq(min(bbrkc_dates$year), max(bbrkc_dates$year), by=5),minor_breaks = NULL)+
  theme_classic() +
  xlab("Year") +
  ylab("Julian day")

ggsave("./Figures/bbrkc_dates_ts.png")

# plot mean date only
plot_mean <- bbrkc_dates %>%
  select(year, mean_samp_date, CI_samp)

xtra <- data.frame(year = 2020,
                   mean_samp_date = NA,
                   CI_samp = NA)

plot <- rbind(plot_mean, xtra) %>%
  arrange(year)

ggplot(plot, aes(year, mean_samp_date)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin = mean_samp_date - CI_samp,
                ymax = mean_samp_date + CI_samp)) +
  geom_smooth(method = "gam", se = F) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab("Mean sampling date (day of year)")
  

ggsave("./Figures/bbrkc_mean_date.png", width = 6, height = 4, units = 'in')
