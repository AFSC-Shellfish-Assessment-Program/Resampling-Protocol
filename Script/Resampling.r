# notes ----
#Determine clutch condition/thresholds for CPT resampling discussions
#Assess bottom temps relevant to thresholds 
#NOTE: Figures saved for May 2022 CPT presentation

# Erin Fedewa
# last updated: 2022/1/3

# load ----
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(cowplot)


######### Data ----

## EBS RKC Haul data with size comps 
catch_ebs <- read.csv("./Data/crabhaul_rkc.csv") %>% as_tibble()
head(catch_ebs)

#Management Areas 
sta<-read_csv("./Data/stock_stations.csv")
head(sta)

#BBRKC stations only
sta %>% 
  pull(BBRKC) %>%
  na.omit() -> BBonly

######## Data wrangling ----

## crab data EBS
catch_ebs %>%
  filter(AKFIN_SURVEY_YEAR %in% c(1980:2021),
         GIS_STATION %in% BBonly) %>% 
  select(AKFIN_SURVEY_YEAR, CRUISE, GIS_STATION, LENGTH, SHELL_CONDITION, SEX, EGG_COLOR, EGG_CONDITION,
         CLUTCH_SIZE, SAMPLING_FACTOR, DISTANCE_FISHED, NET_WIDTH, MID_LATITUDE, MID_LONGITUDE,
         HAUL_TYPE, PERFORMANCE, GEAR_TEMPERATURE) -> crab_ebs
names(crab_ebs) <- c("year", "cruise", "Station", "cl", "sc", "sex", "egg_color","egg_cond", "clutch_size", 
                     "sample_factor", "distance_fished", "net_width", "lat", "lon", 
                     "haul_type", "performance", "temperature")

#Retow stations only
crab_ebs %>%
  group_by(year, haul_type) %>%
  summarise(num_stations = n_distinct(Station))%>%
  print(n=54) #Re-tow years 1999, 2000, 2006-12, 2017, 2021

###################
#Mature females by clutch code: look at a few recent retow years 

#Total number of mature females on Leg one 
crab_ebs %>%
  filter(year %in% c(2008, 2009, 2010, 2011,2012,2017,2021),
             haul_type==3,
             sex==2,
             clutch_size!=0) %>%
  group_by(year) %>%
  summarise(num_crab = round(sum(sample_factor,na.rm = T))) 

#Mature females by clutch codes 
crab_ebs %>%
  filter(year %in% c(2008, 2009, 2010, 2011,2012,2017,2021),
         haul_type==3,
         sex==2,
          clutch_size!=0) %>%
  mutate(clutch = ifelse(egg_cond==2, "Eyed",
                         ifelse(egg_cond==4 | egg_cond==0 & clutch_size==1, "Barren",
                                ifelse(egg_cond==5, "Hatching",
                                         ifelse(egg_cond==1, "Uneyed", NA))))) %>%
  filter(!is.na(clutch)) %>%
  group_by(year, clutch) %>%
  summarise(num_crab = round(sum(sample_factor,na.rm = T))) %>%
  print(n=50)

#############
  #Mature females by clutch code timeseries (1999 on- when protocol was adopted)

crab_ebs %>%
  filter(year >= 1999,
         haul_type==3,
         sex==2,
         clutch_size!=0) %>%
  group_by(year) %>%
  mutate(num_crab = round(sum(sample_factor,na.rm = T))) %>%
  ungroup() %>%
  mutate(clutch = ifelse(egg_cond==2, "Eyed",
                         ifelse(egg_cond==4 | egg_cond==0 & clutch_size==1, "Barren",
                                ifelse(egg_cond==5, "Hatching",
                                       ifelse(egg_cond==1, "Uneyed", NA))))) %>%
  filter(!is.na(clutch)) %>%
  group_by(year, clutch) %>%
  summarise(clutch_perc = ((round(sum(sample_factor,na.rm = T)))/mean(num_crab))*100,
            total_mature = mean(num_crab)) %>%
  pivot_wider(names_from = clutch, values_from = clutch_perc) %>%
  mutate(thres_total = sum(c(Barren, Eyed, Hatching), na.rm = T)) %>%
  relocate(Uneyed, .after=thres_total) %>%
  replace(is.na(.), 0) -> clutch_timeseries 

write.csv(clutch_timeseries,"./Output/Clutch_Timeseries_Output.csv") 

#Plot Timeseries 
crab_ebs %>%
  filter(year >= 1999,
         haul_type==3,
         sex==2,
         clutch_size!=0) %>%
  group_by(year) %>%
  mutate(num_crab = round(sum(sample_factor,na.rm = T))) %>%
  ungroup() %>%
  mutate(clutch = ifelse(egg_cond==2, "Eyed",
                         ifelse(egg_cond==4 | egg_cond==0 & clutch_size==1, "Barren",
                                ifelse(egg_cond==5, "Hatching", NA)))) %>%
  filter(!is.na(clutch)) %>%
  group_by(year, clutch) %>%
  summarise(clutch_perc = ((round(sum(sample_factor,na.rm = T)))/mean(num_crab))*100) %>%
  ggplot(aes(fill=clutch, y=clutch_perc, x=year)) +
  geom_bar(position="stack", stat="identity") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        axis.text.x=element_text(size=11)) +
  ylab("Percentage of Mature Females") +
  scale_x_continuous(breaks=seq(1995, 2021, 2)) +
  geom_hline(yintercept = 10) +  #Current threshold
  #annotate(geom="text", x=2020, y=12, label="10% Threshold") +
  geom_hline(yintercept = 25, linetype="dashed") -> plot #Proposed threshold 
  #annotate(geom="text", x=2020, y=27, label="25% Threshold")  

ggsave("./Output/Clutch Timeseries Plot.png", plot=plot, height=7, width=9, units="in")

#Plot bimodal distribution...going rouge here with a new package....
clutch_timeseries %>%
  mutate(Resample = as.factor(ifelse(year %in% c(1999,2000, 2006:2012, 2017, 2021), "Yes", "No"))) -> plot2

gghistogram(plot2, x = "thres_total", rug = TRUE ,xlab="Threshold Total",
  fill = "Resample", palette = c("#00AFBB", "#E7B800"))-> phist

pdensity <- ggdensity(plot2, x = "thres_total", 
  color= "Resample", fill="Resample", palette = c("#00AFBB", "#E7B800"), alpha = .3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), position = "right")  +
  theme_half_open(11, rel_small = 1) +
  rremove("x.axis")+
  rremove("xlab") +
  rremove("x.text") +
  rremove("x.ticks") +
  rremove("legend") 

# Align the two plots and then overlay them.
aligned_plots <- align_plots(phist, pdensity, align="hv", axis="tblr")
ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]]) -> plot2

ggsave("./Output/Distribution Plot.png", plot=plot2, height=7, width=9, units="in")

############################################################
#BB bottom temps for temp/threshold regression

crab_ebs %>%
  filter(year >= 1999,
         haul_type==3) %>%
  group_by(year) %>%
  summarise(mean_temp = mean(temperature, na.rm=TRUE)) %>%
  right_join(clutch_timeseries) %>%
  mutate(Resample = as.factor(ifelse(year %in% c(1999,2000, 2006:2012, 2017, 2021), "Yes", "No"))) %>%
  ggplot(aes(mean_temp, thres_total, fill=Resample, label=year)) +
  geom_point(aes(colour=Resample), size=3) +
  geom_text(hjust=.5, vjust=1.5, size=3) +
  theme_bw() +
  ylim(0, 100) +
  ylab("Threshold Total") +
  xlab("Bristol Bay Average Bottom Temperature (C)") -> plot3

ggsave("./Output/Temperature Plot.png", plot=plot3, height=6, width=7, units="in")
  
#######################################################
#2021 Leg 1 vrs resampling size comp figures 

crab_ebs %>%
  filter(year == 2021,
         sex==2,
         clutch_size!=0) %>%
  mutate(haul_type = recode(haul_type, "3" = "Leg 1", "17" = "Resampling")) %>%
  group_by(year) %>%
  ggplot()+
  geom_histogram(aes(x=cl), binwidth=2, color = "#000000", fill = "red") +
  facet_wrap(~haul_type) +
  theme_bw() +
  xlab("Carapace Length (mm)") +
  ylab("Count") -> plot4

ggsave("./Output/Size Comp Plot.png", plot=plot4, height=6, width=8, units="in")
