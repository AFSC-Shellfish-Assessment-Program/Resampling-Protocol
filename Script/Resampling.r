# notes ----
# Determine clutch condition/thresholds for CPT resampling discussions
#Assess bottom temps relevant to thresholds 

# Erin Fedewa
# last updated: 2022/1/3

# load ----
library(tidyverse)

#setwd("~/CPT Mtgs/Jan 2022")

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
         HAUL_TYPE, PERFORMANCE) -> crab_ebs
names(crab_ebs) <- c("year", "cruise", "Station", "cl", "sc", "sex", "egg_color","egg_cond", "clutch_size", 
                     "sample_factor", "distance_fished", "net_width", "lat", "lon", 
                     "haul_type", "performance")

#Retow stations only
crab_ebs %>%
  group_by(year, haul_type) %>%
  summarise(num_stations = sum(nrow(Station)))%>%
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
  #Mature females by clutch code timeseries (1995 on)

crab_ebs %>%
  filter(year >= 1995,
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
  summarise(clutch_perc = ((round(sum(sample_factor,na.rm = T)))/mean(num_crab))*100) %>%
  pivot_wider(names_from = clutch, values_from = clutch_perc) %>%
  mutate(thres_total = sum(c(Barren, Eyed, Hatching), na.rm = T)) %>%
  relocate(Uneyed, .after=thres_total) %>%
  replace(is.na(.), 0) -> clutch_timeseries 

write.csv(clutch_timeseries,"Clutch_Timeseries_Output.csv") 

#Plot 
crab_ebs %>%
  filter(year >= 1995,
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
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank()) +
  ylab("Percentage of Mature Females") +
  scale_x_continuous(breaks=seq(1995, 2021, 5)) +
  geom_hline(yintercept = 10) + #Current threshold
  geom_hline(yintercept = 25, linetype="dashed") #Proposed threshold

############################################################
#BB bottom temps for temp/threshold regression












#######################################################
#Variance around threshold estimates (error bars on plot?)



#New read.csv naming convention for github
#Write figure with resampled and threshold text 
