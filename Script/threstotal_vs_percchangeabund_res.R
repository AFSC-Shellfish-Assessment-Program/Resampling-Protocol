### NOTES ----
#Purpose: 
#1) Producing IDW maps for mature females that haven't completed the molt/mate cycle
#2) Producing #s and scatter plot threshold total of mature females that haven't completed the molt/mate cycle vs. 
#   % change in abundance between leg 1 mature female abundance and resampling abundance in retow years

# Author: Emily Ryznar ()

### LOAD PACKAGES & DATA -----------------------------------------------------------------------

library(RColorBrewer)
library(patchwork)
library(tidyverse)

## Crab specimen data- AKFIN download 
catch_rkc <- read.csv("./Data/CRABHAUL_RKC.csv") %>% as_tibble()

##Pulling out survey years from crab haul data, storing them in new column AKFIN_SURVEY_YEAR
catch_rkc%>%
  mutate(START_DATE = lubridate::mdy(START_DATE))%>%
  mutate(AKFIN_SURVEY_YEAR = lubridate::year(START_DATE))-> catch_rkc


##Adding NAs for WIDTH and LENGTH for hauls that don't have them so function below runs
catch_rkc$WIDTH_1MM <- NA

## Survey strata data- AKFIN download 
strata_rkc <- read.csv("./Data/STRATA_RKC_NEWTIMESERIES.csv") %>% as_tibble()

## Stock stations
sta <- read.csv("./Data/stock_stations_updated.csv")

#############################################------------------------------------------------
### Generate function to calculate abundance and biomass each year (will need to step through years with function below for full timeseries, see below)
bio_abund <- function(data_crab, data_strata, stock, years){
  
  #District area lookup table by stock, pull districts by stock specified
  data.frame(STOCK = c("BBRKC", 
                       rep("PribRKC", 2), 
                       rep("PribBKC", 2), 
                       rep("StMattBKC", 2),
                       rep("TannerW", 3), 
                       "TannerE", 
                       rep("Snow", 3), 
                       rep("Hybrid", 3), 
                       rep("Hair", 4), 
                       rep("Allstations", 10),
                       rep("NorthRKC",1)),
             DISTRICT = c("Bristol Bay", 
                          "Pribilof MTCA", "Pribilof Single",
                          "Pribilof MTCA", "Pribilof Single",
                          "St. Matthew MTCA", "St. Matthew Single",
                          "Pribilof MTCA", "St. Matthew MTCA", "West 166",
                          "East 166",
                          "Pribilof MTCA", "Single", "St. Matthew MTCA",
                          "Pribilof MTCA", "Single", "St. Matthew MTCA",
                          "Bristol Bay", "Northern Unstratified", "Pribilof MTCA", "Pribilof Single",
                          "Bristol Bay", "Northern Unstratified", "Pribilof MTCA","Pribilof Single",      
                          "BKC Unstratified", "St. Matthew MTCA", "St. Matthew Single", "East 166",             
                          "West 166", "Single",("Northern Unstratified"))) %>%
    dplyr::filter(STOCK == stock) %>%
    pull(DISTRICT) -> dist
  
  #Pull stock stations from strata tables using stock districts
  data_strata %>%
    dplyr::filter(DISTRICT %in% dist) %>%
    pull(STATION_ID) %>%
    unique() -> stock_stations
  
  #Specify retow stations for BBRKC, pull by year
  retow_sta <- read.csv("./Data/retow_sta.csv") ### NEED TO ADD RETOW STATIONS EACH YEAR
  
  retow_sta %>%
    dplyr::filter(SURVEY_YEAR %in% years) %>%
    dplyr::select(GIS_STATION) %>%
    pull() -> retow_stations
  
  #Create lookup table for male cutlines 
  mat_lookup <- tibble(stock = c("BBRKC", "PribRKC", "PribBKC", "StMattBKC", "TannerE", "TannerW", "Snow", "Hybrid", "Hair","NorthRKC"),
                       cutline = c(120, 120, 120, 105, 113, 103, 95, 95, NA,120),
                       legal = c(135, 135, 135, 120, 120, 110, 78, 78, 83,135),
                       recruit = c(134, 134, 134, 119, 124, 124, 101, 101, NA, 134))
  
  #Create maturity levels
  if (stock != "Hair"){
    data_crab %>%
      dplyr::filter(SEX %in% 1:2,
                    AKFIN_SURVEY_YEAR %in% years) %>%
      mutate(MAT_SEX = case_when((SEX == 1 & stock %in% c("BBRKC", "PribRKC", "PribBKC", "StMattBKC","NorthRKC") & LENGTH_1MM >= mat_lookup$cutline[mat_lookup$stock == stock]) ~ "Mature Male",
                                 (SEX == 1 & stock %in% c("BBRKC", "PribRKC", "PribBKC", "StMattBKC","NorthRKC") & LENGTH_1MM < mat_lookup$cutline[mat_lookup$stock == stock]) ~ "Immature Male",
                                 (SEX == 1 & stock %in% c("TannerE", "TannerW", "Snow", "Hybrid") & WIDTH_1MM >= mat_lookup$cutline[mat_lookup$stock == stock]) ~ "Mature Male",
                                 (SEX == 1 & stock %in% c("TannerE", "TannerW", "Snow", "Hybrid") & WIDTH_1MM < mat_lookup$cutline[mat_lookup$stock == stock]) ~ "Immature Male",
                                 (SEX == 2 & CLUTCH_SIZE >= 1) ~ "Mature Female",
                                 (SEX == 2 & CLUTCH_SIZE == 0) ~ "Immature Female")) -> mature #for mature/immature males and females
    data_crab %>%  
      dplyr::filter(SEX %in% 1, 
                    AKFIN_SURVEY_YEAR %in% years) %>%
      mutate(MAT_SEX = case_when((stock %in% c("BBRKC", "PribRKC", "PribBKC", "StMattBKC","NorthRKC") & LENGTH_1MM >= mat_lookup$legal[mat_lookup$stock == stock]) ~ "Legal Male",
                                 (stock %in% c("BBRKC", "PribRKC", "PribBKC", "StMattBKC","NorthRKC") & LENGTH_1MM <= mat_lookup$recruit[mat_lookup$stock == stock]) ~ "Pre-recruit Male", 
                                 (stock %in% c("TannerE", "TannerW", "Snow", "Hybrid") & WIDTH_1MM >= mat_lookup$legal[mat_lookup$stock == stock]) ~ "Legal Male",
                                 (stock %in% c("TannerE", "TannerW", "Snow", "Hybrid") & WIDTH_1MM <= mat_lookup$recruit[mat_lookup$stock == stock]) ~ "Pre-recruit Male")) %>% #for legal and pre-recruits
      rbind(mature) -> mat_spec #join "mature" dataframe to legal/prerecruit dataframe to create overlapping size classes
    
  } else{
    data_crab %>%
      dplyr::filter(SEX %in% 1:2,
                    AKFIN_SURVEY_YEAR %in% years) %>%
      mutate(MAT_SEX = case_when((SEX == 1 & LENGTH_1MM >= mat_lookup$legal[mat_lookup$stock == stock]) ~ "Legal Male",
                                 (SEX == 1 & LENGTH_1MM < mat_lookup$legal[mat_lookup$stock == stock]) ~ "Sublegal Male",
                                 (SEX == 2) ~ "Female")) -> mat_spec
  }
  
  #Calculate CPUE by GIS STATION, maturity, and year
  mat_spec %>%
    dplyr::filter(GIS_STATION %in% stock_stations) %>%
    mutate(CPUE = SAMPLING_FACTOR/AREA_SWEPT,
           CPUE_KG = (SAMPLING_FACTOR * CALCULATED_WEIGHT_1MM) / AREA_SWEPT / 1000)%>%
    group_by(AKFIN_SURVEY_YEAR, HAUL_TYPE, GIS_STATION, MAT_SEX)%>%
    dplyr::summarise(CPUE = sum(CPUE),
                     CPUE_KG =sum(CPUE_KG)) -> cpue
  
  #Pull out unique haul types by year in order to include in zero-catch station dataframe below
  data_crab%>%
    dplyr::filter(AKFIN_SURVEY_YEAR %in% years) %>%
    distinct(HAUL_TYPE)->HT
  
  #Conditionally specifying maturity/sex combos for each stock to expand grid below
  if (stock != "Hair"){
    mat_sex_combos <- c("Mature Male", "Immature Male", "Mature Female", 
                        "Immature Female", "Legal Male", "Pre-recruit Male")
  } else{
    mat_sex_combos <- c("Sublegal Male", "Legal Male", "Female")
  }
  
  #Join to zero catch stations, summarize 
  cpue %>%
    right_join(expand_grid(MAT_SEX = mat_sex_combos,
                           HAUL_TYPE = as.numeric(levels(factor(HT$HAUL_TYPE))),
                           data_strata %>%
                             dplyr::filter(SURVEY_YEAR %in% years,
                                           STATION_ID %in% stock_stations) %>% 
                             distinct(SURVEY_YEAR, STATION_ID, STRATUM, TOTAL_AREA) %>%
                             rename_all(~c("GIS_STATION", "AKFIN_SURVEY_YEAR",
                                           "STRATUM", "TOTAL_AREA")))) %>%
    replace_na(list(CPUE = 0, CPUE_KG=0))-> station_haul_cpue
  
  cpue %>%
    right_join(expand_grid(MAT_SEX = mat_sex_combos,
                           HAUL_TYPE = as.numeric(levels(factor(HT$HAUL_TYPE))),
                           data_strata %>%
                             dplyr::filter(SURVEY_YEAR %in% years,
                                           STATION_ID %in% stock_stations) %>% 
                             distinct(SURVEY_YEAR, STATION_ID, STRATUM, TOTAL_AREA) %>%
                             rename_all(~c("GIS_STATION", "AKFIN_SURVEY_YEAR",
                                           "STRATUM", "TOTAL_AREA")))) %>%
    replace_na(list(CPUE = 0, CPUE_KG=0))%>%
    group_by(AKFIN_SURVEY_YEAR, GIS_STATION, MAT_SEX) %>%
    nest() %>%
    #Females: replacing original stations with resampled stations in retow yrs for BBRKC females
    mutate(data = purrr::map2(data, MAT_SEX, function(data, mat_sex) {
      if(17 %in% data$HAUL_TYPE & stock == "BBRKC" & (mat_sex == "Mature Female"|mat_sex == "Immature Female")
         & GIS_STATION %in% retow_stations) 
      {data %>% dplyr::filter(HAUL_TYPE == 17) -> x} else{x <- data %>% dplyr::filter(HAUL_TYPE != 17)}
      return(x)  
    })) %>%
    unnest() -> station_haul_cpue_res
  
  #Sum across haul, scale abundance, biomass, and variance to strata, then sum across strata and calc CIs
  station_haul_cpue %>%
    filter(MAT_SEX == "Mature Female") %>%
    group_by(AKFIN_SURVEY_YEAR, GIS_STATION, STRATUM, MAT_SEX, TOTAL_AREA, HAUL_TYPE) %>% 
    dplyr::summarise(CPUE = sum(CPUE), CPUE_KG = sum(CPUE_KG))%>%
    #Scale to abundance by strata
    group_by(AKFIN_SURVEY_YEAR, STRATUM, MAT_SEX, HAUL_TYPE) %>%
    dplyr::summarise(AREA = TOTAL_AREA,
                     MEAN_CPUE = mean(CPUE),
                     N_CPUE = n(),
                     VAR_CPUE = (var(CPUE)*(AREA^2))/N_CPUE,
                     MEAN_CPUE_KG = mean(CPUE_KG),
                     N_CPUE_KG = n(),
                     VAR_CPUE_KG = (var(CPUE_KG)*(AREA^2))/N_CPUE_KG,
                     ABUNDANCE = (MEAN_CPUE * AREA),
                     BIOMASS = (MEAN_CPUE_KG * AREA))%>%
    distinct() %>%
    #Sum across strata
    group_by(AKFIN_SURVEY_YEAR, MAT_SEX, HAUL_TYPE) %>%
    dplyr::summarise(AREA=sum(AREA),
                     MEAN_CPUE = sum(MEAN_CPUE),
                     SD_CPUE = sqrt(sum(VAR_CPUE)),
                     N_CPUE = sum(N_CPUE),
                     MEAN_CPUE_KG = sum(MEAN_CPUE_KG),
                     SD_CPUE_KG = sqrt(sum(VAR_CPUE_KG)),
                     N_CPUE_KG = sum(N_CPUE_KG),
                     ABUNDANCE = sum(ABUNDANCE),
                     ABUNDANCE_CI = 1.96*(SD_CPUE),
                     BIOMASS = sum(BIOMASS),
                     BIOMASS_CI = 1.96*(SD_CPUE_KG)) %>%
    ungroup() -> leg1_out
  
  leg1 <- leg1_out %>%
          filter(HAUL_TYPE == 3)
  
  
  station_haul_cpue_res %>%
    filter(MAT_SEX == "Mature Female") %>%
    group_by(AKFIN_SURVEY_YEAR, GIS_STATION, STRATUM, MAT_SEX, TOTAL_AREA) %>% 
    dplyr::summarise(CPUE = sum(CPUE), CPUE_KG = sum(CPUE_KG))%>%
    #Scale to abundance by strata
    group_by(AKFIN_SURVEY_YEAR, STRATUM, MAT_SEX) %>%
    dplyr::summarise(AREA = TOTAL_AREA,
                     MEAN_CPUE = mean(CPUE),
                     N_CPUE = n(),
                     VAR_CPUE = (var(CPUE)*(AREA^2))/N_CPUE,
                     MEAN_CPUE_KG = mean(CPUE_KG),
                     N_CPUE_KG = n(),
                     VAR_CPUE_KG = (var(CPUE_KG)*(AREA^2))/N_CPUE_KG,
                     ABUNDANCE = (MEAN_CPUE * AREA),
                     BIOMASS = (MEAN_CPUE_KG * AREA))%>%
    distinct() %>%
    #Sum across strata
    group_by(AKFIN_SURVEY_YEAR, MAT_SEX) %>%
    dplyr::summarise(AREA=sum(AREA),
                     MEAN_CPUE = sum(MEAN_CPUE),
                     SD_CPUE = sqrt(sum(VAR_CPUE)),
                     N_CPUE = sum(N_CPUE),
                     MEAN_CPUE_KG = sum(MEAN_CPUE_KG),
                     SD_CPUE_KG = sqrt(sum(VAR_CPUE_KG)),
                     N_CPUE_KG = sum(N_CPUE_KG),
                     ABUNDANCE = sum(ABUNDANCE),
                     ABUNDANCE_CI = 1.96*(SD_CPUE),
                     BIOMASS = sum(BIOMASS),
                     BIOMASS_CI = 1.96*(SD_CPUE_KG)) %>%
    ungroup() -> res_out
  
  prepost_res <- data_frame(YEAR = res_out$AKFIN_SURVEY_YEAR, PERC_CHANGE = ((res_out$ABUNDANCE-leg1$ABUNDANCE)/leg1$ABUNDANCE)*100)
 
}


#################################################################--------------------------------------------------------------------
#Run functions for each species and stock
#Update years
years <- c(1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 
          2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021)


#Red King Crab-------------------------------------------------------------------
data_crab <- catch_rkc
data_strata <- strata_rkc

#BBRKC
stock <- "BBRKC"

#Generate full data table (abundance and biomass per year)
years%>%
  map_df(~bio_abund(data_crab, data_strata, stock, .x)) -> perc_change_out 




######### Data ----

## EBS RKC Haul data with size comps 
catch_ebs <- catch_rkc %>% as_tibble()
head(catch_ebs)

#BBRKC stations only
sta %>% 
  pull(BBRKC) %>%
  na.omit() -> BBonly

## crab data EBS
catch_ebs %>%
  filter(AKFIN_SURVEY_YEAR %in% c(1980:2021),
         GIS_STATION %in% BBonly) %>% 
  dplyr::select(AKFIN_SURVEY_YEAR, CRUISE, GIS_STATION, LENGTH, SHELL_CONDITION, SEX, EGG_COLOR, EGG_CONDITION,
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

#############
#Mature females by clutch code timeseries (1999 on- when protocol was adopted)
retow_years <- c(1999, 2000, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2017, 2021)

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
  replace(is.na(.), 0) %>%
  ungroup() %>%
  dplyr::select(thres_total) %>%
  cbind(perc_change_out) -> res_timeseries

res_timeseries$RES <-ifelse(res_timeseries$YEAR %in% retow_years, "Y", "N")

ggplot(res_timeseries %>%
         dplyr::filter(RES == "Y"), aes(thres_total, PERC_CHANGE, label=YEAR)) +
  geom_point(size=3) +
  geom_text(hjust=.5, vjust=1.8, size=3) +
  theme_classic() +
  ylim(-5, 110) +
  xlab("Threshold total") +
  ylab("% Change in Abundance") -> plot3

ggsave("./Figures/thres_vs_percchange.png", plot=plot3, height=6, width=7, units="in")


res_timeseries %>%
  filter(RES == "Y") -> res_timeseries

write.csv(res_timeseries, "./Output/res_timeseries.csv")
