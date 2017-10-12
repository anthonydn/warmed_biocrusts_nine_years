#datetime: measurement date and time (same as above, just different name)
#block: measurement block (plots were arranged in 5 blocks)
#treatment: L for lamp warming plots (+2°C); C for control

#radiation: W m-2 (in weather2012 file)
#airtemp: Air temperature, °C
#Rh: percent relative humidity
#windspeed: m s-1
#T5: soil temperature (°C) at 5 cm depth
##T15: soil temperature (°C) at 15 cm depth
#moist5: soil moisture (volumetric water content, %) at 5 cm depth (Campbell CS616)
#moist15: soil moisture (volumetric water content, %) at 15 cm depth (Campbell CS616)
#flux: net ecosystem exchange of CO2-C (µmol CO2-C m-2 s-1)
#PAR: µmol m-2 s-1 (Apogee PAR sensor)
#castle_temp_max: daily maximum air temperature (°C) from nearby Castle Valley site (http://www.wrcc.dri.edu/cgi-bin/cliMAIN.pl?ut1241)
#castle_temp_min: daily minimum air temperature (°C) from nearby Castle Valley site
#castle_temp: daily mean air temperature (°C) from nearby Castle Valley site
#rain: 24-hr rainfall totals (mm)

#(2012 only)
#CSVW_2: Shortened Campbell CS616 probe (2 cm depth). Average of 9 probes, three each in blocks 1,2,5.
#The values for soil moisture for the short probes have had an equation applied that makes them read the same as the long probes given the same conditions.
#CVW_2: Regular Campbell CS616 probe (2 cm depth). Average of 6 probes, three each in blocks 3,4.
#DVW_5: Decagon EC-5 probe (5 cm depth). Average of 15 probes, 3 each block
#DVW_10: Decagon EC-5 probe (10 cm depth). Average of 15 probes, 3 each block
#T10: soil temperature (°C) at 15 cm depth

library(tidyverse)
library(lubridate)

proj_dir <- "https://anthony.darrouzet-nardi.net/data/warmed_biocrusts_nine_years/"

##Initial dataset assembly. Raw data not provided for this, but code is shown to document approach
# 2006 - import data that has been cleaned for C & L from 2015 Biogeochemistry paper
# raw0607 <- readr::read_csv("C:/Dropbox/USGS Projects/Autochambers/Data/autochambers_2005to2007/Fourth and hopefully final cleaning of data/homer_marge_C_L_clean_only.csv") %>% 
#   mutate(datetime = round_date(datetime, "hour") %>% force_tz("MST")) %>% 
#   unite(tb, treatment, block) %>% 
#   unique
# 
# # 2006 - join to a skeleton frame with all time points present and export for cleaning
# ds <- seq(min(raw0607$datetime), max(raw0607$datetime), by = "hour")
# acfill_wide <- 
#   data.frame(datetime = rep(ds, 20), tb = rep(unique(raw0607$tb), ea = length(ds))) %>% 
#   left_join(raw0607) %>%
#   spread(tb, flux) %>% 
#   write.csv("C:/Dropbox/USGS Projects/Autochambers/Data/autochambers_2005to2007/Fourth and hopefully final cleaning of data/acfill_wide06.csv",
#     row.names = F, na = "")
# 
# # 2012 -import data, combine homer and marge
# marge.raw <- rexc("Marge2012.xlsx",2)
# homer.raw <- rexc("Homer2012.xlsx",2) %>% unique #homer had some duplicated rows
# marge.raw$system <- "marge"
# homer.raw$system <- "homer"
# ac2012.raw <- rbind(marge.raw, homer.raw) %>% 
#   mutate(datetime = round_date(datetime, "hour") %>% force_tz("MST")) %>%
#   select(datetime, system, chamber, flux)
# 
# # 2012 - create data frame with rows for every hour in the measurement period
# ds <- seq(min(ac2012.raw$datetime), max(ac2012.raw$datetime), by = "hour")
# acfill_wide <- data.frame(
#     datetime = rep(ds, 20),
#     system = rep(c("homer", "marge"), ea = 10 * length(ds)),
#     chamber = rep(1:10, 2, ea = length(ds))) %>% 
#   left_join(ac2012.raw) %>%
#   left_join(rexc("chamber codes.xlsx")) %>%
#   arrange(datetime, block, treatment) %>%
#   select(datetime, block, treatment, flux) %>%
#   unite(tb, treatment, block) %>% 
#   spread(tb, flux)
# 
# write.csv(acfill_wide, paste(proj_dir, "acfill_wide.csv", sep = ''), row.names = F)

### AT THIS POINT IN THE SCRIPT, I HAND-PROCESSED THE DATA TO ELIMINATE BAD POINTS
# I resaved as an xlsx, then used alt-hls to color cells in excel to assist in finding points

#####WEATHER MERGE

## 2006
ac2006 <- readr::read_csv(paste0(proj_dir, "acfill_wide06_cleaned.csv")) %>% 
  gather(tb, flux, -datetime) %>%
  separate(tb, c("treatment", "block")) %>% 
  mutate(datetime = round_date(datetime, "hour") %>% force_tz("MST"))

weather2006 <- rexc("SoilMet_0607.xlsx") %>% 
  filter(!(minute(datetime) %in% 29:30)) %>% 
  mutate(datetime = round_date(datetime, "hour") %>% force_tz("MST"), 
    date = date(datetime)) %>% 
  select(datetime, airtemp = airtemp, Rh = Rh, windspeed = windspeed, 
    rain = rain) %>%
  left_join(rexc("PAR0607.xlsx") %>% 
    transmute(datetime = round_date(hour, "hour") %>% force_tz("MST"), PAR)) %>% 
  group_by(date = date(datetime)) %>% 
  mutate(rain = sum(rain, na.rm = TRUE)) %>% 
  ungroup %>% 
  left_join(rexc("castle_valley_weather.xlsx") %>% 
  transmute(date = force_tz(date(date), "MST"), castle_temp_max = (temp_max-32)/1.8,
    castle_temp_min = (temp_min-32)/1.8,
    castle_temp = (temp-32)/1.8)) %>% 

soil_temp_moist2006 <- rexc("SoilMet_0607.xlsx") %>% 
  mutate(datetime = round_date(datetime, "hour") %>% force_tz("MST")) %>% 
  select(datetime, contains("T_5"), contains("T_15"), contains("CVW"),
    -contains("CC")) %>%
  gather(., ... = -datetime) %>%
  separate(key, c("block", "treatment", "sensor", "depth")) %>%
  group_by(datetime, treatment, sensor, depth) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  unite(sensor, sensor, depth) %>%
  spread(sensor, value) %>% 
  rename(T5 = T_5, T15 = T_15) %>%
  ungroup

## 2012
ac2012 <- rexc("acfill_wide12_cleaned.xlsx") %>%
  gather(treatment, flux, -datetime) %>% 
  separate(treatment, c("treatment", "block")) %>%
  mutate(datetime = round_date(datetime, "hour") %>% force_tz("MST")) %>% 
  filter(datetime >= as.POSIXct("2012-05-01 00:00:00", "MST"))

weather2012 <- rexc("weather2012.xlsx") %>%
  mutate(datetime = round_date(datetime, "hour") %>% force_tz("MST")) %>% 
  select(datetime, airtemp = AirTemp, Rh = RH, windspeed = WindSpd,
    PAR = PAR_A, rain = Rain_mm) %>%  
  group_by(date = date(datetime)) %>% 
  mutate(rain = sum(rain, na.rm = TRUE)) %>% 
  ungroup

soil_temp_moist2012 <- rexc("SoilMet_1203to1412.xlsx", 2) %>%
  mutate(datetime = round_date(TIMESTAMP, "hour") %>% force_tz("MST")) %>%
  select(datetime, contains("T_5"), contains("T_10"), contains("CVW_2"),
    contains("CSVW_2"), -contains("Peri"),
    contains("DVW_5"), contains("DVW_10"), -contains("WX")) %>%
  gather(., ... = -datetime) %>%
  separate(key, c("block", "treatment", "sensor", "depth")) %>%
  group_by(datetime, treatment, sensor, depth) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  unite(sensor, sensor, depth) %>%
  spread(sensor, value) %>% 
  rename(T5 = T_5, T10 = T_10) %>%
  ungroup

#######IMPUTATION SETUP

#set up wide data frame for imputation

acw_wide06 <- ac2006 %>% 
  unite(tb, treatment, block) %>% 
  spread(tb, flux) %>% 
  left_join(weather2006) %>% 
  left_join(soil_temp_moist2006 %>%  
    gather(sensor, value, -datetime, -treatment) %>% 
    unite(ts, treatment, sensor) %>% 
    spread(ts, value)) %>% 
  mutate(hour = as.numeric(hour(datetime)))

acw_wide <- ac2012 %>%  
  unite(tb, treatment, block) %>% 
  spread(tb, flux) %>% 
left_join(weather2012) %>% 
left_join(soil_temp_moist2012 %>%  
  gather(sensor, value, -datetime, -treatment) %>% 
    unite(ts, treatment, sensor) %>% 
    spread(ts, value)) %>% 
  mutate(hour = as.numeric(hour(datetime)))

#function for adding lagged variables to aid in imputation
lagger <- function(data, col, lags = 24) {
  for (i in col) { for (j in lags) { x <- data[[i]]
    data[[paste(i, "_", j, "before", sep = '')]] <- c(rep(NA, j), x[1:(length(x) - j)])
    data[[paste(i, "_", j, "after", sep = '')]] <- c(x[(1+j):(length(x))], rep(NA, j))}}
  data}

#all 2006 data
acimp0607_preimpute <- acw_wide06 %>% 
  lagger(c("C_B1", "L_B2", "W_B3", "LW_B2", "C_T5", "C_CVW_5"), c(24,48,72)) %>%
  data.frame

#2/19-11/17 (2013)
acimp13_preimpute <- acw_wide %>% 
  filter(datetime >= as.POSIXct("2013-02-19 00:00:00", "MST"),
    datetime <= as.POSIXct("2013-11-17 00:00:00", "MST")) %>%
  select(-date) %>% 
  lagger(c("C_B1", "L_B2", "W_B3", "LW_B2", "C_T5", "C_CSVW_2"), c(24,48,72)) %>%
  data.frame

#2/14-11/17 (2014)
acimp14_preimpute <- acw_wide %>% 
  filter(datetime >= as.POSIXct("2014-02-19 00:00:00", "MST"),
    datetime <= as.POSIXct("2014-11-17 00:00:00", "MST")) %>%
  select(-date) %>% 
  lagger(c("C_B1", "L_B2", "W_B3", "LW_B2", "C_T5", "C_CSVW_2"), c(24,48,72)) %>%
  data.frame

## IMPUTATION
library(doParallel)
registerDoParallel(cores = 7)
system.time(acimp0607_raw <- missForest::missForest(select(acimp0607_preimpute, -datetime), ntree = 100, parallelize = "forests")) # 7 hours
system.time(acimp13_raw <- missForest::missForest(select(acimp13_preimpute, -datetime), ntree = 100, parallelize = "forests")) # 2.4 hours
system.time(acimp14_raw <- missForest::missForest(select(acimp14_preimpute, -datetime), ntree = 100, parallelize = "forests")) # 2 hours

acimp0607 <- bind_cols(acimp0607_preimpute["datetime"], acimp0607_raw$ximp)
acimp13 <- bind_cols(acimp13_preimpute["datetime"], acimp13_raw$ximp)
acimp14 <- bind_cols(acimp14_preimpute["datetime"], acimp14_raw$ximp)

save(acimp0607_raw, acimp13_raw, acimp14_raw, acimp0607, acimp13, acimp14, file = paste(proj_dir, "acimp061314.Rdata", sep = ''))







