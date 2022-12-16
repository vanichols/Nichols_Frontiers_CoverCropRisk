# author: gina nichols (virginia.nichols@gmail.com)
# purpose: summarise raw weather data
# created: april 7 2021
# updated feb 18 2022
# updated 7/21/2022, added wds processing 
# updated 9/9/2022 adding GDDS 'chance'


# setup -------------------------------------------------------------------


rm(list = ls())
library(tidyverse)
library(lubridate)
library(saapsim) #--bespoke function to get doy from date and vice versa
#devtools::install_github("vanichols/saapsim")
library(readxl)
library(patchwork)
library(scales)


#--april 1-17 = 91-107
#--april 18-31 = 108-120
#--may 1-17 = 121-137
#--may 18-31 = 138-151

#saf_date_to_doy("2001-05-18")


# weather -----------------------------------------------------------------

wea <- 
      read_excel("2_R-code/01_wea-and-wds/raw_ames-wea.xlsx") %>% 
  filter(year > 1988) 

max(wea)

# fall precip - sum ---------------------------------------------------------------

#--sum precip, chance over 1/2 inch? basically 100%...
prcp_oct <- 
  wea %>% 
  filter(day > saf_date_to_doy("2001-10-15"), day < 
           saf_date_to_doy("2001-11-30")) %>% 
  group_by(year) %>% 
  summarise(fall_precip_mm = sum(rain_mm)) %>% 
  mutate(fall_precip_in = fall_precip_mm / 10 / 2.54 ,
         month = "15oct-30nov")

prcp_nov <- 
  wea %>% 
  filter(day > saf_date_to_doy("2001-11-01"), day < 
           saf_date_to_doy("2001-11-30")) %>% 
  group_by(year) %>% 
  summarise(fall_precip_mm = sum(rain_mm)) %>% 
  mutate(fall_precip_in = fall_precip_mm / 10 / 2.54 ,
         month = "1nov-30nov")


prcp <- 
  prcp_oct %>%
  select(year, month, fall_precip_in) %>% 
  bind_rows(
    prcp_nov %>% select(year, month, fall_precip_in)
  ) 

prcp %>% write_csv("dat_fall-precip.csv")



# fall precip - smy manu -----------------------------------------------

prcp %>% 
  group_by(month) %>% 
  summarise(mean_in = mean(fall_precip_in)) %>% 
  mutate(mean_cm = mean_in * 2.54)



# fall precip - chance of 0.5 inch----------------------------------------------------

prcp_chance <- 
  prcp %>% 
  mutate(more_than_half_in = ifelse(fall_precip_in > 0.5, "yes", "no")) %>%
  group_by(month) %>% 
  mutate(ntot = n()) %>% 
  group_by(month, ntot, more_than_half_in) %>% 
  summarise(n = n()) %>% 
  mutate(chance_halfin = n/ntot) %>% 
  filter(more_than_half_in == "yes") %>% 
  ungroup() %>% 
  select(month, chance_halfin)

prcp_chance %>% write_csv("dat_chance-fall-precip.csv")



# fall precip - chance as function of precip needed ----------------------------------------------------

prcp_probs <- NULL

for (i in seq(from = 0.1, to = 3.5, by = 0.05)) {

  tmp <-
    prcp %>%
    mutate(fall_precip_cm = fall_precip_in * 2.54) %>%
    mutate(more_than_X = ifelse(fall_precip_cm > i, "yes", "no")) %>%
    group_by(month) %>%
    mutate(ntot = n()) %>%
    group_by(month, ntot, more_than_X) %>%
    summarise(n = n()) %>%
    mutate(chance_X = n / ntot) %>%
    filter(more_than_X == "yes") %>%
    ungroup() %>%
    select(month, chance_X) %>%
    mutate(precip_req_cm = i)
  
  prcp_probs <- 
    prcp_probs %>% 
    bind_rows(tmp)
  
}

prcp_probs %>% 
  write_csv("dat_precip-chance-fxn-rqd.csv")


# gdd ---------------------------------------------------------------------

#--say you plant after oct 15

#--sum gdd, base temp of 0, kantar porter say you need > 130 just to emerge 
# (https://fieldcropnews.com/2019/10/impact-of-planting-date-and-growing-degree-day-accumulations-in-winter-wheat/)

gdd_oct <- 
  wea %>% 
  filter(day > saf_date_to_doy("2001-10-15")) %>%
  mutate(gdd = (mint_c + maxt_c)/2,
         gdd = ifelse(gdd <0, 0, gdd)) %>% 
  group_by(year) %>% 
  summarise(gdds = sum(gdd)) %>% 
  mutate(month = "oct") 

gdd_nov <- 
  wea %>% 
  filter(day > saf_date_to_doy("2001-11-01")) %>%
  mutate(gdd = (mint_c + maxt_c)/2,
         gdd = ifelse(gdd <0, 0, gdd)) %>% 
  group_by(year) %>% 
  summarise(gdds = sum(gdd)) %>% 
  mutate(month = "nov") 

gdds <- 
  gdd_oct %>% 
  bind_rows(gdd_nov)

gdds %>%
  write_csv("dat_200gdds.csv")

chance_gdds <- 
  gdds %>% 
  mutate(more_than_200gdds = ifelse(gdds > 200, "yes", "no")) %>% 
  group_by(month) %>% 
  mutate(ntot = n()) %>%
  group_by(month, more_than_200gdds, ntot) %>% 
  summarise(n = n()) %>% 
  mutate(chance_fall200gdds = n/ntot) %>% 
  filter(more_than_200gdds == "yes") %>% 
  ungroup() %>% 
  select(month, chance_fall200gdds)


chance_gdds %>%
  write_csv("dat_chance-200gdds.csv")

# fall precip - chance as function of precip needed ----------------------------------------------------

gdd_probs <- NULL

for (i in seq(from = 50, to = 300, by = 5)) {
  
  tmp <-
    gdds %>% 
    mutate(more_than_200gdds = ifelse(gdds > i, "yes", "no")) %>% 
    group_by(month) %>% 
    mutate(ntot = n()) %>%
    group_by(month, more_than_200gdds, ntot) %>% 
    summarise(n = n()) %>% 
    mutate(chance_fall200gdds = n/ntot) %>% 
    filter(more_than_200gdds == "yes") %>% 
    ungroup() %>% 
    select(month, chance_fall200gdds) %>% 
    mutate(gdds_req = i)
  
  gdd_probs <- 
    gdd_probs %>% 
    bind_rows(tmp)
  
}

gdd_probs %>% 
  write_csv("dat_gdds-chance-fxn-rqd.csv")




# workable days -----------------------------------------------------------

wd_dat <- 
  read_csv("2_R-code/01_wea-and-wds/dat_workable-days.csv") %>% 
  mutate(two_or_more = ifelse(days_tot >= 2, "yes", "no"),
         four_or_more= ifelse(days_tot >= 4, "yes", "no"))

#--stats for manu
wd_dat %>% 
  group_by(cat2) %>% 
  summarise(wd = median(days_tot))

wd2om_chance <- 
  wd_dat %>% 
  group_by(cat2) %>% 
  mutate(ntot = n()) %>% 
  group_by(cat2, two_or_more, ntot) %>% 
  summarise(n = n()) %>% 
  mutate(chance_2om = n/ntot) %>% 
  filter(two_or_more == "yes") %>% 
  ungroup() %>% 
  select(cat2, chance_2om)

wd4om_chance <- 
  wd_dat %>% 
  group_by(cat2) %>% 
  mutate(ntot = n()) %>% 
  group_by(cat2, four_or_more, ntot) %>% 
  summarise(n = n()) %>% 
  mutate(chance_4om = n/ntot) %>% 
  filter(four_or_more == "yes") %>% 
  ungroup() %>% 
  select(cat2, chance_4om)

chance_wd <- 
  wd2om_chance %>% 
  left_join(wd4om_chance) %>% 
  mutate(cat2 = factor(cat2, levels = c("early April", "late April", "early May", "late May"))) %>% 
  arrange(cat2) 

chance_wd %>% 
  write_csv("dat_chance-wd.csv")



