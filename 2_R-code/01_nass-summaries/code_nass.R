# author: gina nichols (virginia.nichols@gmail.com)
# purpose: summarise nass data
# created: july 20 2022
# last updated: dec 15 2022


rm(list = ls())
library(tidyverse)
library(lubridate)
library(saapsim)
library(readxl)
library(patchwork)



# data --------------------------------------------------------------------

#--data downloaded from NASS

dat <- 
  read_csv("2_R-code/01_nass-summaries/dat_NASS-corn-soybean-prod.csv") %>% 
  janitor::clean_names() %>% 
  select(year, state, commodity:value)

#--top 5 states
dat %>% 
  arrange(commodity, -value)

istates <- c("IOWA", "ILLINOIS", "INDIANA", "MINNESOTA", "NEBRASKA")

#--what percentage is produced?
dat %>% 
  mutate(cat = ifelse(state %in% istates, "I-state", "other")) %>% 
  group_by(cat, commodity) %>% 
  summarise(tot = sum(value)) %>% 
  pivot_wider(names_from = cat, values_from = tot) %>% 
  janitor::clean_names() %>% 
  mutate(tot = i_state + other,
         pct_i = i_state/tot)

