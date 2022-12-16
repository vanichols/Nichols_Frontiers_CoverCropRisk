#--summarise SAs of termination
#--aug 26 2022
#--updated 12/13/2022, use new worksheets with 100 GDDs probabilities
#--updated 12/14/2022, added a 50% chance of 10% yield reduction

# setup -------------------------------------------------------------------


#--getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
curdir <- paste(getwd())

rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(saapsim)
library(janitor)
#devtools::install_github("vanichols/saapsim")


library(patchwork)
library(scales)
library(ggthemes)
library(ggpubr)
# data --------------------------------------------------------------------

#--if you plant a cover crop but get no money for it
dcorn_cc <- 
  read_excel("../../1_decision-analysis-workbooks/UPDATED_cover-crop-decision-tree-raw-100 GDDs - 50pct.xlsx",
           sheet = "Maize-no-societal-benefits-SA",
           skip = 5) %>% 
  select(11:12) %>%
  clean_names() %>% 
  remove_empty("rows") %>% 
  distinct() %>% 
  filter(spring != "NA",
         !is.na(spring)) %>% 
  rename(value = 2) %>% 
  mutate(spring = str_replace(spring, ", ", "\n")) %>% 
  mutate(scenario  = "Rye - Maize",
         decision_id = c("2", "3", "2/3"))

#--if you don't
dcorn_no <- 
  read_excel("../../1_decision-analysis-workbooks/UPDATED_cover-crop-decision-tree-raw-100 GDDs - 50pct.xlsx",
             sheet = "Maize-no-societal-benefits-SA",
             skip = 5) %>% 
  select(2:3) %>%
  clean_names() %>% 
  remove_empty("rows") %>% 
  distinct() %>% 
  rename(value = 2) %>% 
  filter(rye_decision == "don't plant rye") %>% 
  mutate(scenario  = "Rye - Maize",
         decision_id = c("1"))


#--if you plant a cover crop with perfect information
dcorn_cc_perfect <- 
  read_excel("../../1_decision-analysis-workbooks/UPDATED_cover-crop-decision-tree-raw-100 GDDs - 50pct.xlsx",
             sheet = "Maize-yield-drag-perfect-info",
             skip = 5) %>% 
  select(11:12) %>%
  clean_names() %>% 
  remove_empty("rows") %>% 
  distinct() %>% 
  filter(spring != "NA",
         !is.na(spring)) %>% 
  rename(value = 2) %>% 
  mutate(spring = str_replace(spring, ", ", "\n")) %>% 
  mutate(scenario  = "Rye - Maize",
         perfect_info = "Y",
         decision_id = c("2", "3", "2/3")) %>% 
  filter(spring != "cover crop dies")



#--0.405 ha in an acre

dat <- 
  dcorn_cc_perfect %>% 
  bind_rows(dcorn_no) %>% 
  bind_rows(dcorn_cc) %>% 
  mutate(desc = 
           case_when(
             decision_id == "1"|decision_id == "4" ~ "Do not plant a cover crop",
             decision_id == "2"|decision_id == "5" ~ "Plant cover crop, plan to terminate early April",
             decision_id == "3"|decision_id == "6" ~ "Plant cover crop, plan to terminate late April",
             decision_id == "2/3"|decision_id == "5/6" ~ "Plant cover crop, fails to establish",
             TRUE ~ "hmm"
             )) %>% 
  select(desc, scenario, perfect_info, decision_id, desc, value) %>% 
  mutate(value_ha = value/0.405,
         perfect_info = ifelse(is.na(perfect_info), "N", "Y"))

dat %>% 
  filter(desc != "Do not plant a cover crop") %>% 
  select(desc, perfect_info, value_ha) %>% 
  pivot_wider(names_from = perfect_info,
              values_from = value_ha) %>% 
  mutate(voi = `Y` - `N`)

