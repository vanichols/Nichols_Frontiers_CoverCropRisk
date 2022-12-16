#--summarise SA
#--aug 26 2022
#--updated dec 13 2022 with 100 GDDs requirement (values changed)
#--added value of perfect information, changed probability of yield decline to 50%

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

source("code_palettes.R")

theme_set(theme_bw())

mytheme <-
  theme(
    axis.title.y = element_text(angle = 90, vjust = 0.5),
    axis.title = element_text(size = rel(1.3)),
    axis.text = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1.2)),
    legend.title = element_text(size = rel(1.3)),
    legend.position = "top",
    strip.text = element_text(size = rel(1.2)),
    strip.background =element_rect(fill=NA, color = NA)
  )


my_ylab <- (expression("Value of decision ($ "~ha^-1~")"))
my_xlab <- (expression("Value of cost share or incentive ($ "~ha^-1~")")) 


# yield drag --------------------------------------------------------------


ddrag <- 
  read_excel("../../1_decision-analysis-workbooks/UPDATED_cover-crop-decision-tree-raw-100 GDDs - 50pct.xlsx",
                    sheet = "Maize-yield-drag-SA",
                    skip = 5) %>% 
  select(54:58) %>%
  clean_names()

#--current 
ddrag %>% 
  filter(pct_total_yield_14_days == 0.9) %>% 
  mutate(value = dont_plant_cover_crop - plant_cover_crop)

34.6/.405 # $85

#--best
ddrag %>% 
  filter(pct_total_yield_14_days == 1) %>% 
  mutate(value = dont_plant_cover_crop - plant_cover_crop)

24.5/.405 # $60

ddrag %>%
  pivot_longer(2:ncol(.)) %>%
  mutate(value_ha = value / 0.405) %>%
  filter(name %in% c("plant_cover_crop", "dont_plant_cover_crop")) %>%
  mutate(
    rye_dec_nice = ifelse(name == "plant_cover_crop",
                          "Plant rye",
                          "Do not plant rye"),
    yield_drag = 1 - pct_total_yield_14_days
  ) %>%
  filter(yield_drag < 0.3) %>%
  ggplot(aes(yield_drag, value_ha)) +
  geom_line(aes(color = rye_dec_nice), size = 2) +
  geom_vline(xintercept = 0.1, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(aes(x = 0.105, y = 1013, label = "Current yield penalty,\n$85 gap"),
            check_overlap = T, hjust = 0, fontface = "italic") +
  geom_text(aes(x = 0.005, y = 1013, label = "No yield penalty,\n$60 gap"),
            check_overlap = T, hjust = 0, fontface = "italic") +
  scale_color_manual(values = c(ylw, grn)) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) +
  theme_clean() +
  theme(
    axis.title = element_text(size = rel(1.2)),
    #axis.title.y = element_text(angle = 0, vjust = 0.5),
    strip.text.x = element_text(size = rel(1.5)),
    #        legend.justification = c(1, 1),
    legend.position = "top",
    legend.background = element_rect(color = NULL)
  ) +
  labs(x = "Relative reduction in maize yields caused by\nplanting <14 days after cover crop termination\n(50% chance)",
       y = my_ylab,
       color = NULL)



# perfect info ------------------------------------------------------------


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
         decision_id = c("2", "3", "2/3")) 

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
         perfect_info = "N",
         decision_id = c("2", "3", "2/3"))


dcorn_cc %>% 
  bind_rows(dcorn_cc_perfect) %>% 
  mutate(desc = 
           case_when(
             decision_id == "1"|decision_id == "4" ~ "Do not plant a cover crop",
             decision_id == "2"|decision_id == "5" ~ "Plant cover crop,\nplan to terminate early April",
             decision_id == "3"|decision_id == "6" ~ "Plant cover crop,\nplan to terminate late April",
             decision_id == "2/3"|decision_id == "5/6" ~ "Plant cover crop, fails to establish",
             TRUE ~ "hmm"
           )) %>% 
  filter(spring != "cover crop dies") %>% 
  mutate(value_ha = value/0.405) %>%
  select(desc, perfect_info, value_ha) %>% 
  pivot_wider(names_from = perfect_info, values_from = value_ha) %>% 
  ggplot() +
  geom_segment(aes(x = desc, xend = desc, 
                   y = N, yend = Y),
               arrow = arrow(length = unit(0.4,"cm"))) +
  geom_point(aes(x = desc, y = N), pch = 19, size = 4, color = dkgrn) +
  geom_point(aes(x = desc, y = Y), pch = 19, size = 4, color = ltgrn) 



ggsave("../../6_submissions/Second-submission/Figure5.png", width = 7.4, height = 4.5)
ggsave("fig5_yield-drag-SA.png", width = 7.4, height = 4.5)



