#--summarise SAs of termination
#--aug 26 2022
#--updated 12/13/2022, use new worksheets with 100 GDDs probabilities
#--updated 12/14/2022, added a 50% chance of 10% yield reduction

# setup -------------------------------------------------------------------

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

source("2_R-code/02_manu-figs/code_palettes.R")

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


# data --------------------------------------------------------------------

#--if you plant a cover crop but get no money for it
dcorn_cc <- 
  read_excel("1_Decision-analysis-workbook/UPDATED_cover-crop-decision-tree-raw-100 GDDs - 50pct.xlsx",
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
  read_excel("1_Decision-analysis-workbook/UPDATED_cover-crop-decision-tree-raw-100 GDDs - 50pct.xlsx",
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


dsoy_cc <- 
  read_excel("1_Decision-analysis-workbook/UPDATED_cover-crop-decision-tree-raw-100 GDDs - 50pct.xlsx",
             sheet = "Soy-no-societal-benefits-SA",
             skip = 5) %>% 
  select(11:12) %>%
  clean_names() %>% 
  remove_empty("rows") %>% 
  distinct() %>% 
  filter(spring != "NA",
         !is.na(spring)) %>% 
  rename(value = 2) %>% 
  mutate(spring = str_replace(spring, ", ", "\n")) %>% 
  mutate(scenario  = "Rye - Soybean",
         decision_id = c("5", "6", "5/6"))

dsoy_no <- 
  read_excel("1_Decision-analysis-workbook/UPDATED_cover-crop-decision-tree-raw-100 GDDs - 50pct.xlsx",
             sheet = "Soy-no-societal-benefits-SA",
             skip = 5) %>% 
  select(2:3) %>%
  clean_names() %>% 
  remove_empty("rows") %>% 
  distinct() %>% 
  rename(value = 2) %>% 
  filter(rye_decision == "don't plant rye") %>%  
  mutate(scenario  = "Rye - Soybean",
         decision_id = c("4"))


#--0.405 ha in an acre

dat <- 
  dcorn_cc %>% 
  bind_rows(dcorn_no) %>% 
  bind_rows(dsoy_cc) %>% 
  bind_rows(dsoy_no) %>% 
  mutate(desc = 
           case_when(
             decision_id == "1"|decision_id == "4" ~ "Do not plant a cover crop",
             decision_id == "2"|decision_id == "5" ~ "Plant cover crop, plan to terminate early April",
             decision_id == "3"|decision_id == "6" ~ "Plant cover crop, plan to terminate late April",
             decision_id == "2/3"|decision_id == "5/6" ~ "Plant cover crop, fails to establish",
             TRUE ~ "hmm"
             )) %>% 
  select(desc, scenario, decision_id, desc, value) %>% 
  mutate(value_ha = value/0.405)


# bar graph ---------------------------------------------------------------

dat %>% 
  arrange(desc) %>% 
  mutate(desc = fct_inorder(desc),
         desc2 = fct_rev(desc),
         scenario = factor(scenario, levels = c("Rye - Maize", "Rye - Soybean"))) %>%  
  ggplot(aes(desc2, value_ha), value_ha) + 
  geom_col(width = 0.5,
           aes(fill = desc2),
           show.legend = F, 
           color = "black") +
  geom_text(aes(x = desc2, y = value_ha + 70, 
                label = paste("$", round(value_ha, 0))),
            fontface = "italic") +
  coord_flip() +
  scale_fill_manual(values = c(
    "Do not plant a cover crop" = "gray80", 
    "Plant cover crop, fails to establish" = ylw, 
    "Plant cover crop, plan to terminate early April" = ltgrn, 
    "Plant cover crop, plan to terminate late April" = dkgrn)) +
  scale_y_continuous(labels = label_dollar(), limits = c(0, 1150)) +
  labs(x = NULL,
       y = my_ylab) +
  theme_clean() +
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text.x = element_text(size = rel(1.2)),
        #axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.text.x = element_text(size = rel(1.5))) + 
  facet_grid(scenario~.)


# lollipop ----------------------------------------------------------------

dat2 <- 
  dcorn_cc %>% 
  bind_rows(dcorn_no) %>% 
  bind_rows(dsoy_cc) %>% 
  bind_rows(dsoy_no) %>% 
  mutate(desc = 
           case_when(
             decision_id == "1"|decision_id == "4" ~ "Do not plant a cover crop",
             decision_id == "2"|decision_id == "5" ~ "Plant cover crop,\nplan to terminate early April",
             decision_id == "3"|decision_id == "6" ~ "Plant cover crop,\nplan to terminate late April",
             decision_id == "2/3"|decision_id == "5/6" ~ "Plant cover crop,\nfails to establish",
             TRUE ~ "hmm"
           )) %>% 
  select(desc, scenario, decision_id, desc, value) %>% 
  mutate(value_ha = value/0.405)

dat2 %>% 
  arrange(desc) %>% 
  mutate(desc = fct_inorder(desc),
         desc2 = fct_rev(desc),
         scenario = factor(scenario, levels = c("Rye - Maize", "Rye - Soybean"))) %>%  
  ggplot(aes(desc2, value_ha), value_ha) + 
  geom_segment(
    aes(x = desc2, xend = desc2,
        y = 0, yend = value_ha)
  ) +
  geom_point(
    aes(fill = desc2),
    pch = 21,
    size = 5,
    show.legend = F, 
    color = "black") +
  geom_text(aes(x = desc2, y = value_ha + 200, 
                label = paste("$", round(value_ha, -1))),
            fontface = "italic") +
  coord_flip() +
  scale_fill_manual(values = c(
    "Do not plant a cover crop" = "gray80", 
    "Plant cover crop,\nfails to establish" = ylw, 
    "Plant cover crop,\nplan to terminate early April" = ltgrn, 
    "Plant cover crop,\nplan to terminate late April" = dkgrn)) +
  scale_y_continuous(labels = label_dollar(), limits = c(0, 1400)) +
  labs(x = NULL,
       y = my_ylab) +
  theme_clean() +
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text.x = element_text(size = rel(1.2)),
        #axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.text.x = element_text(size = rel(1.5))) + 
  facet_grid(scenario~.)



ggsave("2_R-code/02_manu-figs/Figure2.png", width = 6, height = 4.5)
