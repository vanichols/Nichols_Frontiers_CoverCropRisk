#--summarise SAs of termination
#--aug 26 2022

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

# data --------------------------------------------------------------------

dcorn <- read_excel("../../1_decision-analysis-workbooks/UPDATED_cover-crop-decision-tree-raw.xlsx",
           sheet = "Maize-no-societal-benefits-SA",
           skip = 5) %>% 
  select(54:58) %>%
  clean_names() %>% 
  filter(flat_payment == 0) %>% 
  distinct()


dsoy <- 
  read_excel("../../1_decision-analysis-workbooks/UPDATED_cover-crop-decision-tree-raw.xlsx",
                    sheet = "Soy-no-societal-benefits-SA",
                    skip = 5) %>% 
  select(57:61) %>%
  clean_names() %>% 
  filter(flat_payment == 0)



#--0.405 ha in an acre

dat <- 
  dcorn %>% 
  mutate(scenario  = "Soybean - Rye - Maize") %>% 
  bind_rows(
    dsoy %>% 
      mutate(scenario = "Maize - Rye - Soybean")
  ) %>% 
  select(scenario, everything()) %>% 
  pivot_longer(3:ncol(.)) %>% 
  filter(name %in% c("plant_kill_early_spring", "plant_delay_kill")) %>% 
  mutate(
    value_ha = value / 0.405) %>% 
  mutate(rye_dec_nice = ifelse(name == "plant_kill_early_spring", 
                               "Terminate early", 
                               "Delay termination")) 
  
         

dat

dat %>% 
  mutate(rye_dec_nice = fct_inorder(rye_dec_nice)) %>% 
  ggplot(aes(rye_dec_nice, value_ha)) +
  geom_col(aes(fill = rye_dec_nice), 
           width = 0.5, color = "black", size = 2,
           show.legend = F) +
  geom_text(
    aes(x = rye_dec_nice, y = value_ha + 50, label = paste0("$", round(value_ha, 0))),
    fontface = "italic"
  ) +
  scale_fill_manual(values = c(ltgrn, dkbl)) +
  scale_y_continuous(labels = label_dollar()) +
  labs(x = NULL,
       y = my_ylab) +
  theme_clean() +
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text.x = element_text(size = rel(1.2)),
        #axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.text.x = element_text(size = rel(1.5))) + 
  facet_grid(.~scenario)

ggsave("fig_dec-value-termination.png", width = 6.1, height = 4.5)
ggsave("Figure6.png", width = 6.1, height = 4.5)
