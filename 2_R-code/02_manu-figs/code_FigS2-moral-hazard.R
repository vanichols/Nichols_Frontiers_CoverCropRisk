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

dcorn <- 
  read_excel("../../1_decision-analysis-workbooks/UPDATED_cover-crop-decision-tree-raw.xlsx",
           sheet = "Maize-moral-hazard",
           skip = 5) %>% 
  select(11:12) %>%
  clean_names() %>% 
  remove_empty("rows") %>% 
  distinct() %>% 
  filter(spring != "NA",
         !is.na(spring)) %>% 
  rename(value = 2) %>% 
  mutate(spring = str_replace(spring, ", ", "\n"))

dsoy<- 
  read_excel("../../1_decision-analysis-workbooks/UPDATED_cover-crop-decision-tree-raw.xlsx",
             sheet = "Soy-moral-hazard",
             skip = 5) %>% 
  select(11:12) %>%
  clean_names() %>% 
  remove_empty("rows") %>% 
  distinct() %>% 
  filter(spring != "NA",
         !is.na(spring)) %>% 
  rename(value = 2) %>% 
  mutate(spring = str_replace(spring, ", ", "\n"))


#--0.405 ha in an acre

dat <- 
  dcorn %>% 
  mutate(scenario  = "Soybean - Rye - Maize") %>% 
  bind_rows(
    dsoy %>% 
      mutate(scenario = "Maize - Rye - Soybean")
  ) %>% 
  select(scenario, everything()) %>% 
  mutate(spring = str_to_sentence(spring),
         value_ha = value/0.405)
         

dat %>% 
  ggplot(aes(reorder(spring, value_ha), value_ha)) + 
  geom_col(width = 0.5, aes(fill = spring), 
           show.legend = F, 
           color = "black") +
  coord_flip() +
  scale_fill_manual(values = c(ylw, dkbl, ltgrn)) +
  scale_y_continuous(labels = label_dollar()) +
  labs(x = NULL,
       y = my_ylab) +
  theme_clean() +
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text.x = element_text(size = rel(1.2)),
        #axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.text.x = element_text(size = rel(1.5))) + 
  facet_grid(scenario~.)

ggsave("FigureS2.png", width = 6.1, height = 4.5)
