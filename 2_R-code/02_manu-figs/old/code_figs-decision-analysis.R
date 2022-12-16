#--summarise decision workbook
#--aug 26 2022

#--need to check the spreadsheet...

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


# data --------------------------------------------------------------------

dcorn <- read_excel("../../1_decision-analysis-workbooks/UPDATED_cover-crop-decision-tree-raw.xlsx",
           sheet = "Maize-no-societal-benefits",
           skip = 5) %>% 
  select(1:3) %>%
  clean_names() %>% 
  filter(!is.na(rye_decision))


dsoy <- read_excel("../../1_decision-analysis-workbooks/UPDATED_cover-crop-decision-tree-raw.xlsx",
                    sheet = "Soy-no-societal-benefits",
                    skip = 5) %>% 
  select(1:3) %>%
  clean_names() %>% 
  filter(!is.na(rye_decision))


dat <- 
  dcorn %>% 
  mutate(scenario  = "Rye - Maize") %>% 
  bind_rows(
    dsoy %>% 
      mutate(scenario = "Rye - Soy")
  ) %>% 
  mutate(rye_dec_nice = ifelse(rye_decision == "plant rye", 
                               "Plant rye", 
                               "Do not plant rye"),
         value_ha = e_cover_crop_decision /.405)


dat %>% 
  ggplot(aes(rye_dec_nice, value_ha)) +
  geom_col(aes(fill = rye_dec_nice), 
           width = 0.5, color = "black", size = 2) +
  geom_text(aes(
    y = value_ha + 50,
    label = paste0("$", round(value_ha, 0))),
    fontface = "italic") +
  scale_y_continuous(labels = label_dollar()) +
  labs(x = NULL, y = "Value of decision\nper hectare") +
  guides(fill = "none") +
  scale_fill_manual(values = c(ylw, grn)) +
  theme_clean() +
  theme(axis.title = element_text(size = rel(1.2)),
        #axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.text.x = element_text(size = rel(1.5))) + 
  facet_grid(.~scenario)

ggsave("fig_dec-value.png")
