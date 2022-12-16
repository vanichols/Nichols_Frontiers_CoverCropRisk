#--summarise SA
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


# data --------------------------------------------------------------------

dcorn <- read_excel("../../1_decision-analysis-workbooks/UPDATED_cover-crop-decision-tree-raw.xlsx",
           sheet = "Maize-no-societal-benefits-SA",
           skip = 5) %>% 
  select(54:58) %>%
  clean_names()


dsoy <- read_excel("../../1_decision-analysis-workbooks/UPDATED_cover-crop-decision-tree-raw.xlsx",
                    sheet = "Soy-no-societal-benefits-SA",
                    skip = 5) %>% 
  select(56:60) %>%
  clean_names()



#--0.405 ha in an acre

dat <- 
  dcorn %>% 
  mutate(scenario  = "Rye - Maize") %>% 
  bind_rows(
    dsoy %>% 
      mutate(scenario = "Rye - Soybean")
  ) %>% 
  select(scenario, everything(), - x56) %>% 
  pivot_longer(3:ncol(.)) %>% 
  mutate(
    flat_payment_ha = flat_payment / 0.405,
    value_ha = value / 0.405) %>% 
  filter(name %in% c("plant_cover_crop", "dont_plant_cover_crop")) %>% 
  mutate(rye_dec_nice = ifelse(name == "plant_cover_crop", 
                               "Plant rye", 
                               "Do not plant rye"))
         



#--these are eyeballed from the SA excel
dummy <- 
  tibble(scenario = c("Rye - Maize", "Rye - Soybean"),
         value_ha = c(1044, 600),
         flat_payment_ha = c(99, 45))

#--$12-74 are available payments
dat %>% 
  ggplot(aes(flat_payment_ha, value_ha)) +
  geom_rect(aes(xmin = 12, xmax = 74,
                ymin = 540, ymax = 1200),
            fill = "gray80") +
  geom_line(aes(color = rye_dec_nice), size = 2) +
  geom_point(data = dummy,
             aes(x = flat_payment_ha, y = value_ha),
             size = 3) +
  geom_text(data = dummy,
             aes(x = flat_payment_ha, y = value_ha + 50, label = paste0("$", flat_payment_ha)),
             fontface = "italic") +
  scale_color_manual(values = c(ylw, grn)) + 
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  theme_clean() +
  theme(axis.title = element_text(size = rel(1.2)),
        #axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.text.x = element_text(size = rel(1.5)),
#        legend.justification = c(1, 1),
        legend.position = "top",
legend.background = element_rect(color = NULL)) + 
  labs(x = "Value of cost share or incentive ($/ha)", 
       y = "Value of decision ($/ha)",
       color = NULL) +
  facet_grid(.~scenario)

ggsave("fig_dec-value-SA.png")


# yield drag --------------------------------------------------------------


ddrag <- read_excel("../../1_decision-analysis-workbooks/UPDATED_cover-crop-decision-tree-raw.xlsx",
                    sheet = "Maize-yield-drag-SA",
                    skip = 5) %>% 
  select(54:58) %>%
  clean_names()

ddrag %>%
  pivot_longer(2:ncol(.)) %>%
  mutate(value_ha = value / 0.405) %>%
  filter(name %in% c("plant_cover_crop", "dont_plant_cover_crop")) %>%
  mutate(
    rye_dec_nice = ifelse(name == "plant_cover_crop",
                          "Plant rye",
                          "Do not plant rye"),
    yield_drag = 1 - yield_10days
  ) %>%
  filter(yield_drag < 0.3) %>%
  ggplot(aes(yield_drag, value_ha)) +
  geom_line(aes(color = rye_dec_nice), size = 2) +
  geom_vline(xintercept = 0.1, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(aes(x = 0.105, y = 1013, label = "Current yield drag, $99 gap"),
            check_overlap = T, hjust = 0, fontface = "italic") +
  geom_text(aes(x = 0.005, y = 1013, label = "No yield drag, $24 gap"),
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
  labs(x = "Reduction in maize yields from\nplanting <10 days after cover crop termination",
       y = "Value of decision ($/ha)",
       color = NULL)

ggsave("fig_yield-drag-SA.png")

