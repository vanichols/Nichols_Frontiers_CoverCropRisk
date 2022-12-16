#--summarise SA
#--aug 26 2022
#--updated dec 13 2022 with 50 GDDs requirement (values changed)

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


ddrag <- read_excel("../../1_decision-analysis-workbooks/UPDATED_cover-crop-decision-tree-raw-50 GDDs.xlsx",
                    sheet = "Maize-yield-drag-SA",
                    skip = 5) %>% 
  select(54:58) %>%
  clean_names()

#--current 
43/.405

#--best
24.4/.405

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
  geom_text(aes(x = 0.105, y = 1013, label = "Current yield penalty, $105 gap"),
            check_overlap = T, hjust = 0, fontface = "italic") +
  geom_text(aes(x = 0.005, y = 1013, label = "No yield penalty, $60 gap"),
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
  labs(x = "Relative reduction in maize yields caused by\nplanting <14 days after cover crop termination",
       y = my_ylab,
       color = NULL)

ggsave("../../6_submissions/Second-submission/Figure5.png", width = 7.4, height = 4.5)
ggsave("fig5_yield-drag-SA.png", width = 7.4, height = 4.5)

