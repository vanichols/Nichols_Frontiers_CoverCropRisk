#--summarise SA
#--aug 26 2022
#--updated dec 15 to include 50% chance and 100 GDDs

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

my_lab1 <- (expression("Value of decision ($ "~ha^-1~")"))
my_lab2 <- (expression("Value of cost share or incentive ($ "~ha^-1~")"))


# data --------------------------------------------------------------------

dcorn <- read_excel("../../1_decision-analysis-workbooks/UPDATED_cover-crop-decision-tree-raw-100 GDDs - 50pct.xlsx",
           sheet = "Maize-no-societal-benefits-SA",
           skip = 5) %>% 
  select(54:58) %>%
  clean_names()


dsoy <- read_excel("../../1_decision-analysis-workbooks/UPDATED_cover-crop-decision-tree-raw-100 GDDs - 50pct.xlsx",
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
         

# maize = $34 -- $84/ha
34/0.405
# soy = $12 -- $30/ha
12/0.405

dat %>% 
  select(scenario, rye_dec_nice, value_ha, flat_payment_ha) %>% 
  pivot_wider(names_from = rye_dec_nice, values_from = value_ha)


#--these are eyeballed from the SA excel
dummy <- 
  tibble(scenario = c("Rye - Maize", "Rye - Soybean"),
         value_ha = c(1044, 600),
         flat_payment_ha = c(85, 30),
         flat_payment_lab = c("85*", "30"))

dummy_rect <- 
  tibble(scenario = c("Rye - Maize", "Rye - Soybean"),
         x1 = c(12, 12),
         x2 = c(74, 74),
         y1 = c(925, 550),
         y2 = c(1090, 690))


# fig payment SA --------------------------------------------------------------

#--$12-74 are available payments


fig_inc <-
  dat %>%
  ggplot(aes()) +
  # geom_rect(aes(xmin = 12, xmax = 74,
  #               ymin = 540, ymax = 1200),
  #           fill = "gray80") +
  geom_rect(data = dummy_rect,
            aes(
              xmin = x1,
              xmax = x2,
              ymin = y1,
              ymax = y2
            ),
            fill = "gray80") +
  geom_line(aes(flat_payment_ha, value_ha,
                color = rye_dec_nice), size = 2) +
  geom_point(data = dummy,
             aes(x = flat_payment_ha, y = value_ha),
             size = 3) +
  geom_text(
    data = dummy,
    aes(
      x = flat_payment_ha,
      y = value_ha + 50,
      label = paste0("$", flat_payment_lab)
    ),
    fontface = "italic"
  ) +
  scale_color_manual(values = c(ylw, grn)) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar(), limits = c(400, 1400)) +
  theme_clean() +
  theme(
    axis.title = element_text(size = rel(1)),
    #axis.title.y = element_text(angle = 0, vjust = 0.5),
    strip.text.x = element_text(size = rel(1.1)),
    #        legend.justification = c(1, 1),
    legend.position = "top",
    legend.background = element_rect(color = NULL)
  ) +
  labs(x = my_lab2,
       y = my_lab1,
       color = NULL,
       caption = "*Sensitive to reduction in maize yields") +
  facet_grid(. ~ scenario)

fig_inc


# fig yield drag SA -------------------------------------------------------

my_ylab2 <- (expression("Value of decision ($ "~ha^-1~")"))
my_xlab2 <- (expression("Value of cost share or incentive ($ "~ha^-1~")")) 


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


fig_drag <- 
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
  scale_y_continuous(labels = label_dollar(), limits = c(900, 1090)) +
  theme_clean() +
  theme(
    axis.title = element_text(size = rel(1)),
    #axis.title.y = element_text(angle = 0, vjust = 0.5),
    strip.text.x = element_text(size = rel(1.1)),
    #        legend.justification = c(1, 1),
    legend.position = "top",
    legend.background = element_rect(color = NULL)
  ) +
  labs(x = "Relative reduction in maize yields caused by\nplanting <14 days after cover crop termination\n(50% chance)",
       y = my_ylab2,
       color = NULL)



# together ----------------------------------------------------------------

fig_inc / fig_drag +
  plot_layout(guides = 'collect')  & theme(legend.position = 'top') 


ggsave("../../6_submissions/Second-submission/Figure3.png",  width = 6, height = 7)

