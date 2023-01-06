#--make weather figs
#--april 7 2021
# updated feb 18 2022
# updated sept 9
# updated dec 15 2022


# setup -------------------------------------------------------------------


rm(list = ls())
library(tidyverse)
library(lubridate)
library(saapsim)
#devtools::install_github("vanichols/saapsim")
library(readxl)
library(patchwork)
library(scales)
library(ggthemes)
library(ggpubr)
library(patchwork)

source("2_R-code/02_manu-figs/code_palettes.R")

theme_set(theme_bw())

mytheme <-
  theme(
    axis.title.y = element_text(angle = 90, vjust = 0.5),
    axis.title = element_text(size = rel(1.1)),
    axis.text = element_text(size = rel(1)),
    legend.text = element_text(size = rel(1.1)),
    legend.title = element_text(size = rel(1.1)),
    legend.position = "top",
    strip.text = element_text(size = rel(1.1)),
    strip.background =element_rect(fill=NA, color = NA)
  ) 



# precip vs chance --------------------------------------------------------

prcp_chance <- read_csv("2_R-code/01_wea-and-wds/dat_precip-chance-fxn-rqd.csv")

prcp_chance %>% 
  filter(grepl("oct", month)) %>% 
  filter(chance_X > .95) %>% 
  arrange(chance_X, -precip_req_cm)


f1 <- 
  prcp_chance %>% 
  mutate(timeframe = ifelse(month == "15oct-30nov", "Following soybean harvest (15-Oct)", "Following maize harvest (1-Nov)")) %>%
  ggplot(aes(precip_req_cm, chance_X)) + 
  geom_line(aes(color = timeframe), size = 2) + 
  geom_vline(xintercept = 1.27, linetype = "dashed") +
  labs(x = "Assumed precipitation\nrequired for germination (cm)",
       y = "Probability of\nprecipitation", 
       color = NULL) +
  scale_y_continuous(labels = label_percent(1), limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_color_manual(values = c(dkbl, pnk)) +
  guides(color = guide_legend(nrow = 2)) +
  mytheme

f1

# gdds vs chance --------------------------------------------------------


gdds_chance <- read_csv("2_R-code/01_wea-and-wds/dat_gdds-chance-fxn-rqd.csv")

gdds_chance %>% 
  filter(gdds_req == 100)

f2 <- 
  gdds_chance %>%
  mutate(timeframe = ifelse(month == "oct", "Following soybean harvest (15-Oct)", "Following maize harvest (1-Nov)")) %>% 
  ggplot(aes(gdds_req, chance_fall200gdds)) + 
  geom_line(aes(color = timeframe), size = 2) + 
  geom_vline(xintercept = 100, linetype = "dashed") +
  labs(x = "Assumed GDDs\nrequired for establishment",
       y = "Probability of\nGDDs", 
       color = NULL) +
  scale_y_continuous(labels = label_percent(1), limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_x_continuous(limits = c(50, 300)) +
  scale_color_manual(values = c(dkbl, pnk)) +
  guides(color = guide_legend(nrow = 2)) +
  mytheme

f2

f1 / f2 +
  plot_layout(guides = 'collect')  & theme(legend.position = 'top') 


ggsave("2_R-code/02_manu-figs/Figure4.jpg", width = 5, height = 6.7)
ggsave("4_temp/Second-submission/Figure4.jpg", width = 5, height = 6.7)
