#--summarise weather
#--april 7 2021
# updated feb 18 2022



# setup -------------------------------------------------------------------


#--getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
curdir <- paste(getwd())

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

dkbl <- "#2d2d8a"
grn <- "#00b050"
ylw <- "#ffc000"
pnk <- "#cc0099"
ltbl <- "#daedef"

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



# fall rain ---------------------------------------------------------------

prcp <- read_csv("../01_weather/dat_fall-precip.csv")
prcp_chance <- read_csv("../01_weather/dat_chance-fall-precip.csv")
    

# prcp figs ---------------------------------------------------------------

prcp %>% 
  pull(month) %>% 
unique()


f_prcp_line <- 
  prcp %>% 
  mutate(month = ifelse(month == "15oct-30nov", 
                        "After soybean harvest\n(15-Oct through 1-Dec)", 
                        "After maize harvest\n(1-Nov through 1-Dec)"),
         month = factor(month, levels = c("After soybean harvest\n(15-Oct through 1-Dec)", 
                                          "After maize harvest\n(1-Nov through 1-Dec)")),
         mycolor = ifelse(fall_precip_in > 0.5, "clr1", "clr2"),
         fall_precip_cm = fall_precip_in *2.54) %>%
  ggplot() + 
  geom_segment(aes(x = year, xend = year, y = 0, yend = fall_precip_cm)) +
  geom_point(aes(year, fall_precip_cm, color = mycolor), size = 4) + 
  geom_hline(yintercept = 0.5*2.54, linetype = "dashed", color = "black") +
  mytheme + 
  guides(color = F) +
  scale_color_manual(values = c(dkbl, "red")) +
  facet_grid(.~month) +
  labs(x = "Year",
       y = "Total precipitation\n(cm)") + 
  mytheme +
  theme(axis.title.y = element_text(angle = 0)) 
  

f_prcp_line

ggsave("sfig_precip.png")


# precip vs chance --------------------------------------------------------


prcp_chance <- read_csv("../01_weather/dat_precip-chance-fxn-rqd.csv")

prcp_chance %>% 
  filter(grepl("oct", month)) %>% 
  filter(chance_X > .95) %>% 
  arrange(chance_X, -precip_req_cm)

prcp_chance %>% 
  ggplot(aes(precip_req_cm, chance_X)) + 
  geom_line(aes(color = month), size = 2) + 
  geom_vline(xintercept = 1.27, linetype = "dashed") +
  labs(x = "Assumed precipitation required for germination (cm)",
       y = "Probability", 
       color = NULL) +
  scale_y_continuous(labels = label_percent(2)) +
  mytheme

ggsave("sfig_precip-chance.png")

# gdd ---------------------------------------------------------------------

chance_gdds <- read_csv("../01_weather/dat_chance-200gdds.csv")

gdds <- read_csv("../01_weather/dat_200gdds.csv")



# gdds fig ----------------------------------------------------------------

f_gdds_point <- 
  gdds %>% 
  mutate(month = str_to_title(month),
         month = ifelse(month == "Oct", 
                        "After soybean harvest\n(15-Oct through 31-Dec)", 
                        "After maize harvest\n(1-Nov through 31-Dec)"),
         month = factor(month, levels = c("After soybean harvest\n(15-Oct through 31-Dec)", 
                                          "After maize harvest\n(1-Nov through 31-Dec)")),
         mycolor = ifelse(gdds > 200, "clr1", "clr2")) %>%
  ggplot() + 
  geom_segment(aes(x = year, xend = year, y = 0, yend = gdds)) +
  geom_point(aes(year, gdds, color = mycolor), size = 4) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0) +
  mytheme + 
  guides(color = F) +
  facet_grid(.~month) +
  scale_color_manual(values = c(pnk, "gray40")) +
  labs(x = "Year",
       y = "Total growing degree days\n(GDDs)") + 
  theme(axis.title.y = element_text(angle = 0)) 



f_gdds_point

ggsave("sfig_gdds.png")



# gdds vs chance --------------------------------------------------------


gdds_chance <- read_csv("../01_weather/dat_gdds-chance-fxn-rqd.csv")

gdds_chance %>% 
  ggplot(aes(gdds_req, chance_fall200gdds)) + 
  geom_line(aes(color = month), size = 2) + 
  geom_vline(xintercept = 200, linetype = "dashed") +
  labs(x = "Assumed GDDS required for establishment",
       y = "Probability", 
       color = NULL) +
  scale_y_continuous(labels = label_percent(2)) +
  mytheme

ggsave("sfig_gdds-chance.png")

# working days chance -------------------------------------------------------
library(ggbeeswarm)

wd_dat <- 
  read_csv("../01_wea-and-wds/dat_workable-days.csv") %>% 
  mutate(two_or_more = ifelse(days_tot >= 2, "yes", "no"),
         four_or_more= ifelse(days_tot >= 4, "yes", "no"))


chance_wd <- 
  read_csv("../01_wea-and-wds/dat_chance-wd.csv")


library(ggridges)

wd_dat %>%
  mutate(
    cat2 = str_to_title(cat2),
    cat2 = factor(cat2, 
                  levels = c("Early April",
                             "Late April", 
                             "Early May",
                             "Late May")),
    cat2 = fct_rev(cat2)) %>% 
  ggplot(aes(days_tot, cat2)) + 
  geom_density_ridges()


wd_dat %>% 
  select(year, cat2, days_tot)

#--cumulative probability

wd_prob <- NULL

for (i in seq(from = 0.1, to = 5, by = 0.1)){
  
  tmp <-
    wd_dat %>%
    select(year, cat2, days_tot) %>% 
    mutate(X_or_more = ifelse(days_tot >= i, "yes", "no")) %>%
    group_by(cat2) %>%
    mutate(ntot = n()) %>%
    group_by(cat2, X_or_more, ntot) %>%
    summarise(n = n()) %>%
    mutate(chance_Xom = n / ntot)  %>%
    filter(X_or_more == "yes") %>% 
    ungroup() %>%
    select(cat2, chance_Xom) %>%
    mutate(days = i)
  
  wd_prob <- 
    bind_rows(wd_prob, tmp)
  
}


mytheme2 <-
  theme(
    axis.title.y = element_text(angle = 90, vjust = 0.5),
    axis.title = element_text(size = rel(1.3)),
    axis.text = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1.2)),
    legend.title = element_text(size = rel(1.3)),
    legend.justification = c(0, 0),
    legend.position = c(0.1,0.1),
    strip.text = element_text(size = rel(1.2)),
    strip.background =element_rect(fill=NA, color = NA)
  )


wd_prob %>%
  mutate(
    cat2 = str_to_title(cat2),
    cat2 = factor(cat2, 
                  levels = c("Early April",
                             "Late April", 
                             "Early May",
                             "Late May"))) %>% 
  ggplot(aes(days, chance_Xom)) + 
  geom_line(aes(color = cat2, size = cat2)) + 
  scale_color_manual(values = c("dodgerblue", "dodgerblue",
                                "orange", "orange")) +
  scale_size_manual(values = c(1, 2, 1, 2)) + 
  labs(x = "Number of Workable Field Days",
       y = "Cumulative Probability",
       color = NULL,
       size = NULL) + 
  scale_y_continuous(labels = label_percent()) + 
  mytheme2

ggsave("sfig_working-days.png")

# wdchnce -----------------------------------------------------------------



# wd_dat %>%
#   unite(two_or_more, four_or_more, col = "more") %>% 
#   mutate(cat2 = factor(cat2, levels = c("early April", "late April", "early May", "late May"))) %>% 
#   ggplot(aes(cat2, days_tot)) + 
#   geom_quasirandom(aes(color = more, pch = more), size = 2) +
#   geom_hline(yintercept = 2, color = "gray20", linetype = "dashed") +
#   geom_hline(yintercept = 4, color = "gray20", linetype = "dashed") +
#   geom_text(data = chance_wd, 
#             aes(x = 0.6, y = 7, label = paste("2+ WDs:", round(chance_2om*100, 0), "%"))) +
#   geom_text(data = chance_wd, 
#             aes(x = 0.6, y = 6, label = paste("4+ WDs:", round(chance_4om*100, 0), "%"))) +
#   facet_wrap(~cat2, scales = "free") + 
#   mytheme + 
#   scale_color_manual(values = c("gray20", pnk, pnk)) +
#   scale_shape_manual(values = c(21, 17, 18)) +
#   guides(color = F, shape = F) +
#   labs(x = NULL,
#        y = "Total workable days (WDs)") + 
#   theme(axis.text.x = element_blank())
# 
# ggsave("../fig_workable-days.png")
# 

# chance establishment ----------------------------------------------------


empty_precip <-
  prcp_chance %>% 
  rename("chance_suffprecip" = 2) %>% 
  mutate(rain_thresh = 0.5)

for (i in seq(0.25, 1.75, 0.25)) {
  
  tmp.precip <- 
    prcp %>% 
    mutate(suffprecip = ifelse(fall_precip_in > i, "yes", "no")) %>% 
    group_by(month) %>% 
    mutate(ntot = n()) %>%
    group_by(month, suffprecip, ntot) %>% 
    summarise(n = n()) %>% 
    mutate(chance_suffprecip = n/ntot) %>% 
    filter(suffprecip == "yes") %>% 
    ungroup() %>% 
    select(month, chance_suffprecip) %>% 
    mutate(rain_thresh = i)
  
  empty_precip <- 
    empty_precip %>% 
    bind_rows(tmp.precip)
  
}

empty_precip


prcp_chance05 <- 
  prcp %>% 
  mutate(more_than_half_in = ifelse(fall_precip_in > 0.5, "yes", "no")) %>%
  group_by(month) %>% 
  mutate(ntot = n()) %>% 
  group_by(month, ntot, more_than_half_in) %>% 
  summarise(n = n()) %>% 
  mutate(chance_halfin = n/ntot) %>% 
  filter(more_than_half_in == "yes") %>% 
  ungroup() %>% 
  select(month, chance_halfin)

prcp_chance025 <- 
  prcp %>% 
  mutate(more_than_quarter_in = ifelse(fall_precip_in > 0.25, "yes", "no")) %>%
  group_by(month) %>% 
  mutate(ntot = n()) %>% 
  group_by(month, ntot, more_than_quarter_in ) %>% 
  summarise(n = n()) %>% 
  mutate(chance_quarterin = n/ntot) %>% 
  filter(more_than_quarter_in  == "yes") %>% 
  ungroup() %>% 
  select(month, chance_quarterin)



empty_gdds <-
  chance_gdds %>% 
  rename("chance_suffgdds" = 2) %>% 
  mutate(gdd_thresh = 200)
  
for (i in seq(50, 300, 25)) {
  
 tmp.gdds <- 
    gdds %>% 
    mutate(suffgdds = ifelse(gdds > i, "yes", "no")) %>% 
    group_by(month) %>% 
    mutate(ntot = n()) %>%
    group_by(month, suffgdds, ntot) %>% 
    summarise(n = n()) %>% 
    mutate(chance_suffgdds = n/ntot) %>% 
    filter(suffgdds == "yes") %>% 
    ungroup() %>% 
    select(month, chance_suffgdds) %>% 
      mutate(gdd_thresh = i)
 
 empty_gdds <- 
   empty_gdds %>% 
   bind_rows(tmp.gdds)
 
}
  
empty_gdds

full_join(empty_gdds, empty_precip) %>% 
  filter(month == "oct") %>% 
  filter(gdd_thresh < 100)

full_join(empty_gdds, empty_precip) %>% 
  mutate(chance_est = chance_suffgdds * chance_suffprecip) %>% 
  ggplot(aes(rain_thresh, chance_est)) + 
  geom_line(aes(color = gdd_thresh, group = gdd_thresh), size = 3) + 
  facet_grid(.~month) + 
  scale_color_viridis_c()

full_join(empty_gdds, empty_precip) %>% 
  mutate(chance_est = chance_suffgdds * chance_suffprecip,
         month = ifelse(month == "nov", "CC planted in Nov", "CC planted in Oct"),
         month = factor(month, levels = c("CC planted in Oct", "CC planted in Nov"))) %>% 
  ggplot(aes(gdd_thresh, chance_est)) + 
  geom_line(aes(color = rain_thresh, group = rain_thresh), size = 3) + 
  facet_grid(.~month) + 
  scale_color_viridis_c() + 
  scale_y_continuous(label = label_percent()) +
  labs(x = "Growing-degree-days (GDDs) needed for establishment",
      y = "Chance of rye\ncover crop establishment", 
      color = "Rainfall needed\nfor establishment (inches)") + 
  mytheme + 
  theme(legend.position = "right", 
        legend.direction = "vertical")

ggsave("../fig_SA-estab.png")


