#--look at isaiah's workable days data
#--march 18 2021
#--updated april 7 2021
#--looked at again 5/4/2022
#--and again 7/21/2022 - output is dat_workable-days.csv

#--getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
curdir <- paste(getwd())


library(tidyverse)
library(lubridate)

dat <- 
  read_csv("IowaFieldWorkableDays.csv") %>% 
  mutate(year = year(datetime),
         month = month(datetime),
         day = day(datetime))

dat %>% 
  ggplot(aes(datetime, days)) + 
  geom_point() + 
  facet_wrap(~crd)


# test --------------------------------------------------------------------
# 
# tst <- 
#   dat %>% 
#   filter(year == 2011) %>% 
#   select(datetime, days)
# 
# 
# newdates <- tibble(datetime = seq(min(tst$datetime)-6,max(tst$datetime), by = "1 day"))
# 
# #--join and fill
# tst.s <- 
#   newdates %>%
#   left_join(
#     tst %>%
#       mutate(days_avg = days/7) %>%
#       left_join(newdates)
#   ) %>%
#   fill(days_avg, .direction = "up")
# 
# 
# tst.s %>% 
#   ggplot(aes(datetime, days_avg)) + 
#   geom_point()
# 
# tst.s %>% 
#   mutate(year = year(datetime),
#          month = month(datetime),
#          month2 = as.character(month(datetime, label = T, abbr = T)),
#          day = day(datetime),
#          cat = ifelse(day < 16, "1-15", "16-31"),
#          cat = paste(month2, cat)) %>% 
#   group_by(year, month, month2, cat) %>% 
#   summarise(days_avg = mean(days_avg, na.rm = T)) %>% 
#   ungroup() %>% 
#   arrange(year, month) %>% 
#   mutate(cat = fct_inorder(cat),
#          cat_rev = fct_rev(cat)) %>% 
#   ggplot(aes(cat_rev, days_avg)) + 
#   geom_point() + 
#   coord_flip() 
# 

# more years --------------------------------------------------------------------

#--only have good weather data to 2019. 
tst <- 
  dat %>% 
  filter(year > 1988) %>%
  filter(year < 2020) %>% 
  select(datetime, days)


#--create a list of all dates in this time frame
newdates <- tibble(datetime = seq(min(tst$datetime)-6,max(tst$datetime), by = "1 day"))

#--join and fill
tst.s <- 
  newdates %>%
  left_join(
    tst %>%
      mutate(days_avg = days/7) %>%
      left_join(newdates)
  ) %>%
  fill(days_avg, .direction = "up")


tst.s %>% 
  ggplot(aes(datetime, days_avg)) + 
  geom_point()

tst.more <- 
  tst.s %>% 
  mutate(year = year(datetime),
         month = month(datetime),
         month2 = as.character(month(datetime, label = T, abbr = T)),
         day = day(datetime),
         cat = case_when(
           day < 9 ~ "1-9",
           (day >= 9 & day < 17) ~ "10-17",
           (day >= 17 & day < 25) ~ "18-24",
           (day >= 25 & day) ~ "25-31"),
         cat = paste(month2, cat)) %>% 
  filter(month > 3, month < 6) %>% 
  group_by(year, month, month2, cat) %>% 
  summarise(days_tot = sum(days_avg, na.rm = T)) %>% 
  ungroup() 


tst.more %>% 
  arrange(year, month) %>% 
  mutate(cat = fct_inorder(cat),
         cat_rev = fct_rev(cat)) %>% 
  ggplot(aes(cat_rev, days_tot)) + 
  geom_point() + 
  stat_summary(geom = "point", color = "red", size = 6) +
  coord_flip() +
  guides(color = F)

#--write results
tst.more %>% 
  mutate(cat2 = case_when(
    grepl("Apr 1-9", cat) ~ "early April",
    grepl("Apr 10-17", cat) ~ "early April",
    grepl("Apr 18-24", cat) ~ "late April",
    grepl("Apr 25-31", cat) ~ "late April",
    grepl("May 1-9", cat) ~ "early May",
    grepl("May 10-17", cat) ~ "early May",
    grepl("May 18-24", cat) ~ "late May",
    grepl("May 25-31", cat) ~ "late May"
  )) %>% 
  mutate(year_span = "1989-2019") %>% 
  write_csv("dat_workable-days.csv")


#--look at it
tst.more %>% 
  mutate(morethan2 = ifelse(days_tot >2, ">2", "less than 2")) %>% 
  group_by(cat) %>% 
  mutate(ntot = n()) %>% 
  group_by(cat, morethan2, ntot) %>% 
  summarise(n = n()) %>% 
  mutate(pct_chance_2ormorewds = n/ntot) %>% 
  filter(morethan2 == ">2") %>% 
  mutate(cat = fct_inorder(cat),
         cat_rev = fct_rev(cat)) %>% 
  ggplot(aes(cat_rev, pct_chance_2ormorewds)) + 
  geom_col() + 
  coord_flip() + 
  labs(title = "Probability of having more than\n2 working days")
