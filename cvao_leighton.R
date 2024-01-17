library(tidyverse)
library(lubridate)
library(janitor)
library(openair)


# Reading in data ---------------------------------------------------------

nox = read.csv("data/NOx_2014-2022.csv") %>% 
  tibble() %>% 
  mutate(date = dmy_hm(date)) %>% 
  remove_empty() %>% 
  clean_names() %>% 
  mutate(no_ppt_v_ozone_corrected = ifelse(no_ozone_corrected_flag_v2 > 0.148,
                                           NA_real_,no_ppt_v_ozone_corrected),
         no2_diode_ppt_v_ozone_corrected = ifelse(no2_diode_ozone_corrected_flag_v3 > 0.148,
                                                  NA_real_,no2_diode_ppt_v_ozone_corrected)) %>% 
  select(date,o3,no = no_ppt_v_ozone_corrected,
         no2_blc = no2_ppt_v_ozone_corrected,no2_diode = no2_diode_ppt_v_ozone_corrected)

air_mass = read.csv("data/cvao_air_masses.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hm(X)) %>% 
  remove_empty() %>% 
  clean_names() %>% 
  select(date,everything(),-x)

dat = nox %>% left_join(air_mass,by = "date") 

# Air mass data tidying ---------------------------------------------------

#creating a flag if in a specified year any of the air masses has changed more than 1% in a day
air_mass_flag = air_mass %>% 
  filter(date > "2021-01-01" & date < "2022-01-01") %>% 
  mutate(doy = yday(date)) %>% 
  select(-date) %>% 
  group_by(doy) %>% 
  summarise(across(c(upwelling:south_atlantic), ~ diff(range(.x, na.rm = TRUE)),
                   .names = "range_{.col}")) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(flag = ifelse(any(c(range_upwelling:range_south_atlantic) < 1),1,0))

dat21 = dat %>% 
  filter(date > "2021-01-01" & date < "2022-01-01") %>% 
  mutate(doy = yday(date)) %>% 
  left_join(select(air_mass_flag,doy,flag),by = "doy")


# Daily ozone changes -----------------------------------------------------

dat21_ozone = dat21 %>% 
  # filter(flag == 0) %>% 
  mutate(hour = hour(date),
         o3_five = ifelse(hour == 17,o3,NA_real_),
         o3_nine = ifelse(hour == 9,o3,NA_real_),
         no = ifelse(hour >= 11 & hour <= 15,no,NA_real_)) %>% 
  timeAverage("1 day") %>% 
  mutate(delta_o3 = o3_five - o3_nine) %>%
  rollingMean(pollutant = "delta_o3",width = 30)

dat21_ozone %>% 
  ggplot() +
  geom_point(aes(date,delta_o3),size = 0.8) +
  geom_path(aes(date,rolling30delta_o3),linewidth = 0.8)

