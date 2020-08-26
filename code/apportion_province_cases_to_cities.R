library(tidyverse)
setwd("~/Documents/GitHub/africa_export/")
dat <- read_csv("data/table_getAreaStat_en_2020-03-19.csv") #%>% select(-comment)
dat <- dat %>% select(-comment)
dat <- dat %>% pivot_longer(c(confirmedCount,suspectedCount,curedCount,deadCount))

used_cities <-c("Wuhan", "Beijing", "Shanghai", "Guangzhou", "Zhengzhou", "Tianjin",
         "Hangzhou", "Jiaxing", "Changsha", "Xi'an", "Nanjing", "Shenzhen", 
         "Chongqing", "Nanchang", 
         "Chengdu", "Hefei", "Fuzhou", "Dongguan")

## Which cities are not in the MIDAS list?
cities_missing <- used_cities[!(used_cities %in% unique(dat$cityName))]

## Try at the province short level instead
cities_as_provinces <- cities_missing[!(cities_missing %in% unique(dat$provinceShortName))]

dat_cities <- dat %>% filter(cityName %in% used_cities ) %>% mutate(city_name_use = cityName)

## Note that it's Fuzhou Fuijain, not Jiangxi
dat_cities <- dat_cities %>% filter(!(cityName == "Fuzhou" & provinceShortName == "Jiangxi"))

dat_provinces <- dat %>% filter(provinceShortName %in% cities_missing) %>% 
  mutate(city_name_use = provinceShortName) %>% 
  filter(is.na(cityName))

dat_use <- bind_rows(dat_cities, dat_provinces) %>% 
  select(-c("provinceName","cityName")) %>% 
  group_by(city_name_use, name,provinceShortName) %>%
  summarize(n=sum(value))

apportioned_dat1 <- dat_use %>% left_join(dat %>% filter(is.na(cityName))) %>% 
  mutate(frac_cases=n/value) %>% 
  filter(name %in% c("confirmedCount","curedCount")) %>%
  select(-c("provinceName","cityName")) %>%
  rename(province_n=value)

apportioned_dat_use <- apportioned_dat1 %>% filter(name == "confirmedCount") %>% 
  ungroup() %>%
  select(city_name_use, provinceShortName, frac_cases) %>%
  rename(province=provinceShortName,
         city=city_name_use,
         frac_cases_reported=frac_cases)


## Read in existing frac allocation
prov_city_adjust <- get_prov_city_adjust(file="./out/frac_popn_city.Rdata" )
prov_city_adjust <- prov_city_adjust %>% full_join(apportioned_dat_use)
frac_popn_city2 <- prov_city_adjust
save(frac_popn_city2,file="./out/frac_popn_city.Rdata")
