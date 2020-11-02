## 29th October 2020
## PRE-COMPUTATION SCRIPT D - apportion incidence to Chinese cities by different methods (population, % of cases)
## CRUCIAL NOTES
## - For cities we use that are included in the cityName column, we just find the proportion of cases 
##   in that province reported in that city
## - For "cities" we use that actually correspond to provinces (see https://en.wikipedia.org/wiki/Direct-administered_municipalities_of_China)
##   we just use the total number of that "province" ie. the Beijing cities account for 100% of cases in Beijing

setwd(main_wd)

## These data originally from
dat <- read_csv("data/midas/table_getAreaStat_en_2020-03-13.csv") #%>% select(-comment)
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
         by_cases=frac_cases)


## Read in existing frac allocation

# read in popn size data
popn_size_cities_all<-read.csv("./data/popn_estimates_cities_china.csv")

popn_size_select_cities<-popn_size_cities_all%>%
  subset(asciiname%in%c("Wuhan", "Beijing", "Shanghai", "Guangzhou", "Zhengzhou", "Tianjin",
                        "Hangzhou", "Jiaxing", "Changsha", "Xi'an", "Nanjing", "Shenzhen", 
                        "Chongqing", "Nanchang", 
                        "Chengdu", "Hefei", "Fuzhou", "Dongguan"))

order<-c("Wuhan", "Beijing", "Shanghai", "Guangzhou", "Zhengzhou", "Tianjin",
         "Hangzhou", "Jiaxing", "Changsha", "Xi'an", "Nanjing", "Shenzhen", 
         "Chongqing", "Nanchang", 
         "Chengdu", "Hefei", "Fuzhou", "Dongguan")

popn_size_select_cities<-popn_size_select_cities[match(order,popn_size_select_cities$asciiname),]
popn_size_select_cities$province<-c('Hubei','Beijing','Shanghai','Guangdong','Henan',
                                    'Tianjin','Zhejiang','Zhejiang','Hunan','Shaanxi','Jiangsu','Guangdong','Chongqing',
                                    'Jiangxi','Sichuan','Anhui','Fujian','Guangdong')
popn_size_select_cities %>% select( asciiname,population ) -> df_city_pop
save(df_city_pop,file="./data/df_city_pop.Rdata")

popn_size_provinces<-read.csv("./data/provinces_popn_size_statista.csv")
# merge province & population data at the city level 
merged_popn_data<-merge(popn_size_select_cities,popn_size_provinces,by="province")

# calculate the fraction of total population size per city
frac_popn_city<-merged_popn_data%>%
  group_by(province)%>%
  mutate(fraction=population/popn_size_province)

frac_popn_city %>% 
  select(province,asciiname,fraction) %>% 
  rename( city=asciiname, f_pop_city_prov=fraction ) -> frac_popn_city2
save(frac_popn_city2,file="./data/frac_popn_city.Rdata")
#save( frac_popn_city2, file="./data/frac_popn_city.Rdata" )
prov_city_adjust <- get_prov_city_adjust(file="./data/frac_popn_city.Rdata" )
prov_city_adjust <- prov_city_adjust[,1:4]
colnames(prov_city_adjust)[1:4] <- c("province","city","by_pop","by_city")
prov_city_adjust <- prov_city_adjust %>% full_join(apportioned_dat_use)
frac_popn_city2 <- prov_city_adjust
save(frac_popn_city2,file="./data/frac_popn_city.Rdata")

## Test apportioning code
#load(file="./data/prov_inc_calibrated.RData")
#apportion_default <- adjust_prov_prev_by_city(prov_inc_calibrated,prov_city_adjust,assignment="by_city") %>% mutate(ver="default")
#apportion_by_pop <- adjust_prov_prev_by_city(prov_inc_calibrated,prov_city_adjust,assignment="by_pop") %>% mutate(ver="by_pop")
#apportion_by_cases <- adjust_prov_prev_by_city(prov_inc_calibrated,prov_city_adjust,assignment="by_cases") %>% mutate(ver="by_cases")

#apportioned_dat_combined <- bind_rows(apportion_default, apportion_by_pop, apportion_by_cases)

#ggplot(apportioned_dat_combined) + 
#  geom_line(aes(x=date,y=n_infected_caladj,col=ver)) + 
#  facet_wrap(~city, scales="free_y")

