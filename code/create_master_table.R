## Generate master table
library(tidyverse)
## Need all destinations
destination_countries <- c("Spain", "United States", "Algeria", "Nigeria", "United Kingdom", 
  "Ethiopia", "Australia", "Netherlands", "Ghana", "New Zealand", 
  "Cote D'Ivoire", "Russia", "Japan", "United Arab Emirates", "Malaysia", 
  "Philippines", "Indonesia", "Sweden", "Germany", "Thailand", 
  "Viet Nam", "Egypt", "Brazil", "Tanzania", "Senegal", "South Africa", 
  "Guinea", "Argentina", "Korea (South)", "Morocco", "Hong Kong (SAR)", 
  "Zimbabwe", "Congo (Kinshasa)", "Sudan", "Taiwan", "Angola", 
  "Zambia", "Mauritius", "Cambodia", "Gabon", "Mauritania", "Madagascar", 
  "Singapore", "Canada", "Chile", "Seychelles", "Equatorial Guinea", 
  "Tunisia", "Kenya", "Macao (SAR)", "Uganda", "Mozambique")

highsurv_countries <-c('United States','Australia','Canada','Korea (South)',
                                  'United Kingdom',
                                  'Netherlands','Sweden',
                                  'Germany','Spain','Singapore')
african_countries<-c('Mauritius','Mauritania','South Africa','Kenya','Egypt','Ethiopia','Morocco',
                              'Algeria','Nigeria','Ghana','Tanzania','Senegal','Guinea',
                              'Zimbabwe','Congo (Kinshasa)','Sudan','Angola','Zambia',
                              'Gabon','Madagascar','Equatorial Guinea','Tunisia',
                              'Uganda','Mozambique','Seychelles',"Cote D'Ivoire")

global_countries <- c("New Zealand", "Russia", "Japan", "United Arab Emirates", "Malaysia", 
                      "Philippines", "Indonesia", "Thailand", "Viet Nam", "Brazil", 
                      "Argentina", "Hong Kong (SAR)", "Taiwan", "Cambodia", "Chile", 
                      "Macao (SAR)") %>% 
  c(.,"United States","Australia","Canada","Korea (South)","Germany","Spain","Singapore") %>% 
  c(.,"South Africa","Kenya","Ethiopia","Nigeria")


## Need all origins - each Chinese city
origin_cities <- c("Hefei", "Beijing", "Chongqing", "Fuzhou", "Guangzhou", "Dongguan", 
                    "Shenzhen", "Zhengzhou", "Wuhan", "Changsha", "Nanjing", "Nanchang", 
                    "Xi'an", "Shanghai", "Chengdu", "Tianjin", "Hangzhou", "Jiaxing")

## All scenarios 1-5
scenarios <- c("Scenario 1","Scenario 2", "Scenario 3","Scenario 4","Scenario 5")

## All dates 2019-12-08 to 2020-02-29
dates <- seq(as.Date("2019-11-01"),as.Date("2020-03-03"),by="1 day")
names(dates) <- paste0("day",1:124)
table_key <- expand_grid(date=dates,origin_city=origin_cities, destination_country=destination_countries, scenario=scenarios)

############################
## Read in all prevalences
############################
prev_cities <- read_csv("./out/prev_all_scenarios_combined.csv") #%>% select(-X1)
prev_cities <- prev_cities %>% dplyr::select(-X1)
prev_cities_4_5 <- read_csv("./out/prev_scenarios_4_5_combined.csv")

prev_all <- bind_rows(prev_cities, prev_cities_4_5)
prev_all <- prev_all %>% pivot_longer(-c("cities","Scenario"))
prev_all$name <- dates[prev_all$name]
colnames(prev_all) <- c("origin_city","scenario","date","prevalence_o")
scenario_key <- c("Intermediate"="Scenario 1","Lower"="Scenario 2", "Upper"="Scenario 3", "Scenario 4"="Scenario 4","Scenario 5"="Scenario 5")
prev_all$scenario <- scenario_key[prev_all$scenario]
prev_all <- prev_all %>% distinct()

############################
## Get all flight data
############################
flights_all_cities <- read_csv("data/all_flight_pairs.csv")
flights_all_cities <- flights_all_cities %>% group_by(date, origin_city, destination_country) %>%
  summarise(daily_volume=sum(daily_volume))
flights_all_cities <- flights_all_cities %>% ungroup() %>% mutate(origin_city = ifelse(origin_city == "Xi An","Xi'an",origin_city))

############################
## Ascertainment rates
############################
ascertainment_rates <- tibble(scenario=c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5"),
                              asct_rate=c(0.0973, 0.145, 0.0922, 0.0973, 1))

############################
## Alpha factors
############################
alphas <- tibble(scenario=c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5"),
                 alpha=c(1.522779, 2.412217, 1.597522, 1.522779, 1.522779))


############################
## has detected cases
############################
load(file="./out/hasdetected.Rdata")
african_countries %>% length() # 26
dates %>% length() # 124
df_hasdetected %>% filter( destination_country%in%african_countries ) -> df_hasdetected

############################
## Combine all
############################
## Different names
setdiff(unique(flights_all_cities$origin_city),unique(prev_all$origin_city))
comb1 <- full_join(prev_all, table_key)
comb2 <- full_join(comb1, flights_all_cities)
comb3 <- full_join(comb2, ascertainment_rates)
comb4 <- full_join(comb3, alphas)

all_dat <- comb4 %>% 
  mutate(asct_rate = ifelse(origin_city == "Wuhan", 1, asct_rate),
        is_wuhan=ifelse(origin_city=="Wuhan",1,0),
         is_prelockdown_date=ifelse(date <= as.Date("2020-01-23"),1,0),
         is_africa_d=ifelse(destination_country %in% african_countries, 1, 0),
         is_global_d=ifelse(destination_country %in% global_countries, 1, 0),
         is_highsurv_d=ifelse(destination_country %in% highsurv_countries, 1, 0)) %>%
  rename(fvolume_od=daily_volume) %>% 
  left_join( df_hasdetected, by=c("destination_country","date") ) %>% 
  mutate( has_detected_d=ifelse( ( is_africa_d==1 & is.na(has_detected_d) ),0,has_detected_d  ) ) 

write_csv(all_dat, "data/master_table.csv")

## Get peak time by city
all_dat %>% select(origin_city, date, prevalence_o, scenario) %>% 
  group_by(origin_city, scenario) %>% 
  filter(prevalence_o == max(prevalence_o)) %>% 
  pull(date) %>%
  range()


  
  