
library(dvmisc)
library(tidyverse)
library(rio)
## Generate master table
setwd("~/Desktop/nCoV exports")
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
                     'Uganda','Mozambique','Seychelles', "Cote D'Ivoire")

global_countries_old <- c("New Zealand", "Russia", "Japan", "United Arab Emirates", "Malaysia", 
                          "Philippines", "Indonesia", "Thailand", "Viet Nam", "Brazil", 
                          "Argentina", "Hong Kong (SAR)", "Taiwan", "Cambodia", "Chile", 
                          "Macao (SAR)") 
# new global countries
global_countries <- c("New Zealand","Australia", # only 2 destinations for Oceania
                      "United Kingdom","Germany","Russia", # top 3 dest in Europe (not Spain,Holland,Sweden)
                      "Japan","Thailand","Korea (South)", # top 3 dest in Asia
                      "United States","Canada", # only 2 in North America
                      "Brazil","Argentina","Chile", # top 3 in South America
                      "Egypt","Ethiopia","South Africa") # top 3 in Africa
countries_we_exclude <- c("United Arab Emirates","Malaysia","Philippines",
                          "Indonesia","Viet Nam","Hong Kong (SAR)",
                          "Taiwan","Cambodia","Macao (SAR)")


## Need all origins - each Chinese city
origin_cities <- c("Hefei", "Beijing", "Chongqing", "Fuzhou", "Guangzhou", "Dongguan", 
                   "Shenzhen", "Zhengzhou", "Wuhan", "Changsha", "Nanjing", "Nanchang", 
                   "Xi'an", "Shanghai", "Chengdu", "Tianjin", "Hangzhou", "Jiaxing")

## All scenarios 1-5 , Scenario 6 = Model 0
scenarios <- c("Scenario 1","Scenario 2", "Scenario 3","Scenario 4","Scenario 5","Scenario 6",
               "Scenario 7","Scenario 8","Scenario 10")

## All dates 2019-12-08 to 2020-02-29
dates <- seq(as.Date("2019-11-01"),as.Date("2020-03-03"),by="1 day")
names(dates) <- paste0("day",1:124)
table_key <- as_tibble(expand_grid(date=dates,
                                   origin_city=origin_cities, 
                                   destination_country=destination_countries, 
                                   scenario=scenarios))


############################
## Read in all prevalences
############################
# add Scenarios 1-7
load( file = "city_prev_mod01.Rdata" )
prev_all=city_prev_mod0
load( file = "city_prev_mod02.Rdata" )
bind_rows(prev_all,city_prev_mod0) -> prev_all
load( file = "city_prev_mod03.Rdata" )
bind_rows(prev_all,city_prev_mod0) -> prev_all
load( file = "city_prev_mod04.Rdata" )
bind_rows(prev_all,city_prev_mod0) -> prev_all
load( file = "city_prev_mod05.Rdata" )
bind_rows(prev_all,city_prev_mod0) -> prev_all
load( file = "city_prev_mod06.Rdata" )
bind_rows(prev_all,city_prev_mod0) -> prev_all
load( file = "city_prev_mod07.Rdata" )
bind_rows(prev_all,city_prev_mod0) -> prev_all
load( file = "city_prev_mod08.Rdata" )
bind_rows(prev_all,city_prev_mod0) -> prev_all
load( file = "city_prev_mod10.Rdata" )
bind_rows(prev_all,city_prev_mod0) -> prev_all

############################
## Get all flight data
############################
flights <- import('https://raw.githubusercontent.com/c2-d2/africa_export/master/data/flights_adjusted_200506.csv?token=AHOEEDCAVE5AX4R4UZUM2PS7EGB6O')

## process flight data -- validation step
colnames(flights)[2]<-"iata"
flights$flight_date<-as.Date(flights$flight_date)
#flights[,c("flight_date","origCityName","destCtryName","dailyvol")]

flights_all_cities_duplicated<-flights%>%
  subset(iata=='SZX'|iata=='HGH')%>%
  mutate(dailyvol=dailyvol/2)

flights_all_cities_duplicated_copy<-flights_all_cities_duplicated
flights_all_cities_duplicated_copy$origCityName<-str_replace_all(flights_all_cities_duplicated_copy$origCityName,'Hangzhou','Jiaxing')
flights_all_cities_duplicated_copy$origCityName<-str_replace_all(flights_all_cities_duplicated_copy$origCityName,'Shenzhen','Dongguan')

iata_not_duplicated<-c('CAN','CGO','CKG','CSX','CTU','FOC','HFE','KHN','NAY/PKX',
                       'NKG','PEK','PVG','SHA',"TSN",'XIY','WUH')
flights_all_cities_not_duplicated<-flights%>%
  subset(iata%in%iata_not_duplicated)

flights_all_cities_final<-data.frame(rbind(flights_all_cities_not_duplicated,
                                           flights_all_cities_duplicated,
                                           flights_all_cities_duplicated_copy))

flights_date_all_cities<-cbind.data.frame(flights_all_cities_final$flight_date,flights_all_cities_final$origCityName,
                                          flights_all_cities_final$destCtryName, flights_all_cities_final$dailyvol)
colnames(flights_date_all_cities)<-c("date","origin_city","destination_country","daily_volume")

#write.csv(flights_date_all_cities,"all_flight_pairs_0630.csv",row.names=F)


flights_all_cities_2<-flights_date_all_cities %>%
  group_by(date, origin_city, destination_country) %>%
  summarise(daily_volume=sum(daily_volume))

flights_all_cities2$origin_city<- ifelse(flights_all_cities2$origin_city == "Xi An","Xi'an",
                                         as.character(flights_all_cities2$origin_city))
#flights_all_cities2$date<-as.Date(flights_all_cities2$date)


#flights_dec=flights_all_cities[flights_all_cities$date=='2019-12-01'&flights_all_cities$destination_country%in%african_countries,]
############################
## Ascertainment rates
############################
#ascertainment_rate_ratios <- tibble(scenario=c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5", "Scenario 6","Scenario 7"),
#       asct_rate=c(1.4,2,2,2,0.2,20,2))

############################
## Alpha factors
############################
# alphas <- tibble(scenario=c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5"),
#                  alpha=c(1.522779, 2.412217, 1.597522, 1.522779, 1.522779))


############################
## has detected cases
############################
load(file="hasdetected.Rdata")
african_countries %>% length() # 26
dates %>% length() # 124
df_hasdetected %>% filter( destination_country%in%african_countries ) -> df_hasdetected

############################
## Combine all
############################
## Different names
setdiff(unique(flights_all_cities2$origin_city),unique(prev_all$origin_city))
comb1 <- full_join(prev_all, table_key)
comb2 <- full_join(comb1, flights_all_cities2)
#comb3 <- full_join(comb2, ascertainment_rate_ratios)
#comb4 <- full_join(comb3, alphas)
comb4 <-  comb2

dates_seq=seq(as.Date('2019-12-01'),as.Date('2020-02-29'),by="day")

all_dat <- comb4 %>% 
  mutate(asct_rate = 1, #ifelse(origin_city == "Wuhan", 1, asct_rate),
         is_wuhan=ifelse(origin_city=="Wuhan",1,0),
         is_prelockdown_date=ifelse(date <= as.Date("2020-01-23"),1,0),
         is_africa_d=ifelse(destination_country %in% african_countries, 1, 0),
         is_global_d=ifelse(destination_country %in% global_countries, 1, 0),
         is_highsurv_d=ifelse(destination_country %in% highsurv_countries, 1, 0)) %>%
  rename(fvolume_od=daily_volume) %>% 
  left_join( df_hasdetected, by=c("destination_country","date") ) %>% 
  mutate( has_detected_d=ifelse( ( is_africa_d==1 & is.na(has_detected_d) ),0,has_detected_d  ) ) %>% 
  mutate( fvolume_od=replace_na(fvolume_od,replace=0) ) %>%
  filter( date%in%dates_seq)

# generate alphas by fitting each scenario, using date sequence for focal period
df_alphas <- generate_alphas( all_dat, file_obs_cnt="./who_imports.csv" )
all_dat %>% left_join( df_alphas, by="scenario" ) -> all_dat

write.csv(all_dat,"master_table_0630.csv",row.names=F)

