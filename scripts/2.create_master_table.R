## 29th October 2020
## SCRIPT 2 - Compiles the prevalence estimates and all data, producing a "master table" which contains all of the 
##            pre-calculations and info to produce all figures and numbers in the main manuscript

## Set the global directory as specified in headers.R
setwd(main_wd)

## All scenarios 
scenarios <- c("Scenario 1","Scenario 2", "Scenario 3","Scenario 4","Scenario 5","Scenario 6",
               "Scenario 7","Scenario 8","Scenario 9")

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
load( file = "./out/city_prev_mod01.Rdata" )
prev_all=city_prev_mod0
load( file = "./out/city_prev_mod02.Rdata" )
bind_rows(prev_all,city_prev_mod0) -> prev_all
load( file = "./out/city_prev_mod03.Rdata" )
bind_rows(prev_all,city_prev_mod0) -> prev_all
load( file = "./out/city_prev_mod04.Rdata" )
bind_rows(prev_all,city_prev_mod0) -> prev_all
load( file = "./out/city_prev_mod05.Rdata" )
bind_rows(prev_all,city_prev_mod0) -> prev_all
load( file = "./out/city_prev_mod06.Rdata" )
bind_rows(prev_all,city_prev_mod0) -> prev_all
load( file = "./out/city_prev_mod07.Rdata" )
bind_rows(prev_all,city_prev_mod0) -> prev_all
load( file = "./out/city_prev_mod08.Rdata" )
bind_rows(prev_all,city_prev_mod0) -> prev_all
load( file = "./out/city_prev_mod09.Rdata" )
bind_rows(prev_all,city_prev_mod0) -> prev_all


############################
## Get flight data
############################
flights_all_cities2 <- read.csv('./data/flights_all_cities2.csv')

############################
## has detected cases
############################
load(file="./data/hasdetected.Rdata")
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
df_alphas <- generate_alphas( all_dat, file_obs_cnt="data/who_imports.csv" )

all_dat %>% left_join( df_alphas, by="scenario" ) -> all_dat

write.csv(all_dat,"./data/master_table_1029.csv",row.names=F)
