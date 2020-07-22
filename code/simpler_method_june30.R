library(stats)
library(tidyverse)
library(patchwork)
library(RColorBrewer)
library(rio)

create_scenario <- 10

if(create_scenario == 1) {
  asc_nonhubei_v_hubei <- 1.4 # relative ascertainment rate non-hubei versus hubei (for example 5 for a 1:5 ratio of Hubei versus non-Hubei ascertainment rate)
  name_scenario <- "Scenario 1" # Maier and Brockmann
  prev_days <- 5
  save_name <- "./out/city_prev_mod01.Rdata"
}
#
if(create_scenario == 2) {
  asc_nonhubei_v_hubei <- 2 # Verity et al
  name_scenario <- "Scenario 2"
  prev_days <- 5
  save_name <- "./out/city_prev_mod02.Rdata"
  #
}
if(create_scenario == 3) { 
  asc_nonhubei_v_hubei <- 2 # Verity et al
  name_scenario <- "Scenario 3"
  prev_days <- 2
  save_name <- "./out/city_prev_mod03.Rdata"
  #
}
if(create_scenario == 4) { 
  asc_nonhubei_v_hubei <- 2 # Verity et al
  name_scenario <- "Scenario 4"
  prev_days <- 7
  save_name <- "./out/city_prev_mod04.Rdata"
  #
}
if(create_scenario == 5) {  
  asc_nonhubei_v_hubei <- 0.2 # Verity et al / 10
  name_scenario <- "Scenario 5"
  prev_days <- 5
  save_name <- "./out/city_prev_mod05.Rdata"
  #
}
if(create_scenario == 6) {  
  asc_nonhubei_v_hubei <- 20 # Verity et al * 10 
  name_scenario <- "Scenario 6"
  prev_days <- 5
  save_name <- "./out/city_prev_mod06.Rdata"
  #
}
if(create_scenario == 7) {  
  asc_nonhubei_v_hubei <- 2 # Verity et al
  name_scenario <- "Scenario 7"
  prev_days <- 5
  save_name <- "./out/city_prev_mod07.Rdata"
}
if(create_scenario == 8) {  
  asc_nonhubei_v_hubei <- 2 # Verity et al
  name_scenario <- "Scenario 8"
  prev_days <- 5
  save_name <- "./out/city_prev_mod08.Rdata"
}
if(create_scenario == 10) {  
  asc_nonhubei_v_hubei <- 1 # Verity et al
  name_scenario <- "Scenario 10"
  prev_days <- 5
  save_name <- "./out/city_prev_mod10.Rdata"
}

############################
## read in confirmed case data from James' covback repository
############################
confirmed_cases<-read.csv("./data/midas_data_final.csv",stringsAsFactors=FALSE) %>% as_tibble()
# subset to only provinces used in our analysis
provinces<-c('Hubei','Beijing','Shanghai','Guangdong','Henan',
             'Tianjin','Zhejiang','Hunan','Shaanxi','Jiangsu','Chongqing',
             'Jiangxi','Sichuan','Anhui','Fujian') # 15
confirmed_cases_final<-confirmed_cases[confirmed_cases$province_raw%in%provinces,] # 124 unique dates
# match date indices to actual dates
dates_and_date=tibble(dates=seq(as.Date('2020-03-02')-122,as.Date('2020-03-02'),by="day")) %>% 
  mutate(date=(1:n()-1))
#dates_and_date= tibble(dates=seq(as.Date('2020-06-18')-227,as.Date('2020-06-18'),by="day")) %>% 
#mutate(date=(1:n()-1) )# 123 unique dates
confirmed_cases_date <- left_join( confirmed_cases_final,dates_and_date, by="date" )

# remove outliers in Wuhan
which(confirmed_cases_date$province_raw=="Hubei" & confirmed_cases_date$n>4000 ) -> which_replace
confirmed_cases_date$n[ c((which_replace[1]-1),(which_replace[2]+1)) ] -> put_instead
confirmed_cases_date$n[ which_replace ] <- put_instead

# backculation: shift by mean reporting delays & incubation period
all_incidence_province <- shift_2_delays(confirmed_cases_date,incubation_period=-5,delay=-7)

if(create_scenario == 10){
  all_incidence_province <- import("./data/tsang_predictions_apportioned.csv") %>%
    rename(n=n_predict) %>%
    mutate(date=as.Date(date))%>%
    mutate(date_full=date) %>%
    rename(dates=date) %>%
    filter(ver == "Overall")
  all_incidence_province <- shift_2_delays(all_incidence_province,incubation_period=-5,delay=0)
}

# plot the results
p <- plot_conf_onset(all_incidence_province,confirmed_cases_date)

# compute cumulative incidence and add population size
prov_cum_incidence <- comp_cum_incidence( all_incidence_province, 
                                          "./data/provinces_popn_size_statista.csv" ) # 15 rows

# add calibration value
calibration_value <- tibble(  is_hubei=c(0,1),
                              calv=c(asc_nonhubei_v_hubei,1) ,
                              calv_inverse=c(1, 1/asc_nonhubei_v_hubei))

# calibrate incidence in Hubei and outside

prov_inc_calibrated <- all_incidence_province %>% 
  mutate(is_hubei=as.numeric(province_raw=="Hubei") ) %>% 
  left_join( calibration_value, by="is_hubei" ) %>% 
  mutate( n_infected_cal=n_infected/calv_inverse ,
          n_onset_cal = n_onset/calv_inverse) %>% 
  select( dates,province_raw,n_infected_cal, n_onset_cal) %>%
  rename(n_onset=n_onset_cal)
# plot calibrated incidence & symptom onset curves

# distribute the cases into cities and add denominator
prov_city_adjust <- get_prov_city_adjust(file="./out/frac_popn_city.Rdata" )
## If scenario 8 (aportion cases proportional to city's fractional share of province population), then per-capita incidence should
## be the same within each province. Otherwise, is different.
city_n_inf_caladj <- adjust_prov_prev_by_city( prov_inc_calibrated , 
                                               prov_city_adjust, 
                                               aportion_all = create_scenario!=8)
load(file="./out/df_city_pop.Rdata")
city_n_inf_caladj_den <- city_n_inf_caladj %>% 
  left_join(df_city_pop,by=c("city"="asciiname")) %>% 
  mutate(n_infected_caladj=n_infected_caladj/population) %>% select(-population)

# compute travel relevant prevalence
# for scenarios 1-6, 8:
prov_inc_prev_cali <- comp_travel_rel_prev(city_n_inf_caladj_den, rel_dur = prev_days) # change rel_dur to # days prevalent for scenario
prov_inc_prev_cali_raw <- comp_travel_rel_prev(city_n_inf_caladj, rel_dur = prev_days) 

# for scenario 7:
if(create_scenario == 7){
  prov_inc_prev_cali <- comp_travel_rel_prev_nonwuh_gap(city_n_inf_caladj_den) 
}

## Sense check
## Plot incidence and prevalence together
#prov_inc_prev_cali %>% ggplot() + 
 # geom_line(aes(x=date,y=travel_prev)) + 
 # geom_line(data=city_n_inf_caladj_den, aes(x=date,y=n_infected_caladj),col="red") + 
  #facet_wrap(~city,scales="free_y")

# rename columns for master table
city_prev_mod0 <- prov_inc_prev_cali %>% 
  rename( origin_city=city,
          prevalence_o=travel_prev) %>% 
  mutate(scenario=name_scenario) %>% 
  select(origin_city,scenario,date,prevalence_o)

# fill in missing dates for master table
seq(from=ymd("2019-11-01"),to=ymd("2020-03-03"), by=1  ) -> dates_mt # 124 dates
expand_grid(origin_city=unique(city_prev_mod0$origin_city),
            scenario=name_scenario,
            date=dates_mt) %>% left_join( city_prev_mod0, by=c("origin_city","scenario","date") ) %>% 
  mutate(prevalence_o=replace_na(prevalence_o,replace = 0)) -> city_prev_mod0

# save
save( city_prev_mod0, file = save_name )


