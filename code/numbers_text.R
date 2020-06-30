library(tidyverse)
library(lubridate)
library(dplyr)
#library(tidylog) # have it on when writing new pipes, otherwise overload with messages

############################
## Load the master table
############################
main_scenario <- "Scenario 10" #main_scenario <- "Scenario 9"
mt <- read_csv("./data/master_table.csv",guess_max = Inf) #mt <- read_csv("./data/master_table_2.csv",guess_max = Inf)
mt %>% mutate( fvolume_od = ifelse( is.na(fvolume_od), 0 , fvolume_od ) ) ->mt

# don't require these steps if using updated master table w/ dates subset to focal period : 
# mt_all_dates <- read_csv("./data/master_table.csv",guess_max = Inf)
#dates_seq=seq(as.Date('2019-12-08'),as.Date('2020-02-29'),by="day")
# mt_all_dates %>% mutate( fvolume_od = ifelse( is.na(fvolume_od), 0 , fvolume_od ) ) ->mt_all_dates
# mt_all_dates$date=as.Date(mt_all_dates$date)
# mt <- mt_all_dates%>%filter(date%in%dates_seq)
mt %>% glimpse()

############################
## alpha
############################
mt %>% group_by(scenario) %>% 
  summarise(alpha_m=mean(alpha) ) %>% print() %>% 
  #
  mutate(alpha_lower=min(alpha_m),
            alpha_upper=max(alpha_m)) %>% filter(scenario==main_scenario) # 1.52; 1.52-2.41
# from manuscript
# each unit force of importation predicts 1.72 (range: 1.52 - 2.4) imported cases

############################
## global Ratio Wuhan/non-Wuhan
############################
mt %>% 
  filter(is_global_d==1) %>% # removed 69%
  mutate( force_imp=prevalence_o*fvolume_od ) %>% # 0% NA
  # by scenario
  group_by(is_wuhan,scenario) %>% 
  summarise( sum_force_imp=sum(force_imp) ) %>% ungroup() %>% 
  #
  pivot_wider(names_from = is_wuhan, values_from = sum_force_imp) %>% 
  set_names( "scenario" , "non_W" , "W"  ) %>% 
  mutate( R=non_W/W,
          frac_W=W/(W+non_W),
          frac_nW=non_W/(W+non_W)) %>% 
  select(-non_W,-W) %>% print() %>% 
  #
  summarise( R_mean=mean(R),
          R_lower=range(R)[1],
          R_upper=range(R)[2],
          frac_W_mean=mean(frac_W),
          frac_W_lower=range(frac_W)[1],
          frac_W_lower=range(frac_W)[1],
          frac_nW_mean=mean(frac_nW),
          frac_nW_lower=min(frac_nW),
          frac_nW_upper=max(frac_nW) ) # R: 
# from manuscript
# for every case from Wuhan imported globally, there may have been 0.6 (range of mean estimates across scenarios: 0.1 - 5.6) imported cases

# for every one case globally imported from Wuhan, 0.6 cases (0.1 - 5.6) may have been imported from outside Wuhan


############################
## Africa Ratio Wuhan/non-Wuhan
############################
mt %>% 
  filter(is_africa_d==1) %>% 
  mutate( force_imp=prevalence_o*fvolume_od ) %>% 
  # by scenario
  group_by(is_wuhan,scenario) %>% 
  summarise( sum_force_imp=sum(force_imp) ) %>% ungroup() %>% 
  #
  pivot_wider(names_from = is_wuhan, values_from = sum_force_imp) %>% 
  set_names( "scenario" , "non_W" , "W"  ) %>% 
  mutate( R=non_W/W,
          frac_W=W/(W+non_W),
          frac_nW=non_W/(W+non_W)) %>% 
  select(-non_W,-W) %>% print() %>% 
  #
  summarise( R_mean=mean(R),
             R_lower=range(R)[1],
             R_upper=range(R)[2],
             frac_W_mean=mean(frac_W),
             frac_nW_mean=mean(frac_nW),
             frac_nW_lower=range(frac_nW)[1],
             frac_nW_upper=range(frac_nW)[2],
              )  # R: 0.9 - 10.2, frac_nW= 0.5 - 0.91
# from Chinese cities outside of Wuhan (68%; 36% - 86%)

# For countries in Africa this ratio was 1 (0.1 - 9.8)

############################
## Weekly proportion -global
############################
mt %>% 
  # filter
  filter(is_global_d==1) %>% 
  filter(date>"2019-11-01") %>% 
  mutate( force_imp=prevalence_o*fvolume_od*alpha ) %>% 
  # 
  group_by(date,is_wuhan,scenario) %>% summarise( force_imp_day=sum(force_imp) ) %>% 
  mutate( year=year(date),week=week(date) ) %>% ungroup() %>% 
  # by scenario and week
  group_by(is_wuhan,scenario,year,week) %>% 
  arrange(date) %>% 
  mutate(n=n()) %>% 
  filter( n==7 ) %>% 
  mutate( force_imp_week=sum(force_imp_day) ) %>% 
  dplyr::slice(  1  ) %>% ungroup() %>% dplyr::select(-year,-week,-force_imp_day,-n) %>% 
  #
  pivot_wider(names_from = is_wuhan, values_from = force_imp_week) %>% 
  set_names( "date", "scenario", "non_W" , "W"  ) %>% 
  mutate( tot_imp=(W+non_W),
          min_tot_imp=min(tot_imp[tot_imp!=0]),
          prop_wuhan=W/(tot_imp + min_tot_imp ) ) %>% 
  dplyr::select(-W,-non_W) -> pf_probt
# from where to start
pf_probt %>% 
  group_by( scenario ) %>% 
  mutate( prob_1 = ppois(q=1, lower.tail=F, lambda =tot_imp ) ) %>% 
  ungroup() -> pf_probt
pf_probt %>% group_by(date) %>% 
  mutate( n=n() ) %>% 
  mutate( prob_W_lower=min(prop_wuhan),
          prob_W_upper=max(prop_wuhan)) %>% 
  filter(scenario==main_scenario) %>% print(n=Inf)
pf_probt %>% group_by(date) %>% 
  mutate( n=n() ) %>% 
  mutate( prob_W_lower=min(prop_wuhan),
          prob_W_upper=max(prop_wuhan)) %>% 
  filter(scenario==main_scenario) %>%
  ggplot() + geom_line(aes(x=date,y=prop_wuhan))
# from manuscript
# the main source of global case exportation in early January was Wuhan (98%; 98%-99%), but due to the Wuhan lockdown and the rapid spread of the virus, 
## main source of case exportation from mid February changed to cities outside of Wuhan (100% across scenarios)

# early on in the pandemic, the majority of imported cases originated in Wuhan (98%; 98% - 99% in the week of 1st January 2020), but this proportion then changed rapidly. 
## In the week of 19th February, the proportion of globally imported cases sourced in Wuhan drops precipitously to 0% (across scenarios)


############################
## Weekly proportion -africa
############################
mt %>% 
  # filter
  filter(is_africa_d==1) %>% 
  filter(date>"2019-11-01") %>% 
  mutate( force_imp=prevalence_o*fvolume_od*alpha ) %>% 
  # 
  group_by(date,is_wuhan,scenario) %>% summarise( force_imp_day=sum(force_imp) ) %>% 
  mutate( year=year(date),week=week(date) ) %>% ungroup() %>% 
  # by scenario and week
  group_by(is_wuhan,scenario,year,week) %>% 
  arrange(date) %>% 
  mutate(n=n()) %>% 
  filter( n==7 ) %>% 
  mutate( force_imp_week=sum(force_imp_day) ) %>% 
  dplyr::slice(  1  ) %>% ungroup() %>% dplyr::select(-year,-week,-force_imp_day,-n) %>% 
  #
  pivot_wider(names_from = is_wuhan, values_from = force_imp_week) %>% 
  set_names( "date", "scenario", "non_W" , "W"  ) %>% 
  mutate( tot_imp=(W+non_W),
          min_tot_imp=min(tot_imp[tot_imp!=0]),
          prop_wuhan=W/(tot_imp + min_tot_imp ) ) %>% 
  dplyr::select(-W,-non_W) -> pf_probt
# from where to start
pf_probt %>% 
  group_by( scenario ) %>% 
  mutate( prob_1 = ppois(q=1, lower.tail=F, lambda =tot_imp ) ) %>% 
  ungroup() -> pf_probt
pf_probt %>% group_by(date) %>% 
  mutate( n=n() ) %>% 
  mutate( prob_W_lower=min(prop_wuhan),
          prob_W_upper=max(prop_wuhan)) %>% 
  filter(scenario==main_scenario) %>% print(n=Inf)
# from manuscript

# Wuhan contributed modestly to all imported cases throughout the time period, with its highest contribution estimated to be 57% (24% - 79%) in the early pandemic (the week of 8th January), subsequently declining to 0% in mid February (the week of 19th February)

############################
## total number of predicted cases for Africa
############################
mt %>% 
  filter(is_africa_d==1) %>% 
  mutate( imp_number=prevalence_o*fvolume_od*alpha ) %>% 
  # by scenario
  group_by(scenario) %>% 
  summarise( sum=sum(imp_number) ) %>% print() %>% 
  summarise(sum_mean=mean(sum),
            sum_lower=min(sum),
            sum_upper=max(sum)) # 
# from manuscript
# mean number of 19 (10 - 101) COVID-19 cases were imported to 26 destinations in Africa

# our model estimates 19 (10 - 101) imported COVID-19 cases

# until the end of February (29 February 2020) 19 (range: 10 - 101) COVID-19 cases from all of China could have been imported

# only from Wuhan
mt %>% 
  filter(is_africa_d==1,is_wuhan==1) %>% 
  mutate( imp_number=prevalence_o*fvolume_od*alpha ) %>% 
  # by scenario
  group_by(scenario) %>% 
  summarise( sum=sum(imp_number) ) %>% print() %>% 
  summarise(sum_mean=mean(sum),
            sum_lower=min(sum),
            sum_upper=max(sum))
# only from non-Wuhan
mt %>% 
  filter(is_africa_d==1,is_wuhan==0) %>% 
  mutate( imp_number=prevalence_o*fvolume_od*alpha ) %>% 
  # by scenario
  group_by(scenario) %>% 
  summarise( sum=sum(imp_number) ) %>% print() %>% 
  summarise(sum_mean=mean(sum),
            sum_lower=min(sum),
            sum_upper=max(sum))

##
### prior to first detection 
##
mt %>% 
  filter(is_africa_d==1) %>% 
  filter(has_detected_d==0) %>% 
  mutate( imp_number=prevalence_o*fvolume_od*alpha ) %>% 
  # by scenario
  group_by(scenario) %>% 
  summarise( sum=sum(imp_number) ) %>% print() %>% 
  summarise(sum_mean=mean(sum),
            sum_lower=min(sum),
            sum_upper=max(sum)) # 44.8;13.6-73.0
# from manuscript


##
### proportion prior
##
mt %>% 
  filter(is_africa_d==1) %>% 
  mutate( imp_number=prevalence_o*fvolume_od*alpha ) %>% 
  # by scenario
  group_by(scenario,has_detected_d) %>% 
  summarise( sum=sum(imp_number) ) %>% 
  pivot_wider(names_from = has_detected_d,
              values_from = sum) %>% 
  set_names("scenario","prior","after") %>% 
  mutate(prop_prior=prior/(prior+after)) %>% mutate(prop_prior=mean(prop_prior)) %>% ungroup() %>% print() %>% 
  summarise( mean_prop=mean(prop_prior),
             pro_lower=min(prop_prior),
             pro_upper=max(prop_prior) )
# 100% (99.9% - 100%) of predicted imports would have occurred prior to any case detection

# majority of predicted cases (100%; 99.9% - 100%) would have occurred prior to any confirmed cases

# majority predicted on days prior to the first case detections in each location (around 100% for all scenarios)

##
### for individual countries
##
mt %>% 
  filter(is_africa_d==1) %>% 
  mutate( imp_number=prevalence_o*fvolume_od*alpha ) %>% 
  # by scenario
  group_by(destination_country,scenario) %>% 
  summarise( sum=sum(imp_number) ) %>% ungroup() %>% 
  group_by(destination_country) %>% mutate( mean_pred= (sum),
                                            lower=range(sum)[1],
                                            upper=range(sum)[2]) %>% 
  dplyr::filter(scenario==main_scenario) %>% 
  arrange( desc(mean_pred) ) %>% 
  select(-sum) %>% print(n=Inf) # Egypt, SA, Kenya, Ethiopia
# from manuscript
#highest imported case count in South Africa (11; 4 - 19) and fewest cases in Equatorial Guinea (0.1; 0 - 0.13)

# highest numbers of imports are expected for South Africa (11; 3.9 - 18.5) and Egypt (9.5; 1.8 - 15.3), followed by Kenya (4.2; 1.5 - 6.9) and Zambia (3.3; 1.4 - 5.4)

# lowest expected case counts in Mauritania (0.1; 0-0.14) and Equatorial Guinea (0.1; 0 - 0.13) 

# highest numbers are expected for South Africa (11; 3.9 - 18.5) and Egypt (9.5; 1.8 - 15.3), followed by Kenya (4.2; 1.5 - 6.9) and Ethiopia (3.0; 0.5 - 4.8).

#  the lowest numbers for Mauritania (0.1; 0 - 0.2) and for Equatorial Guinea (0.1; 0 - 0.1)

############################
## date range (for majority of case arrival)
############################
mt %>% 
  filter(is_africa_d==1) %>% 
  #filter(scenario=="Scenario 1") %>% 
  mutate( imp_number=prevalence_o*fvolume_od*alpha ) %>% 
  #
  group_by(date,scenario) %>% 
  summarise( sum_daily=sum(imp_number) ) %>% ungroup() %>% 
  arrange( scenario,desc(sum_daily) ) %>% 
  group_by(scenario) %>% 
  #normalise
  mutate( sum_daily=sum_daily/sum(sum_daily) ) %>% 
  mutate( cumsum_daily=cumsum(sum_daily) ) %>% 
  mutate( in_interval=as.numeric(cumsum_daily<=0.90) ) -> pf
# visual inspection
# pf %>% 
#                 #
#                 ggplot( aes(x=date,y=sum_daily, col=as.factor(in_interval)) ) +
#                 geom_point(size=0.6) +
#                 facet_wrap(~scenario,ncol=1)
pf                 %>% filter( in_interval==1 ) %>% 
  summarise( int_start=min(date),
             int_end=max(date)) 
# from manuscript
# between 16th January (±2 day) and 6th February (±2 days)

# 90% of all imported cases were estimated to be imported between 16th January (±2 day) and 6th February (±2 days)

# the majority of case imporations in Africa (90%) occurred between 16th January (±2 day) and 6th February (±2 days)

############################
## understimation
############################
# wuhan detected / all wuhan
1/2.8
# all china / all wuhan


# 1 wuhan detected / 2.8 wuhan real / 2.8*1.96 call of china real
1- 1/(2.8+2.8*0.555) # fraction missed
1- 1/(2.8+2.8*0.0555) # fraction missed - lower
1- 1/(2.8+2.8*5.55) # fraction missed - upper

# 88% (73% - 92%) of all cases imported globally may have been undetected

############################
## Prevalence estimates for the 5 scenarios
############################
prevalence_dat <- mt %>% 
  select(origin_city, scenario, date, prevalence_o) %>% 
  distinct()

############################
## Peak time overall
############################
prevalence_dat %>%
  group_by(origin_city, scenario) %>%
  filter(prevalence_o == max(prevalence_o)) %>%
  group_by(scenario) %>%
  summarise(min_date=min(date),
            max_date=max(date)) 

############################
## Peak prevalence in Jiaxing
############################
prevalence_dat %>%
  filter(origin_city == "Jiaxing") %>%
  group_by(scenario) %>%
  filter(prevalence_o == max(prevalence_o)) %>%
  mutate(prevalence_percentage = prevalence_o * 100) %>%
  ungroup() %>%
  summarise(min_prev = min(prevalence_percentage),
            max_prev = max(prevalence_percentage))

############################
## Peak prevalence in Shanghai
############################
prevalence_dat %>%
  filter(origin_city == "Shanghai") %>%
  group_by(scenario) %>%
  filter(prevalence_o == max(prevalence_o)) %>%
  mutate(prevalence_percentage = prevalence_o * 100) %>%
  ungroup() %>%
  summarise(min_prev = min(prevalence_percentage),
            max_prev = max(prevalence_percentage))

############################
## Peak prevalence in Tianjin
############################
prevalence_dat %>%
  filter(origin_city == "Tianjin") %>%
  group_by(scenario) %>%
  filter(prevalence_o == max(prevalence_o)) %>%
  mutate(prevalence_percentage = prevalence_o * 100) %>%
  ungroup() %>%
  summarise(min_prev = min(prevalence_percentage),
            max_prev = max(prevalence_percentage))

