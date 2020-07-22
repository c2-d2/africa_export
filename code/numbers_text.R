library(tidyverse)
library(lubridate)
library(dplyr)
#library(tidylog) # have it on when writing new pipes, otherwise overload with messages

############################
## Load the master table
############################
main_scenario <- "Scenario 10" 
#mt <- read_csv("./data/master_table_2.csv",guess_max = Inf) 
#mt <- read_csv("./data/master_table.csv",guess_max = Inf)
#mt <- read_csv("./data/master_table_0630.csv",guess_max = Inf)
#mt <- read_csv("/Users/taylorchin/Dropbox (Harvard University)/nCoV export/master_table_0630.csv",guess_max = Inf)
#mt <- read_csv("/Users/taylorchin/Dropbox (Harvard University)/nCoV export/master_table_0630-FINAL.csv",guess_max = Inf)
mt <- read_csv("/Users/taylorchin/Dropbox (Harvard University)/nCoV export/master_table_0630.csv",guess_max = Inf)

mt %>% mutate( fvolume_od = ifelse( is.na(fvolume_od), 0 , fvolume_od ) ) ->mt

mt %>% glimpse()

############################
## alpha
############################
mt %>% group_by(scenario) %>% 
  summarise(alpha_m=mean(alpha) ) %>% print() %>% 
  #
  mutate(alpha_lower=min(alpha_m),
            alpha_upper=max(alpha_m)) %>% filter(scenario==main_scenario) # 



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
#  abstract: for every case from Wuhan imported globally, there may have been 0.6 (range of mean estimates across scenarios: 0.1 - 5.6) imported cases
#  results: we estimated that globally for every one case imported from Wuhan, 0.6 cases (0.1 - 5.6) may have been imported from outside Wuhan. 


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
              )  # R

# from manuscript
#  abstract: (1 case from those cities for each case from Wuhan, range of model scenarios: 0.1-9.8)
#  results: for countries in Africa this ratio was 1 (0.1 - 9.8)

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
  #filter(scenario==main_scenario) %>%
  ggplot() + geom_line(aes(x=date,y=prop_wuhan,col=scenario))


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
# CHECK THIS
#  results: We found that early on in the pandemic, the majority of imported cases originated in Wuhan (99%; 58%-100% in the week of 1st January 2020), but this proportion then changed rapidly. ... dropping precipitously to 0% in the week of 19th February
#  discussion: Our model predicts for the early pandemic that between 58%-100% of globally imported cases came from Wuhan in the week of 1st January 2020, with the rest originating from other Chinese cities. We find this proportion  dropped to 0% in the week of 19th February. 

## where is this: 
#  results: For the African destinations in our analysis, Wuhan contributed slightly less early in the epidemic (97%; 33% - 98% in the week of 1st January 2020, where 33% corresponds to Scenario 2), subsequently declining in early February to 0% (across scenarios) in mid February (the week of 19th February).
#  discussion: We found that for the African locations the average proportion of cases exported from Wuhan in the week of 1st January 2020 was slightly lower than that of all destinations, ranging from 33%-98% in the week of 1st January 2020, but similarly declining to 0% in mid-February.

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
#  abstract: Our model predicts that 18.4 (8.5 - 100) COVID-19 cases were imported to 26 destination countries in Africa
#  results: Our model estimates in our African destinations a total of 18.4 (8.5 - 100) imported COVID-19 cases
#  results: (Scenario 8) However, overall trends in the number of imported cases over time in the African destination countries remained relatively unchanged, with the exception of Egypt, as did the total number of imports (20.6). 
#  discussion: Our model predicted that until the end of February (29 February 2020) 18.4 (range: 8.5 - 100) COVID-19 cases from all of China could have been imported to the 26 African destinations included here.

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

#from manuscript:
#  results: Our model estimates in our African destinations a total of 18.4 (8.5 - 100) imported COVID-19 cases, with approximately 100% predicted on days prior to the first case detections in each location.
#  Figure 2: The vast majority of predicted cases (100%; 99.9% - 100%) would have occurred prior to any confirmed cases in those locations. 
# CHECK THIS: update this to 100% - 100%??

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
# results: The highest numbers of imports are expected for South Africa (4.5; 1.7 - 18) and Egypt (3.9; 1.0 - 33), followed by Kenya (2.0; 0.8 - 9.2) and Algeria (1.6; 0.2 - 2.2). We estimate the lowest expected case counts in Equatorial Guinea and Mauritania (both 0.04; 0 - 0.2)


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
#  abstract: with most of them (90%) predicted to have arrived between 7th January (±10 days) and 5th February (±3 days)
#  results: 90% of all imported cases were estimated to be imported between 7th January (±10 days) and 5th February (±3 days).  
#  discussion: It predicted that the majority (90%) of case importations in these locations occurred between 7th January (±10 day) and 5th February (±3 days). 


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
  select(origin_city, scenario, date, prevalence_o,is_wuhan) %>% 
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

# from manuscript:
##  results: Under these assumptions, we found that the prevalence indicator peaked in all Chinese cities between 19th and 26th January 2020 (Figure 1 C&D and Figure S3).

############################
## Ratio of prevalence indicator in to outside Wuhan
############################
prev_ratio <- prevalence_dat %>% filter(scenario == "Scenario 2") %>%
  group_by(date, is_wuhan) %>%
  summarize(total_prev = mean(prevalence_o)) %>%
  pivot_wider(values_from=total_prev,names_from=is_wuhan) %>%
  mutate(prev_ratio=`0`/`1`) %>% drop_na() %>%
  ungroup()

prev_ratio %>%
  summarize(1/mean(prev_ratio))
  
prev_ratio %>%  ggplot() + 
  geom_line(aes(x=date,y=1/prev_ratio)) +
  scale_y_log10() + 
  geom_hline(yintercept=1)

############################
## Highest and lowest peak prevalence
############################
prevalence_dat %>% 
  group_by(scenario, origin_city) %>%
  filter(prevalence_o == max(prevalence_o)) %>%
  arrange(scenario, -prevalence_o) %>%
  filter(scenario == "Scenario 10")


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

