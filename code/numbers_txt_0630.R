library(tidyverse)
library(lubridate)
library(dplyr)
library (rio)

# scenario key from James
scenario_key <- c("Scenario 1"="Scenario 3", 
                  "Scenario 2"="Scenario 1", 
                  "Scenario 3"="Scenario 4", 
                  "Scenario 4"="Scenario 5", 
                  "Scenario 5"="Scenario 6", 
                  "Scenario 6"="Scenario 7", 
                  "Scenario 7"="Scenario 8", 
                  "Scenario 8"="Scenario 9", 
                  "Scenario 9"="Scenario 10", 
                  "Scenario 10" = "Scenario 2*")

############################
## Load the master table
############################
main_scenario <- "Scenario 2"

mt <- read.csv("master_table_0630-FINAL_VERSION.csv")   #all_dat
mt %>% mutate( fvolume_od = ifelse( is.na(fvolume_od), 0 , fvolume_od ) ) ->mt
mt %>% glimpse()

############################
## alpha
############################
mt %>% group_by(scenario) %>% 
  summarise(alpha_m=mean(alpha) ) %>% print() %>% 
  #
  mutate(alpha_lower=min(alpha_m),
         alpha_upper=max(alpha_m)) %>% filter(scenario==main_scenario) 

###########################
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

### for individual countries

individual_predictions=mt %>% 
  filter(is_africa_d==1) %>% 
  mutate( imp_number=prevalence_o*fvolume_od*alpha ) %>% 
  # by scenario
  group_by(destination_country,scenario) %>% 
  summarise( sum=sum(imp_number) ) %>% ungroup() %>% 
  group_by(destination_country) %>% mutate( mean_pred= (sum),
                                            lower=range(sum)[1],
                                            upper=range(sum)[2]) %>% 
  arrange( desc(mean_pred) ) %>% 
  select(-sum) %>% print(n=Inf) # Egypt, SA, Kenya, Ethiopia

individual_predictions_mean=individual_predictions[,c("destination_country","scenario","mean_pred")]
individual_predictions_mean$scenario<-scenario_key[individual_predictions_mean$scenario]
individual_predictions_reshaped=spread(individual_predictions_mean,key=scenario,value=mean_pred)

write.csv(individual_predictions_reshaped,'tableS1.csv',row.names=F)

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
  )  

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

prop_majority_all=pf_probt %>% group_by(date) %>% 
  mutate( n=n() ) %>% 
  mutate( prob_W_lower=min(prop_wuhan),
          prob_W_upper=max(prop_wuhan)) %>% 
  filter(scenario==main_scenario) #%>%

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
prop_majority_africa=pf_probt %>% group_by(date) %>% 
  mutate( n=n() ) %>% 
  mutate( prob_W_lower=min(prop_wuhan),
          prob_W_upper=max(prop_wuhan)) %>% 
  filter(scenario==main_scenario) %>% print(n=Inf)

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
##

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
date_range=pf                 %>% filter( in_interval==1 ) %>% 
  summarise( int_start=min(date),
             int_end=max(date)) 

range(date_range$int_start)
# from manuscript

############################
## understimation
############################
# wuhan detected / all wuhan
1/2.8
# all china / all wuhan


# 1 wuhan detected / 2.8 wuhan real / 2.8*1.96 call of china real
1- 1/(2.8+2.8*0.6) # fraction missed
1- 1/(2.8+2.8*0.2) # fraction missed - lower
1- 1/(2.8+2.8*5.6) # fraction missed - upper

#1- 1/(0.55+0.55*0.555) # fraction missed
1- 1/(2.8+2.8*0.555) # fraction missed

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