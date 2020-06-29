## TITLE: estimating_province_prevalence
## Description: Estimating province-level prevalence
## Author: Tigist Menkir (Center for Communicable Disease Dynamics, Harvard T.H. Chan School of Public Health)
## Date: 17 June 2020
source("./code/simpler_method_fun.R")
library(stats)
library(tidyverse)
library(patchwork)
library(RColorBrewer)

create_scenario <- 9

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
  save_name <- "./out/city_prev_mod07.Rdata"
}
if(create_scenario == 8) {  
  asc_nonhubei_v_hubei <- 5 # Verity et al
  name_scenario <- "Scenario 8"
  save_name <- "./out/city_prev_mod08.Rdata"
}
#if(create_scenario == 9) {  
#  asc_nonhubei_v_hubei <- c(0.947381, 1.323449, 1.265765, 1.212640, 1.046407) # Tsang et al.
#  name_scenario <- "Scenario 9"
#  save_name <- "./out/city_prev_mod09.Rdata"
#}

if(create_scenario == 9) {  
  asc_rates <- read_csv("data/tsang_ascertainment_rates.csv")
  asc_nonhubei_v_hubei <- rep(1,5) # Tsang et al.
  name_scenario <- "Scenario 9"
  save_name <- "./out/city_prev_mod09.Rdata"
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
if(create_scenario != 9){
  all_incidence_province <- shift_2_delays(confirmed_cases_date,incubation_period=-5,delay=-7)
} else {
  ## If Tsang scaling, shift back to onsets, then inflate, then shift back to infections
  all_incidence_province <- confirmed_cases_date%>%
    arrange( province_raw,dates ) %>%  # helps to check the df visually
    group_by(province_raw)%>%
    mutate(n_onset=shift(n,n=-7)) %>% 
    ungroup() %>% 
    relocate( dates, province_raw,n_onset ) 
  
  scaled_inc <- all_incidence_province %>% left_join(asc_rates %>% rename(dates=date)) %>% 
    mutate(n_onset_scaled=ifelse(province_raw == "Hubei", 
                                 n_onset/asct_rest,
                                 n_onset/asct_wuhan))
                                 #n_onset + rnbinom(n(), n_onset,prob=asct_rest), 
                                 #n_onset + rnbinom(n(), n_onset,prob=asct_wuhan)))
  scaled_inc <- scaled_inc %>% select(-n_onset) %>% rename(n_onset=n_onset_scaled)
  all_incidence_province <- scaled_inc%>%
    arrange( province_raw,dates ) %>%  # helps to check the df visually
    group_by(province_raw)%>%
    mutate(n_infected=shift(n_onset,n=-5)) %>% 
    ungroup() %>% 
    relocate( dates, province_raw,n_infected) 
  
}

# plot the results
p <- plot_conf_onset(all_incidence_province,confirmed_cases_date)

# compute cumulative incidence and add population size
prov_cum_incidence <- comp_cum_incidence( all_incidence_province, "./data/provinces_popn_size_statista.csv" ) # 15 rows

# add calibration value
if(create_scenario != 9){
  calibration_value <- tibble(  is_hubei=c(0,1),
                                calv=c(asc_nonhubei_v_hubei,1) ,
                                calv_inverse=c(1, 1/asc_nonhubei_v_hubei))
} else {
  ## for scenario 9
  calibration_value_scen_9 <- tibble(  is_hubei=c(0,1),
                                calv1=c(asc_nonhubei_v_hubei[1],1) ,
                                calv2=c(asc_nonhubei_v_hubei[2],1) ,
                                calv3=c(asc_nonhubei_v_hubei[3],1) ,
                                calv4=c(asc_nonhubei_v_hubei[4],1) ,
                                calv5=c(asc_nonhubei_v_hubei[5],1) ,
                                calv_inverse1=c(1, 1/asc_nonhubei_v_hubei[1]),
                                calv_inverse2=c(1, 1/asc_nonhubei_v_hubei[2]),
                                calv_inverse3=c(1, 1/asc_nonhubei_v_hubei[3]),
                                calv_inverse4=c(1, 1/asc_nonhubei_v_hubei[4]),
                                calv_inverse5=c(1, 1/asc_nonhubei_v_hubei[5]))
  
  ## define ascertainment rate ratio periods for scenario 9
  asct_period_1<-seq(as.Date('2019-12-02'),as.Date('2020-01-17'),by="day")
  asct_period_2<-seq(as.Date('2020-01-18'),as.Date('2020-01-21'),by="day")
  asct_period_3<-seq(as.Date('2020-01-22'),as.Date('2020-01-26'),by="day")
  asct_period_4<-seq(as.Date('2020-01-27'),as.Date('2020-02-03'),by="day")
  asct_period_5<-seq(as.Date('2020-02-04'),as.Date('2020-03-02'),by="day")
}

# calibrate incidence in Hubei and outside

if (create_scenario!=9){
  prov_inc_calibrated <- all_incidence_province %>% mutate(is_hubei=as.numeric(province_raw=="Hubei") ) %>% 
    left_join( calibration_value, by="is_hubei" ) %>% 
    mutate( n_infected_cal=n_infected/calv_inverse ,
          n_onset_cal = n_onset/calv_inverse) %>% 
    select( dates,province_raw,n_infected_cal, n_onset_cal) %>%
    rename(n_onset=n_onset_cal)
  }
if (create_scenario==9){
  prov_inc_calibrated <- all_incidence_province %>% 
  mutate(is_hubei=as.numeric(province_raw=="Hubei") ) %>% 
  left_join( calibration_value_scen_9, by="is_hubei" ) %>% 
  mutate(n_infected_cal=ifelse(dates%in%asct_period_1,n_infected/calv_inverse1,
                               ifelse(dates%in%asct_period_2,n_infected/calv_inverse2,
                               ifelse(dates%in%asct_period_3,n_infected/calv_inverse3,
                               ifelse(dates%in%asct_period_4,n_infected/calv_inverse4,
                                      n_infected/calv_inverse5))))) %>% 
  select( dates,province_raw,n_infected_cal,n_onset )
  }

# plot calibrated incidence & symptom onset curves

# prov_inc_calibrated with symptom onset curve data
prov_inc_calibrated_2 <- all_incidence_province %>% mutate(is_hubei=as.numeric(province_raw=="Hubei") ) %>% 
  left_join( calibration_value, by="is_hubei" ) %>% 
  mutate( n_infected_cal=n_infected/calv_inverse ) %>% 
  select( dates,province_raw,n_infected_cal,n_onset)

## for three randomly selected provinces
set.seed(96)
random_selection_provinces=sample_n(prov_inc_calibrated,3)$province_raw
all_incidence_province_random_subset<-all_incidence_province%>%
  filter(province_raw%in%random_selection_provinces)
confirmed_cases_date_subset<-confirmed_cases_date%>%
  filter(province_raw%in%random_selection_provinces)

p <- plot_conf_onset_2(all_incidence_province_random_subset %>% rename(n_infected_cal=n_infected),confirmed_cases_date_subset)
p
ggsave("./figures/incidence_subset.pdf",width=15,height=5)



## for all provinces
p2 <- plot_conf_onset_2(prov_inc_calibrated,confirmed_cases_date)
p2
ggsave("./figures/incidence_all.pdf",width=15,height=5)

# distribute the cases into cities and add denominator
prov_city_adjust <- get_prov_city_adjust(file="./out/frac_popn_city.Rdata" )
city_n_inf_caladj <- adjust_prov_prev_by_city( prov_inc_calibrated , prov_city_adjust, aportion_all = create_scenario!=8)
load(file="./out/df_city_pop.Rdata")

## If scenario 8 (aportion cases proportional to city's fractional share of province population), then per-capita incidence should
## be the same within each province. Otherwise, is different.
city_n_inf_caladj_den <- city_n_inf_caladj %>% left_join(df_city_pop,by=c("city"="asciiname")) %>% 
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
prov_inc_prev_cali %>% ggplot() + 
  geom_line(aes(x=date,y=travel_prev)) + 
  geom_line(data=city_n_inf_caladj_den, aes(x=date,y=n_infected_caladj),col="red") + 
  facet_wrap(~city,scales="free_y")


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


# plot
city_prev_mod0 %>% ggplot(aes(x=date,y=prevalence_o)) +
  geom_line(  ) +
  facet_wrap(~origin_city,scales="free")

## Estimating ascertainment rates from M&B

# estimating the period-specific ascertainment rate
mnb<-read.csv("./data/m&b_digitized_hubei_other.csv")

mnb_asct<-mnb%>%
  group_by(Date,Region)%>%
  mutate(asct_rates=1-(ConfirmedCases[1]/(ConfirmedCases[1]+ConfirmedCases[2]))) 

mnb_asct_b4=mnb_asct[c(1,2,5,6),]
mnb_asct_a4=mnb_asct[c(3,4,7,8),]

mnb_asct_b4_ratio=mnb_asct$asct_rates[5]/mnb_asct$asct_rates[1]
mnb_asct_a4_ratio=mnb_asct$asct_rates[7]/mnb_asct$asct_rates[3]

## estimating the ascertainment rate over the entire period

hubei_asct=weighted.mean(c(mnb_asct$asct_rates[1],mnb_asct$asct_rates[3]),
              c(34/41,7/41))
not_hubei_asct=weighted.mean(c(mnb_asct$asct_rates[5],mnb_asct$asct_rates[7]),
              c(34/41,7/41))

hubei_not_hubei_asct_ratio=not_hubei_asct/hubei_asct

# ratio of days
city_prev_mod0 %>% select(date) %>% distinct() %>% summarise( mean(date<ymd("2020-02-22")) )
# catio of detected cases (in the area of China we are interested in)
city_prev_mod0 %>% select(date) %>% distinct() %>% pull(date) -> date_v
confirmed_cases_date %>% filter(dates%in%date_v) %>% 
  mutate( before=(dates<ymd("2020-02-22"))  ) %>% 
  count( before, wt=n,name = "tot_cases"  ) %>% 
  mutate(sum_tot_cases=sum(tot_cases)) %>% 
  mutate(f_case=tot_cases/sum_tot_cases)


## Estimating ascertainment rates from Verity et al. 
verity <- read.csv("./data/digitize_verity.csv")
asc_ratio_verity <- crossprod(verity$outside_wuhan,verity$outside_pop_prop)/crossprod(verity$wuhan,verity$wuhan_pop_prop)






# # using healthcare worker seroprev, Wuhan
# seroprev_h_w=0.038
# 
# # using healthcare worker seroprev, outside of Wuhan (Guangzhou)
# seroprev_h_nw=0.012
# 
# # Note: HAVE TO INCLUDE SEROPREV ESTIMATES FOR CITIES REPRESENTING THE 13 REMAINING PROVINCES
# 
# # read in cities' popn data 
# pop_size<-read.csv("./data/popn_estimates_cities_china.csv")
# 
# # subset to only infection onset rows
# columns2<-c("date","dates","province_raw","n_infected")
# all_incidence_province_subset2<-all_incidence_province[which(
#   colnames(all_incidence_province)%in%columns2)]
# all_incidence_province_subset_long2=melt(all_incidence_province_subset2,
#                                          id.vars=c("date","dates","province_raw"), 
#                                          variable.name="number_individuals")
# 
# # combine infection onset & seroprevalence data
# all_incidence_province_subset_long2$seroprev=ifelse(all_incidence_province_subset_long2$province_raw=="Hubei",
#                                                     seroprev_h_w,
#                                                     seroprev_h_nw)
# 
# 
# # seroprev calibration: scale incidence data using estimated ascertainment rate
# ## first calculate the area under the infection incidence curve 
# ## then standardize the AUC by population size to compare to seroprev measures
# ## estimate ascertainment rates by dividing the standardize incidence measures from above by seroprevalence
# ## scale infection incidence by these estimated ascertainment rates to yield true inf curves
# all_incidence_calibrated<-all_incidence_province_subset_long2%>%
#   mutate(seroprev_scaled=ifelse(province_raw=="Hubei",seroprev*pop_size$population[pop_size$asciiname=='Wuhan'],
#                                 seroprev*pop_size$population[pop_size$asciiname=='Guangzhou']))%>%
#   group_by(province_raw)%>%
#   mutate(auc=sintegral(x=date,fx=value)$int)%>%
#   mutate(asc_rate=auc/seroprev_scaled)%>%
#   mutate(inc_new=value/asc_rate)



###PREVIOUS STUFF (incorporating a priori ascertainment rates,using region-specific ascertainment rates from Maier & Brockmann) 

# pre-processing digitized ascertainment rate input data
#ascertainment_digitized=read.csv("ascertainment_rates_meier_digitized.csv")

# estimate ascertainment rate by dividing confirmed by # unidentified infected + confirmed
# round inflection point to facilitate identifying the date at which this inflection occurs
#ascertainment_rates_all<-ascertainment_digitized%>%
#group_by(Province,TimePeriod)%>%
#mutate(asc_rate=NumberCases[2]/sum(NumberCases[1]+NumberCases[2]))%>%
#mutate(rounded_inflection_point=round(InflectionPoint))

#ascertainment_rates_all$Province=ifelse(ascertainment_rates_all$Province=="Shanxi","Shaanxi",as.vector(ascertainment_rates_all$Province))

# collect all inflection points by province
#inflection_points<-ascertainment_rates_all%>%
# group_by(Province)%>%
#summarise(inflection_point_all=unique(rounded_inflection_point))

#provinces_rep<-c('Beijing','Beijing','Shanghai','Shanghai','Guangdong','Guangdong','Henan','Henan',
# 'Tianjin','Tianjin','Zhejiang','Zhejiang','Hunan','Hunan','Shaanxi','Shaanxi',
#'Jiangsu','Jiangsu','Chongqing','Chongqing','Jiangxi','Jiangxi','Sichuan','Sichuan',
# 'Anhui','Anhui','Fujian','Fujian')

# combine province names & ascertainment rates
#ascertainment_rates_all_combined=cbind.data.frame(provinces_rep,
#  unique(ascertainment_rates_all$asc_rate))
#colnames(ascertainment_rates_all_combined)<-c("Province","asc_rate")
# combine province names & ascertainment rates with inflection points for each province
#ascertainment_rates_all_final=merge(ascertainment_rates_all_combined,inflection_points,
# by="Province")
#colnames(ascertainment_rates_all_final)<-c("province","asc_rate","inflection_point")

# find the corresponding date for each inflection point (note that 1 indicates 1 day since Jan 21st etc., following the convention used in Maier and Brockmann)
#inflection_point_seq<-seq(1,9)
#inflection_date_seq<-seq(as.Date('2020-01-22'),as.Date('2020-01-30'),by="day")
#inflection_dates=cbind.data.frame(inflection_point_seq,inflection_date_seq)
#colnames(inflection_dates)[1]<-"inflection_point"

# combine ascertainment rate data with above data on the dates of inflection points
#ascertainment_rates_all_FINAL=merge(ascertainment_rates_all_final,
#   inflection_dates,by="inflection_point")
#colnames(ascertainment_rates_all_FINAL)[2]<-"province_raw"

# estimate symptom onset and infection incidence by province
#all_incidence_province_2<-confirmed_cases_date_2%>%
# group_by(province_raw)%>%
# mutate(n_smoothed=predict(gam(n~s(dates,spar=0.7),
# family=poisson(link="log")),type="response",
#  newdata=dates))%>%
#mutate(n_onset=ifelse(dates%in%delay_date_seq_1,shift(n_smoothed,n=delay_1),
#  shift(n_smoothed,n=delay_2)))%>%
# mutate(n_onset_inflated=ifelse(dates%in%seq(as.Date('2019-11-01'),
#   ascertainment_rates_all_FINAL[as.vector(ascertainment_rates_all_FINAL$province_raw)==province_raw,"inflection_date_seq"][1],by="day"),
#  n_onset/ascertainment_rates_all_FINAL[as.vector(ascertainment_rates_all_FINAL$province_raw)==province_raw, 
#         "asc_rate"][1],
#   n_onset/ascertainment_rates_all_FINAL[as.vector(ascertainment_rates_all_FINAL$province_raw)==province_raw, 
#"asc_rate"][2]))%>%
  # mutate(n_infected=shift(n_onset_inflated,n=incubation_period))
  
  # subset to 2020
  #all_incidence_province_2020_2<-all_incidence_province_2%>%
  #subset(dates%in%dates_2)
  
  # --subset to only relevant columns -- #
  #all_incidence_province_2020_2_subset<-all_incidence_province_2020_2[which(
  #colnames(all_incidence_province_2020_2)%in%columns)]
  
  #write.csv(all_incidence_province_2020_2_subset,"all_incidence_2020_M&B.csv")
# lineplot of confirmed cases, symptom onset and infection incidence in 2020
#all_incidence_province_2020_2_subset_long=melt(all_incidence_province_2020_2_subset,
# id.vars=c("dates","province_raw"), variable.name="number_individuals")

#lineplot2<-ggplot(all_incidence_province_2020_2_subset_long,
#     aes(x=dates,y=value,col=number_individuals))+
#  geom_line()+scale_x_date(breaks="2 weeks")+
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#  scale_color_discrete(labels = c("confirmed_cases","symptom_onset_incidence","infection_incidence"))+
#  labs(colour="Legend")+ylab("Number of individuals")

#lineplot2+facet_wrap(.~province_raw,scales="free")
#ggsave("simple_version_3.pdf",width=20,height=20)


############ PREVIOUS STUFF: Part II: using all excluding Hubei ascertainment rates from Maier & Brockmann 
#ascertainment_date_seq_1b<-seq(as.Date('2019-11-01'),as.Date('2020-01-26'),by="day")

#ascertainment_rate_exc_hubei_1=10^2/(10^2.5+10^2)
#ascertainment_rate_exc_hubei_2=10^4/(10^4+10^2)

# estimate symptom onset and infection incidence by province
#all_incidence_province_3<-confirmed_cases_date%>%
# group_by(province_raw)%>%
# mutate(n_smoothed=predict(gam(n~s(dates,spar=0.7),
#   family=poisson(link="log")),type="response",
#  newdata=dates))%>%
# mutate(n_onset=ifelse(dates%in%delay_date_seq_1,shift(n_smoothed,n=delay_1),
#  shift(n_smoothed,n=delay_2)))%>% 
# mutate(n_onset_inflated=ifelse(province_raw=="Hubei",n_onset,
#   ifelse(dates%in%ascertainment_date_seq_1b,
#   n_onset/ascertainment_rate_exc_hubei_1,
#    n_onset/ascertainment_rate_exc_hubei_2)))%>%
# mutate(n_infected=shift(n_onset_inflated,n=incubation_period))

# subset to 2020
#all_incidence_province_2020_3<-all_incidence_province_3%>%
# subset(dates%in%dates_2)

# --subset to only relevant columns -- #
#all_incidence_province_2020_3_subset<-all_incidence_province_2020_3[which(
# colnames(all_incidence_province_2020_3)%in%columns)]

# lineplot of confirmed cases, symptom onset and infection incidence in 2020
#all_incidence_province_2020_3_subset_long=melt(all_incidence_province_2020_3_subset,
#    id.vars=c("dates","province_raw"), variable.name="number_individuals")

#lineplot3<-ggplot(all_incidence_province_2020_3_subset_long,
#     aes(x=dates,y=value,col=number_individuals))+
# geom_line()+scale_x_date(breaks="2 weeks")+
# theme(axis.text.x = element_text(angle = 45, hjust = 1))+
# scale_color_discrete(labels = c("confirmed_cases","symptom_onset_incidence","infection_incidence"))+
# labs(colour="Legend")+ylab("Number of individuals")

#lineplot3+facet_wrap(.~province_raw,scales="free")
#ggsave("simple_version_2.pdf",width=20,height=20)

