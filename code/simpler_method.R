## TITLE: estimating_province_prevalence
## Description: Estimating province-level prevalence
## Author: Tigist Menkir (Center for Communicable Disease Dynamics, Harvard T.H. Chan School of Public Health)
## Date: 17 June 2020
source("./code/simpler_method_fun.R")

asc_nonhubei_v_hubei <- 1 # relative ascertainment rate non-hubei versus hubei (for example 5 for a 1:5 ratio of Hubei versus non-Hubei ascertainment rate)
name_scenario <- "Scenario 6"
save_name <- "./out/city_prev_mod06.Rdata"
#
asc_nonhubei_v_hubei <- 5
name_scenario <- "Scenario 7"
save_name <- "./out/city_prev_mod07.Rdata"
#
asc_nonhubei_v_hubei <- 10
name_scenario <- "Scenario 8"
save_name <- "./out/city_prev_mod08.Rdata"


############################
## read in confirmed case data from James' covback repository
############################
confirmed_cases<-read.csv("./data/midas_data_final.csv") %>% as_tibble()
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

# plot the results
p <- plot_conf_onset(all_incidence_province,confirmed_cases_date)

# compute cumulative incidence and add population size
prov_cum_incidence <- comp_cum_incidence( all_incidence_province, "./data/provinces_popn_size_statista.csv" ) # 15 rows

# add calibration value
calibration_value <- tibble(  is_hubei=c(0,1),
                              calv=c(asc_nonhubei_v_hubei,1) )
  
# calibrate incidence in Hubei and outside
prov_inc_calibrated <- all_incidence_province %>% mutate(is_hubei=as.numeric(province_raw=="Hubei") ) %>% 
  left_join( calibration_value, by="is_hubei" ) %>% 
  mutate( n_infected_cal=n_infected/calv ) %>% 
  select( dates,province_raw,n_infected_cal )

# distribute the cases into cities and add denominator
prov_city_adjust <- get_prov_city_adjust(file="./out/frac_popn_city.Rdata" )
city_n_inf_caladj <- adjust_prov_prev_by_city( prov_inc_calibrated , prov_city_adjust)
load(file="./out/df_city_pop.Rdata")
city_n_inf_caladj_den <- city_n_inf_caladj %>% left_join(df_city_pop,by=c("city"="asciiname")) %>% 
  mutate(n_infected_caladj=n_infected_caladj/population) %>% select(-population)

# compute travel relevant prevalence
prov_inc_prev_cali <- comp_travel_rel_prev(city_n_inf_caladj_den)

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

