## TITLE: estimating_province_prevalence
## Description: Estimating province-level prevalence
## Author: Tigist Menkir (Center for Communicable Disease Dynamics, Harvard T.H. Chan School of Public Health)
## Date: 28 May 2020

library(tidyverse)
library(gam) # library(mgcv) <- used mgcv
library(data.table)

# read in confirmed case data from James' covback repository
confirmed_cases<-read.csv("./data/midas_data_final.csv") # TODO: all data should come from data folder

# subset to only provinces used in our analysis
provinces<-c('Hubei','Beijing','Shanghai','Guangdong','Henan',
  'Tianjin','Zhejiang','Hunan','Shaanxi','Jiangsu','Chongqing',
  'Jiangxi','Sichuan','Anhui','Fujian')
confirmed_cases_final<-confirmed_cases[confirmed_cases$province_raw%in%provinces,]

# match date indices to actual dates
dates=seq(as.Date('2019-11-01'),as.Date('2020-03-02'),by="day")
date=seq(0,122)
dates_and_date=cbind.data.frame(dates,date)
confirmed_cases_date=merge(dates_and_date,confirmed_cases_final,by="date")

# define delay and incubation period params
delay=-7
incubation_period=-5

# estimate symptom onset and infection incidence by province
all_incidence_province<-confirmed_cases_date%>%
  group_by(province_raw)%>%
  mutate(n_smoothed=predict(gam(n~s(dates,spar=0.7),
                                family=poisson(link="log")),type="response",
                            newdata=dates))%>%
  mutate(n_onset=shift(n_smoothed,n=delay))%>% 
  mutate(n_infected=shift(n_onset,n=incubation_period))

# subset to 2020
dates_2=seq(as.Date('2020-01-01'),as.Date('2020-03-02'),by="day")
all_incidence_province_2020<-all_incidence_province%>%
  subset(dates%in%dates_2)

###Figures
# --subset to only relevant columns -- #
columns<-c("dates","province_raw","n_smoothed","n_onset","n_infected")
all_incidence_province_2020_subset<-all_incidence_province_2020[which(
  colnames(all_incidence_province_2020)%in%columns)]

# lineplot of confirmed cases, symptom onset and infection incidence in 2020
all_incidence_province_2020_subset_long=melt(all_incidence_province_2020_subset,
                                             id.vars=c("dates","province_raw"), variable.name="number_individuals")

lineplot<-ggplot(all_incidence_province_2020_subset_long,
                 aes(x=dates,y=value))+
  geom_point(data=confirmed_cases_date,aes(x=dates,y=n),size=0.25) +
  geom_line(aes(col=number_individuals))+scale_x_date(breaks="2 weeks")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_color_discrete(labels = c("confirmed_cases","symptom_onset_incidence","infection_incidence"))+
  labs(colour="Legend")+ylab("Number of individuals")

lineplot+facet_wrap(.~province_raw,scales="free")
ggsave("simple_version_1.pdf",width=20,height=20)

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
                                                                       "asc_rate"][2]))%>%
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

