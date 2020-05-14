## TITLE: estimating_city_prevalence
## Description: Estimating city-level prevalence from province-level estimates
## Author: Tigist Menkir (Center for Communicable Disease Dynamics, Harvard T.H. Chan School of Public Health)
## Date: 12 May 2020

#setwd("~/Desktop/nCoV exports/prevalence data")

library(reshape2)
library(tidyverse)

### EXTRACTING PROVINCE-LEVEL PREVALENCE

prev_data_all_median<-read.csv("./data/main_all_prevalence.csv")
prev_data_hubei_median<-read.csv("./data/main_hubei_prevalence.csv")
prev_data_median<-rbind(prev_data_all_median,prev_data_hubei_median[,1:length(prev_data_all_median)])
prev_data_median$Scenario<-rep("Intermediate",nrow(prev_data_median))

prev_data_all_lower<-read.csv("./data/diffuse_r_local_all_prevalence.csv")
prev_data_hubei_lower<-read.csv("./data/diffuse_r_local_hubei_prevalence.csv")
prev_data_lower<-rbind(prev_data_all_lower,prev_data_hubei_lower[,1:length(prev_data_all_lower)])
prev_data_lower$Scenario<-rep("Lower",nrow(prev_data_lower))

prev_data_all_upper<-read.csv("./data/t_switch_6_all_prevalence.csv")
prev_data_hubei_upper<-read.csv("./data/t_switch_6_hubei_prevalence.csv")
prev_data_upper<-rbind(prev_data_all_upper,prev_data_hubei_upper[,1:length(prev_data_all_upper)])
prev_data_upper$Scenario<-rep("Upper",nrow(prev_data_upper))

prev_all<-rbind(prev_data_lower,prev_data_median,prev_data_upper)
prevalence<-prev_all[prev_all$var=='Prevalence of all infected individuals',]
prevalence$asct_rate<-ifelse(prevalence$Scenario=='Lower',0.145,
                             ifelse(prevalence$Scenario=='Intermediate',0.0973,0.0922))

# read in popn size data
popn_size_cities_all<-read.csv("./data/popn_estimates_cities_china.csv")

popn_size_select_cities<-popn_size_cities_all%>%
  subset(asciiname%in%c("Wuhan", "Beijing", "Shanghai", "Guangzhou", "Zhengzhou", "Tianjin",
                        "Hangzhou", "Jiaxing", "Changsha", "Xi'an", "Nanjing", "Shenzhen", 
                        "Chongqing", "Nanchang", 
                        "Chengdu", "Hefei", "Fuzhou", "Dongguan"))

order<-c("Wuhan", "Beijing", "Shanghai", "Guangzhou", "Zhengzhou", "Tianjin",
         "Hangzhou", "Jiaxing", "Changsha", "Xi'an", "Nanjing", "Shenzhen", 
         "Chongqing", "Nanchang", 
         "Chengdu", "Hefei", "Fuzhou", "Dongguan")

popn_size_select_cities<-popn_size_select_cities[match(order,popn_size_select_cities$asciiname),]
popn_size_select_cities$province<-c('Hubei','Beijing','Shanghai','Guangdong','Henan',
                                    'Tianjin','Zhejiang','Zhejiang','Hunan','Shaanxi','Jiangsu','Guangdong','Chongqing',
                                    'Jiangxi','Sichuan','Anhui','Fujian','Guangdong')

popn_size_provinces<-read.csv("./data/provinces_popn_size_statista.csv")
# merge province & population data at the city level 
merged_popn_data<-merge(popn_size_select_cities,popn_size_provinces,by="province")

# calculate the fraction of total population size per city
frac_popn_city<-merged_popn_data%>%
  group_by(province)%>%
  mutate(fraction=population/popn_size_province)

frac_popn_city$fraction<-ifelse(frac_popn_city$asciiname=="Wuhan",1,frac_popn_city$fraction)

provinces_order<-c('Hubei','Beijing','Shanghai','Guangdong','Henan',
                   'Tianjin','Zhejiang','Zhejiang','Hunan','Shaanxi','Jiangsu','Guangdong','Chongqing',
                   'Jiangxi','Sichuan','Anhui','Fujian','Guangdong')

### Loop through scenarios
Scenarios<-c("Lower","Intermediate","Upper")

prev_all_scenarios_combined <- c()
for (scenario in Scenarios) {
  prev_scenario_i<-prevalence[prevalence$Scenario==scenario,]
  asct_rate_i<-unique(prev_scenario_i$asct_rate)
  if(scenario=="Lower"){
    prevalence_all_provinces_final_i<-dcast(prev_scenario_i, province~date,value.var="lower")
  }
  else if (scenario=="Intermediate"){
    prevalence_all_provinces_final_i<-dcast(prev_scenario_i, province~date,value.var="median")
  }
  else {
    prevalence_all_provinces_final_i<-dcast(prev_scenario_i, province~date,value.var="upper")
  }
  prevalence_all_provinces_final_i<-prevalence_all_provinces_final_i%>%
    subset(province%in%provinces_order)
  prevalence_all_provinces_final_i<-
    prevalence_all_provinces_final_i[match(provinces_order,
                                           prevalence_all_provinces_final_i$province),]

  ### ESTIMATING CITY-LEVEL PREVALENCE
  
  # scale province-level prevalence estimates to city-level prevalence estimates using method #1
  # scaling fraction = 1 for all cities which are the only ones represented in their province
  # and as a fraction of # of cities for cities which share a province

  provinces_prev_popn_size_i<-merge(frac_popn_city,
                                         prevalence_all_provinces_final_i,by="province")

  fraction_2<-ifelse(provinces_prev_popn_size_i$province=='Guangdong',
                   1/3,ifelse(provinces_prev_popn_size_i$province=='Zhejiang',1/2,1))

  # construct prevalence by cities data frame using ascertainment rate
  prev_cities_i<-data.frame(matrix(0,nrow=nrow(provinces_prev_popn_size_i),ncol=124))
  for (i in 1:nrow(prev_cities_i)) {
    if(provinces_prev_popn_size_i$asciiname[i]=='Wuhan'){
      scaled_prev<-as.numeric(provinces_prev_popn_size_i[i,7:length(provinces_prev_popn_size_i)])/(provinces_prev_popn_size_i$population[i])
      prev_cities_i[i,]<-scaled_prev
    }
    else{
      scaled_prev_2<-(as.numeric(provinces_prev_popn_size_i[i,7:length(provinces_prev_popn_size_i)])*
                      fraction_2[i])/(provinces_prev_popn_size_i$population[i]*asct_rate_i)
      prev_cities_i[i,]<-scaled_prev_2
    }
  }
  prev_cities_i<-cbind(provinces_prev_popn_size_i$asciiname,prev_cities_i)
  prev_all_scenarios_combined <-rbind(prev_all_scenarios_combined,prev_cities_i)
}
colnames(prev_all_scenarios_combined)<-c("cities",paste0("day",1:124))
prev_all_scenarios_combined$Scenario<-c(rep("Lower",26),rep("Intermediate",26),
                                              rep("Upper",26))
write.csv(prev_all_scenarios_combined,"./out/prev_all_scenarios_combined.csv")
