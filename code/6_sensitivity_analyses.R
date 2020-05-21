## TITLE: Sensitivity Analyses 
## Description: Sensitivity analyses for estimating exported cases under two additional scenarios of prevalence (Scenarios 4 and 5) 
## Author: Tigist Menkir (Center for Communicable Disease Dynamics, Harvard T.H. Chan School of Public Health)
## Date: 12 May 2020

library(reshape2)
library(dplyr)

#setwd("~/Desktop/nCoV exports/prevalence data")


## Set number of bootstrap samples here, can set lower for debugging/testing
n_bootstraps <- 50000


prev_all<-read.csv("./data/main_all_prevalence.csv")
prev_hubei<-read.csv("./data/main_hubei_prevalence.csv")
prev_data<-rbind(prev_all,prev_hubei[,1:length(prev_all)])

prevalence_median_2<-prev_data[prev_data$var=='Prevalence of all infected individuals',]

prevalence_median_2$date<-as.Date(prevalence_median_2$date)
prevalence_median_2_all_provinces_final_median<-dcast(prevalence_median_2, province~date,value.var="median")
provinces_order<-c('Hubei','Beijing','Shanghai','Guangdong','Henan',
                   'Tianjin','Zhejiang','Zhejiang','Hunan','Shaanxi','Jiangsu','Guangdong','Chongqing',
                   'Jiangxi','Sichuan','Anhui','Fujian','Guangdong')
prevalence_median_all_provinces_final_median_2<-prevalence_median_2_all_provinces_final_median%>%
  subset(province%in%provinces_order)

prevalence_median_all_provinces_final_median_2<-
  prevalence_median_all_provinces_final_median_2[match(provinces_order, prevalence_median_all_provinces_final_median_2$province),]

### ESTIMATING CITY-LEVEL PREVALENCE

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

# scale province-level prevalence estimates to city-level prevalence estimates using method #1
# scaling fraction = 1 for all cities which are the only ones represented in their province
# and as a fraction of # of cities for cities which share a province

provinces_prev_popn_size_median<-merge(frac_popn_city,prevalence_median_all_provinces_final_median_2,by="province")

fraction_2<-ifelse(provinces_prev_popn_size_median$province=='Guangdong',
                   1/3,ifelse(provinces_prev_popn_size_median$province=='Zhejiang',1/2,1))

## Scenario 4 - non-conservative prev scenario, where province-level prevalence is attributed
## to cities proportional to their share of total population
prev_cities_scen_4<-data.frame(matrix(0,nrow=nrow(provinces_prev_popn_size_median),ncol=124))
for (i in 1:nrow(prev_cities_scen_4)) {
  if(provinces_prev_popn_size_median$asciiname[i]=='Wuhan'){
    # divide province-level prev by fraction of city's population in overall province's population
    scaled_prev<-(as.numeric(provinces_prev_popn_size_median[i,7:length(provinces_prev_popn_size_median)]))/
      (provinces_prev_popn_size_median$population[i]*0.65)
    scaled_prev_1b<-scaled_prev
    prev_cities_scen_4[i,]<-scaled_prev_1b
  }
  else{
    scaled_prev_2<-(as.numeric(provinces_prev_popn_size_median[i,7:length(provinces_prev_popn_size_median)])*
                      fraction_2[i])/(provinces_prev_popn_size_median$population[i]*0.0973)
    prev_cities_scen_4[i,]<-scaled_prev_2/provinces_prev_popn_size_median$population[i]
  }
}
prev_cities_scen_4<-cbind(provinces_prev_popn_size_median$asciiname,prev_cities_scen_4)
colnames(prev_cities_scen_4)<-c("cities",paste0("day",1:124))
prev_cities_scen_4$Scenario<-rep("Scenario 4",nrow(prev_cities_scen_4))

## Scenario 5 - prev scenario assuming 100% ascertainment rate
prev_cities_scen_5<-data.frame(matrix(0,nrow=nrow(provinces_prev_popn_size_median),ncol=124))
for (i in 1:nrow(prev_cities_scen_5)) {
  scaled_prev<-(as.numeric(provinces_prev_popn_size_median[i,7:length(provinces_prev_popn_size_median)])*
                  fraction_2[i])/(provinces_prev_popn_size_median$population[i])
  prev_cities_scen_5[i,]<-scaled_prev
}

prev_cities_scen_5<-cbind(provinces_prev_popn_size_median$asciiname,prev_cities_scen_5)
colnames(prev_cities_scen_5)<-c("cities",paste0("day",1:124))
prev_cities_scen_5$Scenario<-rep("Scenario 5",nrow(prev_cities_scen_5))

# Combine scenario 4 and scenario 5 prev data
prev_cities_scen_4_5<-rbind(prev_cities_scen_4,prev_cities_scen_5)

### ESTIMATING EXPORTED CASES
dates_seq<-seq(as.Date('2019-12-08'),as.Date('2020-02-29'),by="day")
dates_seq_wuhan<-seq(as.Date('2019-12-08'),as.Date('2020-01-23'),by="day")
dest_countries_validation_list<-c('United States','Australia','Canada','Korea (South)',
                                  'United Kingdom',
                                  'Netherlands','Sweden',
                                  'Germany','Spain','Singapore')

## read in flight data, if not already loaded
#flights<-read.csv("flights_adjusted_200506.csv")
#colnames(flights)[2]<-"iata"

flights_wuhan<-flights%>%
  subset(iata=="WUH"&destCtryName%in%dest_countries_validation_list)

flights_wuhan$flight_date<-as.Date(flights_wuhan$flight_date,format='%Y-%m-%d')
final_flights_wuhan<-flights_wuhan[flights_wuhan$flight_date%in%dates_seq_wuhan,]
flights_date_wuhan<-cbind.data.frame(final_flights_wuhan$flight_date,final_flights_wuhan$destCtryName, final_flights_wuhan$dailyvol)
colnames(flights_date_wuhan)<-c("date","destination_country","daily_volume")

## define bootstrapping function
## Reference: https://stackoverflow.com/questions/17922637/prediction-intervals-for-poisson-regression-on-r
boot_pi <- function(model, pdata, n, p) {
  odata <- model$data
  lp <- (1 - p) / 2
  up <- 1 - lp
  set.seed(2020)
  seeds <- round(runif(n, 1, 1000), 3)
  boot_y <- foreach(i = 1:n, .combine = rbind) %dopar% {
    set.seed(seeds[i])
    bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
    bpred <- predict(update(model, data = bdata), type = "response", newdata = pdata)
    rpois(length(bpred), lambda = bpred)
  }
  boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up)))
  return(data.frame(pred = predict(model, newdata = pdata, type = "response"), lower = boot_ci[, 1], upper = boot_ci[, 2]))
}

## read in prevalence data (looping through scenario 4 and 5)
Scenarios<-c("Scenario 4","Scenario 5")
num_cases_total_list<-c()
scaling_factor_list_sensitivity<-c()
for (scenario in Scenarios){
  prev_cities_i<-prev_cities_scen_4_5[prev_cities_scen_4_5$Scenario==scenario,]
  prevalence_wuhan<-prev_cities_i[prev_cities_i$cities=='Wuhan',colnames(prev_cities_i)!="Scenario"]
  prevalence_wuhan_final<-as.numeric(t(prevalence_wuhan))
  prevalence_wuhan_final<-prevalence_wuhan_final[is.na(prevalence_wuhan_final)==FALSE]
  prevalence_wuhan_final_2<-cbind.data.frame(dates_prev,prevalence_wuhan_final)
  colnames(prevalence_wuhan_final_2)<-c("date","prevalence")
  prevalence_wuhan_FINAL<-prevalence_wuhan_final_2[prevalence_wuhan_final_2$date%in%dates_seq_wuhan,]
  
  ## combine prevalence and flight data
  prev_flight_wuhan<-merge(prevalence_wuhan_FINAL,flights_date_wuhan,by="date")
  
  ## calculate # of exported cases in validation countries
  risk_wuhan<-prev_flight_wuhan%>%
    mutate(prev_vol_product=(prevalence*daily_volume))%>%
    group_by(destination_country)%>%
    summarise(risk_importation=sum(prev_vol_product))
  
  ## order the data frame of # of exported case estimates across validation set destinations 
  ## to match the data frame of reported cases in these destinations
  risk_wuhan<-data.frame(risk_wuhan)
  risk_wuhan_final<-risk_wuhan[match(dest_countries_validation_list,risk_wuhan$destination_country),]
  colnames(risk_wuhan_final)[1]<-"Country"
  
  confirmed_cases_FINAL$Country<-risk_wuhan_final$Country
  
  ## combine confirmed case & predicted # of exported casex data
  risk_wuhan_confirmed_cases<-merge(confirmed_cases_FINAL,risk_wuhan_final,by="Country")
  
  ## scale confirmed case data by the Niehus et al. scaling factor of 2.5 --> 1/0.4
  risk_wuhan_confirmed_cases$confirmed_cases_scaled<-ifelse(risk_wuhan_confirmed_cases$Country!='Singapore',
                                                            round(risk_wuhan_confirmed_cases$Cases_lm*2.5),
                                                            risk_wuhan_confirmed_cases$Cases_lm)
  
  ## run regression of scaled confirmed cases against # of exported cases
  risk_wuhan_confirmed_cases<-risk_wuhan_confirmed_cases%>%
    dplyr::select(`Country`,`risk_importation`,"confirmed_cases_scaled")
  
  reg_risk_wuhan<-glm(confirmed_cases_scaled~offset(log(risk_importation)),
                      data=risk_wuhan_confirmed_cases,
                      family=poisson(link="log"))
  
  ## define scaling factor as the coefficient for risk_wuhan var
  scaling_factor<-exp(coefficients(reg_risk_wuhan)[[1]])
  scaling_factor_list_sensitivity<-c(scaling_factor_list_sensitivity,scaling_factor)
  
  ############## Estimating risk in select African countries using subset of origin cities + Wuhan in China
  
  prevalence_all_cities_1<-prev_cities_i[,-c(1,126)]
  colnames(prevalence_all_cities_1)<-dates_prev
  prevalence_all_cities_final<-cbind(prev_cities_i$cities,prevalence_all_cities_1)
  prevalence_all_cities_final<-prevalence_all_cities_final[duplicated(prevalence_all_cities_final$`prev_cities_i$cities`)==FALSE,]
  colnames(prevalence_all_cities_final)[1]<-"cities"
  prevalence_all_cities_final_2<-prevalence_all_cities_final[,c(1,39:122)]
  
  prevalence_all_cities_FINAL<-melt(prevalence_all_cities_final_2,
                                    id.vars="cities",variable.name="date")
  
  prevalence_all_cities_FINAL$cities<-str_replace_all(prevalence_all_cities_FINAL$cities,
                                                      "Xi'an",'Xi An')
  
  ## combine prevalence and flight data
  colnames(flights_date_all_cities)[1]<-"date"
  colnames(prevalence_all_cities_FINAL)[1]<-"origin_city"
  flights_date_all_cities$date<-as.factor(flights_date_all_cities$date)
  prev_flight_all_cities_FINAL<-merge(prevalence_all_cities_FINAL,
                                      flights_date_all_cities,by=c("origin_city","date"),
                                      stringsAsFactors=FALSE)
  prev_flight_all_cities_FINAL_2<-
    prev_flight_all_cities_FINAL[which(is.na(prev_flight_all_cities_FINAL$daily_volume)==FALSE),]
  
  colnames(prev_flight_all_cities_FINAL_2)[3]<-"daily_prevalence"
  
  ## estimated number of cases exported from all cities in China to destination countries
  risk_all_cities<-prev_flight_all_cities_FINAL_2%>%
    mutate(prev_vol_product=((daily_prevalence*daily_volume)))%>%
    group_by(destination_country,origin_city)%>%
    summarise(export_risk=sum(prev_vol_product))%>%
    group_by(destination_country)%>% 
    summarise(num_exp_dest=scaling_factor*sum(export_risk))
  
  # create table of estimated exported # of cases in African countries
  
  risk_all_cities_africa_table<-risk_all_cities%>%
    subset(destination_country%in%dest_countries_africa_list)%>%
    mutate(`Destination Country`=destination_country)%>%
    mutate(`Number Exported`=num_exp_dest)%>%
    dplyr::select(`Destination Country`,`Number Exported`)
  
  ## bootstrapping CIs 
  
  risk_all_cities_step1<-prev_flight_all_cities_FINAL_2%>%
    mutate(prev_vol_product=((daily_prevalence*daily_volume)))%>%
    group_by(destination_country,origin_city)%>%
    summarise(export_risk=sum(prev_vol_product))%>%
    group_by(destination_country)%>%
    summarise(summed_export=sum(export_risk))
  
  risk_all_cities_africa_step1<-risk_all_cities_step1%>%
    subset(destination_country%in%dest_countries_africa_list)
  
  colnames(risk_all_cities_africa_step1)<-c("destination_country","risk_importation")
  
  risk_all_cities_africa_ALL<-risk_all_cities_africa_step1%>%
    summarise(total_risk_importation=sum(risk_importation))
  
  colnames(risk_all_cities_africa_ALL)[1]<-"risk_importation"
  
  #bootstrap for each destination country in Africa
  risk_PI<-boot_pi(reg_risk_wuhan, risk_all_cities_africa_step1,n_bootstraps, 0.95) # 50000
  risk_PI$destination_country<-risk_all_cities_africa_step1$destination_country
  colnames(risk_PI)<-c("num_exported","lower","upper","destination_country")
  
  risk_PI_lb<-risk_PI%>%
    group_by(destination_country)%>%
    summarise(num_exp_dest=sum(lower))
  
  risk_PI_ub<-risk_PI%>%
    group_by(destination_country)%>%
    summarise(num_exp_dest=sum(upper))
  
  risk_PI_fitted<-risk_PI%>%
    group_by(destination_country)%>%
    mutate(num_exp_dest=num_exported,1)%>%
    dplyr::select(destination_country,num_exp_dest)
  
  LB_fitted<-merge(risk_PI_lb,risk_PI_fitted,by="destination_country")
  LB_UB_fitted<-merge(LB_fitted,risk_PI_ub,by="destination_country")
  colnames(LB_UB_fitted)<-c("Destination Country","Lower Bound","Mean predicted number",
                            "Upper bound")
  
  write.csv(LB_UB_fitted,paste0("LB_UB_fitted_all_", scenario,".csv"),
            row.names=FALSE)
  
  # estimate total number of exported cases
  num_cases_total<-sum(LB_UB_fitted$`Mean predicted number`)
  num_cases_total_list<-c(num_cases_total_list,num_cases_total)
}

num_cases_total_list
