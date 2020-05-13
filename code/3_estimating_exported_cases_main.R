## TITLE: estimating_exported_cases
## Description: Estimating # of exported cases in validation countries and subset of African countries
## Author: Tigist Menkir (Center for Communicable Disease Dynamics, Harvard T.H. Chan School of Public Health)
## Date: 12 May 2020

setwd("~/Desktop/nCoV exports")

library(dplyr)
library(reshape2)
library(formattable)
library(stringr)
library(ggplot2)
library(formattable)
library(doParallel)
library(foreach)

## define date sequences and selection of countries for validation & prediction steps 

dates_seq<-seq(as.Date('2019-12-08'),as.Date('2020-02-29'),by="day")
dates_prev<-seq(as.Date('2019-11-01'),as.Date('2020-03-03'),by="day")
dates_seq_wuhan<-seq(as.Date('2019-12-08'),as.Date('2020-01-23'),by="day")
dest_countries_validation_list<-c('United States','Australia','Canada','Korea (South)',
                                  'United Kingdom',
                                  'Netherlands','Sweden',
                                  'Germany','Spain','Singapore')
dest_countries_africa_list<-c('Mauritius','Mauritania','South Africa','Kenya','Egypt','Ethiopia','Morocco',
                              'Algeria','Nigeria','Ghana','Tanzania','Senegal','Guinea',
                              'Zimbabwe','Congo (Kinshasa)','Sudan','Angola','Zambia',
                              'Gabon','Madagascar','Equatorial Guinea','Tunisia',
                              'Uganda','Mozambique','Seychelles','Cote D\'Ivoire')


## read in flight data
flights<-read.csv("flights_adjusted_200506.csv")

## process flight data -- validation step
colnames(flights)[2]<-"iata"
dest_countries_all_list<-unique(flights$destCtryName)

# subset to only Wuhan flights to validation countries, up to Jan 23
flights_wuhan<-flights%>%
  subset(iata=="WUH"&destCtryName%in%dest_countries_validation_list)

flights_wuhan$flight_date<-as.Date(flights_wuhan$flight_date,format='%Y-%m-%d')
final_flights_wuhan<-flights_wuhan[flights_wuhan$flight_date%in%dates_seq_wuhan,]
flights_date_wuhan<-cbind.data.frame(final_flights_wuhan$flight_date,final_flights_wuhan$destCtryName, final_flights_wuhan$dailyvol)
colnames(flights_date_wuhan)<-c("date","destination_country","daily_volume")

# subset to all Wuhan flights, over the entire study period
flights_wuhan_2<-flights%>%
  subset(iata=="WUH")

flights_wuhan_2$flight_date<-as.Date(flights_wuhan_2$flight_date,format='%Y-%m-%d')
final_flights_wuhan_2<-flights_wuhan_2[flights_wuhan_2$flight_date%in%dates_seq,]
flights_date_wuhan_2<-cbind.data.frame(final_flights_wuhan_2$flight_date,
                                       final_flights_wuhan_2$destCtryName, 
                                       final_flights_wuhan_2$dailyvol)
colnames(flights_date_wuhan_2)<-c("date","destination_country","daily_volume")

## process flight data -- prediction step
# subset to all flights in 26 destination countries
flights$flight_date<-as.Date(flights$flight_date,format='%Y-%m-%d')
flights_all_cities<-flights%>%
  subset(destCtryName%in%dest_countries_all_list)%>%
  subset(flight_date%in%dates_seq)

flights_all_cities_duplicated<-flights_all_cities%>%
  subset(iata=='SZX'|iata=='HGH')%>%
  mutate(dailyvol=dailyvol/2)

flights_all_cities_duplicated_copy<-flights_all_cities_duplicated
flights_all_cities_duplicated_copy$origCityName<-str_replace_all(flights_all_cities_duplicated_copy$origCityName,'Hangzhou','Jiaxing')
flights_all_cities_duplicated_copy$origCityName<-str_replace_all(flights_all_cities_duplicated_copy$origCityName,'Shenzhen','Dongguan')

iata_not_duplicated<-c('CAN','CGO','CKG','CSX','CTU','FOC','HFE','KHN','NAY/PKX',
                       'NKG','PEK','PVG','SHA',"TSN",'XIY','WUH')
flights_all_cities_not_duplicated<-flights_all_cities%>%
  subset(iata%in%iata_not_duplicated)

flights_all_cities_final<-data.frame(rbind(flights_all_cities_not_duplicated,
                                           flights_all_cities_duplicated,
                                           flights_all_cities_duplicated_copy))

flights_date_all_cities<-cbind.data.frame(flights_all_cities_final$flight_date,flights_all_cities_final$origCityName,
                                          flights_all_cities_final$destCtryName, flights_all_cities_final$dailyvol)
colnames(flights_date_all_cities)<-c("date","origin_city","destination_country","daily_volume")

## read in confirmed case data for validation subset of countries
dest_countries_validation_list_df<-c('US','Australia','Canada','Rep  Korea','UK',
                                     'Netherlands','Sweden',
                                     'Germany','Spain','Singapore') 
confirmed_cases_high_capacity_countries<-read.csv("who_imports.csv")
confirmed_cases_final<-confirmed_cases_high_capacity_countries[confirmed_cases_high_capacity_countries$Country
                                                               %in%dest_countries_validation_list_df,]
confirmed_cases_FINAL<-confirmed_cases_final[match(dest_countries_validation_list_df,confirmed_cases_final$Country),]

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

## read in prevalence data, if not already loaded from prev estimation script


########## Loop over prevalence data scenarios

Scenarios<-c("Lower","Intermediate","Upper")

scaling_factor_list<-c()
num_cases_total_list<-c()

for (scenario in Scenarios) {
  ## read in prevalence data 
  prev_cities_i<-prev_all_scenarios_combined[prev_all_scenarios_combined$Scenario==scenario,]
  prevalence_wuhan<-prev_cities_i[prev_cities_i$cities=='Wuhan',colnames(prev_cities_i)!="Scenario"]
  prevalence_wuhan_final<-as.numeric(t(prevalence_wuhan))
  prevalence_wuhan_final<-prevalence_wuhan_final[is.na(prevalence_wuhan_final)==FALSE]
  prevalence_wuhan_final_2<-cbind.data.frame(dates_prev,prevalence_wuhan_final)
  colnames(prevalence_wuhan_final_2)<-c("date","prevalence")
  prevalence_wuhan_FINAL<-prevalence_wuhan_final_2[prevalence_wuhan_final_2$date%in%dates_seq_wuhan,]

  ## combine prevalence and flight data
  prev_flight_wuhan<-merge(prevalence_wuhan_FINAL,flights_date_wuhan,by="date")
  
  ## read in Wuhan prevalence data for entire study period, for calculating ratios
  prevalence_wuhan_FINAL_2<-prevalence_wuhan_final_2[prevalence_wuhan_final_2$date%in%dates_seq,]
  
  ## combine prevalence and flight data
  prev_flight_wuhan_2<-merge(prevalence_wuhan_FINAL_2,flights_date_wuhan_2,by="date")
  
  ## export as CSV for calculating ratios
  write.csv(prev_flight_wuhan_2,paste0("prev_flight_wuhan_2_", scenario, ".csv"),row.names=FALSE)

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

  ## create table of estimated exported # of cases in validation countries
  risk_all_cities_table<-risk_wuhan_confirmed_cases%>%
    mutate(`Destination Country`=Country)%>%
    mutate(`Predicted Number Exported`=round(risk_importation))%>%
    mutate(`Number Reported, scaled`=confirmed_cases_scaled)%>%
    dplyr::select(`Destination Country`,`Predicted Number Exported`,`Number Reported, scaled`)
  
  ## run regression of scaled confirmed cases against # of exported cases
  risk_wuhan_confirmed_cases<-risk_wuhan_confirmed_cases%>%
    dplyr::select(`Country`,`risk_importation`,"confirmed_cases_scaled")

  reg_risk_wuhan<-glm(confirmed_cases_scaled~offset(log(risk_importation)),
                    data=risk_wuhan_confirmed_cases,
                    family=poisson(link="log"))
  
  # print CI for alpha from intermediate scenario
  if (scenario=="Intermediate"){
    print(confint(reg_risk_wuhan) %>% exp())
    predicted_counts_poisson=predict(reg_risk_wuhan,type="response")
  }
  
  ## define scaling factor as the coefficient for risk_wuhan var
  scaling_factor<-exp(coefficients(reg_risk_wuhan)[[1]])
  scaling_factor_list<-c(scaling_factor_list,scaling_factor)
  
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

  write.csv(prev_flight_all_cities_FINAL_2,paste0("prev_flight_all_cities_FINAL_2_", scenario,
                                                 ".csv"),row.names=FALSE)
  
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

  write.csv(risk_all_cities_africa_table,paste0("risk_all_cities_africa_table_", scenario,".csv"),
           row.names=FALSE)
  
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
  risk_PI<-boot_pi(reg_risk_wuhan, risk_all_cities_africa_step1,50000, 0.95)
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
poisson_table<-merge(confirmed_cases_FINAL,risk_wuhan_final,by="Country")
poisson_table<-cbind.data.frame(poisson_table,predicted_counts_poisson)
colnames(poisson_table)[length(poisson_table)]<-"predicted_counts"

ggplot(poisson_table,aes(x=risk_importation,y=predicted_counts))+
  geom_point(aes(y=Cases_lm),alpha=0.5)+
  geom_line(size=1)+xlab("Predicted number of imported cases")+
  ylab("Scaled number of reported cases")

ggsave("regression_fit_plot.pdf",
       width = 14, height = 10, units = "cm")

