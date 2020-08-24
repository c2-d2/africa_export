## TITLE: ComputingRatios
## Description: Calculating the ratio of non-Wuhan exports to Wuhan exports
## Author: Tigist Menkir (Center for Communicable Disease Dynamics, Harvard T.H. Chan School of Public Health)
## Date: 12 May 2020

#setwd("~/Desktop/nCoV exports")

library(dplyr)
library(stringr)

## read in flight data, if not already loaded
#flights<-read.csv("flights_adjusted_2000506.csv")
# colnames(flights)[2]<-"iata"

## define date ranges and destination country groups
dates_seq<-seq(as.Date('2019-12-08'),as.Date('2020-02-29'),by="day")
dates_seq_wuhan<-seq(as.Date('2019-12-08'),as.Date('2020-01-23'),by="day")
dest_countries_all_list<-unique(flights$destCtryName)
dest_countries_africa_list<-c('Mauritania','Mauritius','South Africa','Kenya','Egypt','Ethiopia','Morocco',
                              'Algeria','Nigeria','Ghana','Tanzania','Senegal','Guinea',
                              'Zimbabwe','Congo (Kinshasa)','Sudan','Angola','Zambia',
                              'Gabon','Madagascar','Equatorial Guinea','Tunisia',
                              'Uganda','Mozambique','Seychelles','Cote D\'Ivoire')

## read in combined prevalence/flight data for Wuhan
prev_flight_wuhan_lower<-read.csv("./out/prev_flight_wuhan_2_Lower.csv")
prev_flight_wuhan_lower$Scenario<-rep("Lower",nrow(prev_flight_wuhan_lower))
prev_flight_wuhan_intermediate<-read.csv("./out/prev_flight_wuhan_2_Intermediate.csv")
prev_flight_wuhan_intermediate$Scenario<-rep("Intermediate",nrow(prev_flight_wuhan_intermediate))
prev_flight_wuhan_upper<-read.csv("./out/prev_flight_wuhan_2_Upper.csv")
prev_flight_wuhan_upper$Scenario<-rep("Upper",nrow(prev_flight_wuhan_upper))

prev_flight_wuhan_ALL<-rbind(prev_flight_wuhan_lower,prev_flight_wuhan_intermediate,
                             prev_flight_wuhan_upper)

## read in prevalence data for all cities, if not already loaded from prev estimation script
# prev_all_scenarios_combined<-read.csv("prev_all_scenarios_combined.csv")

########## Loop over prevalence data scenarios

ratio_africa_list<-c()
ratio_international_list<-c()

for (scenario in Scenarios){
  ## Wuhan 
  prev_flight_wuhan_ALL_i<-prev_flight_wuhan_ALL[prev_flight_wuhan_ALL$Scenario==scenario,]
  
  ## estimate # exported cases from Wuhan to each destination country
  risk_wuhan_ALL<-prev_flight_wuhan_ALL_i%>%
    mutate(prev_vol_product=(prevalence*daily_volume))%>%
    group_by(destination_country)%>%
    summarise(risk_importation=sum(prev_vol_product))
  
  # subset Wuhan exports to african countries and summarize n 
  risk_wuhan_africa<-risk_wuhan_ALL%>%
    subset(destination_country%in%dest_countries_africa_list)%>%
    summarise(sum_n=sum(risk_importation))
  
  ## estimate # exported cases from Wuhan to each destination country, by date
  risk_wuhan_ALL_date<-prev_flight_wuhan_ALL_i%>%
    mutate(prev_vol_product=(prevalence*daily_volume))%>%
    group_by(destination_country,date)%>%
    group_by(date)%>%
    summarise(num_exp_dest_wuh=sum(prev_vol_product))
  
  ## estimate # exported cases from Wuhan to African destination countries, by date
  risk_wuhan_africa_date<-prev_flight_wuhan_ALL_i%>%
    subset(destination_country%in%dest_countries_africa_list)%>%
    mutate(prev_vol_product=(prevalence*daily_volume))%>%
    group_by(destination_country,date)%>%
    group_by(date)%>%
    summarise(num_exp_dest_wuh=sum(prev_vol_product))
  
  ## All origin cities, excluding Wuhan
  # note: identical processing steps to prevalence_all_cities_ in estimating_exported_cases script
  # this time subsetting to all origin cities, excluding Wuhan
  prev_cities_i<-prev_all_scenarios_combined[prev_all_scenarios_combined$Scenario==scenario,]
  prevalence_all_cities_1_NW<-prev_cities_i[prev_cities_i$cities!='Wuhan',-1]
  colnames(prevalence_all_cities_1_NW)<-dates_prev
  prevalence_all_cities_final_NW<-cbind(prev_cities_i$cities[prev_cities_i$cities!='Wuhan'],prevalence_all_cities_1_NW)
  prevalence_all_cities_final_NW<-prevalence_all_cities_final_NW[duplicated(prevalence_all_cities_final_NW$`prev_cities_i$cities`)==FALSE,]
  colnames(prevalence_all_cities_final_NW)[1]<-"cities"
  prevalence_all_cities_final_2_NW<-prevalence_all_cities_final_NW[,c(1,39:122)]
  
  prevalence_all_cities_FINAL_NW<-melt(prevalence_all_cities_final_2_NW,
                                       id.vars="cities",variable.name="date")
  
  prevalence_all_cities_FINAL_NW$cities<-str_replace_all(prevalence_all_cities_FINAL_NW$cities,
                                                         "Xi'an",'Xi An')
  
  # subset flight data from all origin cities, excluding Wuhan, to all destination countries
  flights_all_cities_NW<-flights%>%
    subset(iata!="WUH"&destCtryName%in%dest_countries_all_list)
  
  flights_all_cities_NW_duplicated<-flights_all_cities_NW%>%
    subset(iata=='SZX'|iata=='HGH')%>%
    mutate(dailyvol=dailyvol/2)
  
  flights_all_cities_NW_duplicated_copy<-flights_all_cities_NW_duplicated
  flights_all_cities_NW_duplicated_copy$origCityName<-
    str_replace_all(flights_all_cities_NW_duplicated_copy$origCityName,'Hangzhou','Jiaxing')
  flights_all_cities_NW_duplicated_copy$origCityName<-
    str_replace_all(flights_all_cities_NW_duplicated_copy$origCityName,'Shenzhen','Dongguan')
  
  iata_not_duplicated<-c('CAN','CGO','CKG','CSX','CTU','FOC','HFE','KHN','NAY/PKX',
                         'NKG','PEK','PVG','SHA',"TSN",'XIY','WUH')
  flights_all_cities_NW_not_duplicated<-flights_all_cities_NW%>%
    subset(iata%in%iata_not_duplicated)
  
  flights_all_cities_NW_final<-data.frame(rbind(flights_all_cities_NW_not_duplicated,
                                                flights_all_cities_NW_duplicated,
                                                flights_all_cities_NW_duplicated_copy))
  
  flights_all_cities_NW_final<-
    flights_all_cities_NW_final[factor(flights_all_cities_NW_final$destCtryName)%in%dest_countries_all_list,]
  
  flights_all_cities_NW_final$flight_date<-as.Date(flights_all_cities_NW_final$flight_date,format='%Y-%m-%d')
  final_flights_all_cities_NW<-flights_all_cities_NW_final[flights_all_cities_NW_final$flight_date%in%dates_seq,]
  flights_date_all_cities_NW<-cbind.data.frame(final_flights_all_cities_NW$flight_date,final_flights_all_cities_NW$origCityName,
                                               final_flights_all_cities_NW$destCtryName, final_flights_all_cities_NW$dailyvol)
  colnames(flights_date_all_cities_NW)<-c("date","origin_city","destination_country","daily_volume")
  
  colnames(flights_date_all_cities_NW)[1]<-"date"
  colnames(prevalence_all_cities_FINAL_NW)[1]<-"origin_city"
  flights_date_all_cities_NW$date<-as.factor(flights_date_all_cities_NW$date)
  prev_flight_all_cities_NW_FINAL<-merge(prevalence_all_cities_FINAL_NW,
                                         flights_date_all_cities_NW,by=c("origin_city","date"),
                                         stringsAsFactors=FALSE)
  prev_flight_all_cities_NW_FINAL_2<-
    prev_flight_all_cities_NW_FINAL[which(is.na(prev_flight_all_cities_NW_FINAL$daily_volume)==FALSE),]
  
  colnames(prev_flight_all_cities_NW_FINAL_2)[3]<-"daily_prevalence"
  
  # estimate number of exported cases from from all origin cities (excluding Wuhan)
  # to all destination countries 
  risk_all_cities_NW<-prev_flight_all_cities_NW_FINAL_2%>%
    mutate(prev_vol_product=((daily_prevalence*daily_volume)))%>%
    group_by(destination_country,origin_city)%>%
    summarise(export_risk=sum(prev_vol_product))%>%
    group_by(destination_country)%>% 
    summarise(num_exp_dest=sum(export_risk))
  
  # subset all other cities' exports to african countries and summarize n 
  risk_all_cities_NW_africa<-risk_all_cities_NW%>%
    subset(destination_country%in%dest_countries_africa_list)%>%
    summarise(sum_n=sum(num_exp_dest))
  
  # estimate number of exported cases from from all origin cities (excluding Wuhan)
  # to all destination countries at each date
  risk_all_cities_NW_date<-prev_flight_all_cities_NW_FINAL_2%>%
    mutate(prev_vol_product=((daily_prevalence*daily_volume)))%>%
    group_by(destination_country,origin_city,date)%>%
    summarise(export_risk=sum(prev_vol_product))%>%
    group_by(destination_country,date)%>% 
    group_by(date)%>%
    summarise(num_exp_dest_all=sum(export_risk))
  
  # estimate number of exported cases from from all origin cities (excluding Wuhan)
  # to African destination countries at each date
  risk_all_cities_NW_africa_date<-prev_flight_all_cities_NW_FINAL_2%>%
    subset(destination_country%in%dest_countries_africa_list)%>%
    mutate(prev_vol_product=((daily_prevalence*daily_volume)))%>%
    group_by(destination_country,origin_city,date)%>%
    summarise(export_risk=sum(prev_vol_product))%>%
    group_by(destination_country,date)%>% 
    group_by(date)%>%
    summarise(num_exp_dest_all=sum(export_risk))
  
  # combined Wuhan and all cities export estimates
  merged2<-merge(data.frame(risk_wuhan_ALL_date, row.names=NULL), 
                 data.frame(risk_all_cities_NW_date, row.names=NULL), by = 0, all = TRUE)[-1]
  merged2$num_exp_dest_wuh<-ifelse(is.na(merged2$num_exp_dest_wuh)==TRUE,0,
                                   merged2$num_exp_dest_wuh)
  # separately calculate proportion of Wuhan exports of Wuhan + all other cities' exports
  # and proportion of all other cities' exports of Wuhan + all other cities' exports
  merged2<-data.frame(cbind(merged2,
                            merged2$num_exp_dest_wuh/sum(merged2$num_exp_dest_wuh,merged2$num_exp_dest_all),
                            merged2$num_exp_dest_all/sum(merged2$num_exp_dest_wuh,merged2$num_exp_dest_all)))
  colnames(merged2)[5]<-"frac_wuhan"
  colnames(merged2)[6]<-"frac_all"
  
  merged2$date.y<-as.Date(merged2$date.y)
  
  write.csv(merged2,paste0("./out/export_curves_wuh_all_", scenario,
                                                  ".csv"),row.names=FALSE)
  # repeat the above for African destination countries
  merged2_africa<-merge(data.frame(risk_wuhan_africa_date, row.names=NULL), 
                        data.frame(risk_all_cities_NW_africa_date, row.names=NULL), by = 0, all = TRUE)[-1]
  merged2_africa$num_exp_dest_wuh<-ifelse(is.na(merged2_africa$num_exp_dest_wuh)==TRUE,0,
                                          merged2_africa$num_exp_dest_wuh)
  merged2_africa<-data.frame(cbind(merged2_africa,
                                   merged2_africa$num_exp_dest_wuh/sum(merged2_africa$num_exp_dest_wuh,merged2_africa$num_exp_dest_all),
                                   merged2_africa$num_exp_dest_all/sum(merged2_africa$num_exp_dest_wuh,merged2_africa$num_exp_dest_all)))
  colnames(merged2_africa)[5]<-"frac_wuhan"
  colnames(merged2_africa)[6]<-"frac_all"
  
  merged2_africa$date.y<-as.Date(merged2_africa$date.y)
  
  write.csv(merged2_africa,paste0("./out/export_curves_wuh_all_africa_", scenario,
                                 ".csv"),row.names=FALSE)
  
  # ratio of Wuhan exports to all other cities 
  ratio_africa<-risk_all_cities_NW_africa/risk_wuhan_africa
  ratio_africa_list<-c(ratio_africa_list,ratio_africa)
  
  ## calculate ratio of number of cases exported from Wuhan to number of cases 
  ## exported from all other cities, among all countries
  
  # summarise n 
  risk_wuhan_international<-risk_wuhan_ALL%>%
    summarise(sum_n=sum(risk_importation))
  
  # subset all other cities' exports to all countries
  risk_all_cities_NW_international<-risk_all_cities_NW%>%
    summarise(sum_n=sum(num_exp_dest))
  
  # ratio of Wuhan exports to all other cities 
  ratio_international<-risk_all_cities_NW_international/risk_wuhan_international
  ratio_international_list<-c(ratio_international_list,ratio_international)
}

ratio_international_intermediate_scenario<-ratio_international_list[[2]]
ratio_africa_intermediate_scenario<-ratio_africa_list[[2]]



