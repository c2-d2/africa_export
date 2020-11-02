## 29th October 2020
## SCRIPT 1 - Generate time-varying daily prevalence for each province and city for each of the scenarios

## Set the global directory as specified in headers.R
setwd(main_wd)

## Table with key for scenarios
scenario_key <- readxl::read_excel("./data/scenario_key.xlsx")

## Settings for prevalence back-calculation
incu_period <- 5
conf_delay <- 7

## Save outputs to disk or just produce?
save_outputs <- TRUE

## Where to save final prevalence outputs?
overall_save_name <- paste0("./out/all_prev_mods.Rdata")

############################
## read in confirmed case data
############################
confirmed_cases<-read.csv("./data/midas/midas_data_final.csv",stringsAsFactors=FALSE) %>% as_tibble()
# subset to only provinces used in our analysis
provinces<-c('Hubei','Beijing','Shanghai','Guangdong','Henan',
             'Tianjin','Zhejiang','Hunan','Shaanxi','Jiangsu','Chongqing',
             'Jiangxi','Sichuan','Anhui','Fujian') # 15
confirmed_cases_final<-confirmed_cases[confirmed_cases$province_raw%in%provinces,] # 124 unique dates

# match date indices to actual dates
dates_and_date=tibble(dates=seq(as.Date('2020-03-02')-122,as.Date('2020-03-02'),by="day")) %>% 
  mutate(date=(1:n()-1))
confirmed_cases_date <- dplyr::left_join( confirmed_cases_final,dates_and_date, by="date" )

## Remove rows outside of our focal period
confirmed_cases_date <- confirmed_cases_date %>% filter(!is.na(dates))

# remove outliers in Wuhan
which(confirmed_cases_date$province_raw=="Hubei" & confirmed_cases_date$n>4000 ) -> which_replace
confirmed_cases_date$n[ c((which_replace[1]-1),(which_replace[2]+1)) ] -> put_instead
confirmed_cases_date$n[ which_replace ] <- put_instead

## df_city_pop, data frame with population of each City
load(file="./data/df_city_pop.Rdata")

# Possible ways to distribute cases across cities
prov_city_adjust <- get_prov_city_adjust(file="./data/frac_popn_city.Rdata" )

all_outputs <- NULL

## Loop through each scenario and produce prevalence estimates
for(index in 1:nrow(scenario_key)){
  scenario_id <- scenario_key$scenario_id[index]
  save_name <- paste0("./out/city_prev_mod",str_pad(scenario_id,2,pad="0"),".Rdata")
  
  name_scenario <- paste0("Scenario ", scenario_id)
  sensitivity_group <- scenario_key$sensitivity[index]
  scenario_description <- scenario_key$name[index]
  
  ## Note that if the entry is "varied", these will turn to NA
  prev_days <- as.numeric(scenario_key$days_prevalent[index])
  asc_nonhubei_v_hubei <- as.numeric(scenario_key$ARR[index])
  
  ## How are province cases assigned to cities?
  assignment <- scenario_key$assignment[index]
  
  print("")
  print(paste0("Generating scenario ID: ",scenario_id, "; ARR: ", asc_nonhubei_v_hubei, "; days prevalent: ", prev_days,
            "; assignment: ", assignment))
  
  # backculation: shift by mean reporting delays & incubation period
  all_incidence_province <- shift_2_delays(confirmed_cases_date,incubation_period=-incu_period,delay=-conf_delay)
  
  ## If using time-varying ascertainment rates, use the estimated onset data from Tsang et al. directly
  if(is.na(asc_nonhubei_v_hubei)){
    all_incidence_province <- import("./data/tsang_predictions_apportioned.csv") %>%
      rename(n=n_predict) %>%
      mutate(date=as.Date(date))%>%
      mutate(date_full=date) %>%
      rename(dates=date) %>%
      filter(ver == "Overall")
    all_incidence_province <- shift_2_delays(all_incidence_province,incubation_period=-incu_period,delay=0)
    asc_nonhubei_v_hubei <- 1
  }
  
  # plot the back-shifted curves
  p <- plot_conf_onset(all_incidence_province,confirmed_cases_date)
  
  # compute cumulative incidence and add population size
  prov_cum_incidence <- comp_cum_incidence( all_incidence_province, 
                                            "./data/provinces_popn_size_statista.csv" ) # 15 rows
  
  # add calibration value
  calibration_value <- tibble(  is_hubei=c(0,1),
                                calv=c(asc_nonhubei_v_hubei,1) ,
                                calv_inverse=c(1, 1/asc_nonhubei_v_hubei))

  # calibrate incidence in Hubei and outside
  ## amounts to inflating Hubei case counts
  prov_inc_calibrated <- all_incidence_province %>% 
    mutate(is_hubei=as.numeric(province_raw=="Hubei") ) %>% 
    dplyr::left_join( calibration_value, by="is_hubei" ) %>% 
    mutate( n_infected_cal=n_infected/calv_inverse ,
            n_onset_cal = n_onset/calv_inverse) %>% 
    select( dates,province_raw,n_infected_cal, n_onset_cal) %>%
    rename(n_onset=n_onset_cal)

  ## Figure out what proportion of a province's cases should get assigned to each city
  city_n_inf_caladj <- adjust_prov_prev_by_city( prov_inc_calibrated , 
                                                 prov_city_adjust, 
                                                 assignment=assignment)
  
  
  ## Convert city-level case counts to per capita
  city_n_inf_caladj_den <- city_n_inf_caladj %>% 
    dplyr::left_join(df_city_pop,by=c("city"="asciiname")) %>% 
    mutate(n_infected_caladj=n_infected_caladj/population) %>% select(-population)
  
  # compute travel relevant prevalence
  ## amounts to each case being relevant for travel for `prev_days`
  ## But if NA, then we're using the special scenario where cases are prevalent for longer in Hubei
  if(!is.na(prev_days)){
    prov_inc_prev_cali <- comp_travel_rel_prev(city_n_inf_caladj_den, rel_dur = prev_days) 
    prov_inc_prev_cali_raw <- comp_travel_rel_prev(city_n_inf_caladj, rel_dur = prev_days) 
  } else {
    prov_inc_prev_cali <- comp_travel_rel_prev_nonwuh_gap(city_n_inf_caladj_den) 
  }
  
  ## Sense check
  ## Plot incidence and prevalence together
  p_assigned_and_prevalent <- prov_inc_prev_cali %>% ggplot() + 
    geom_line(aes(x=date,y=travel_prev)) + 
    geom_line(data=city_n_inf_caladj_den, aes(x=date,y=n_infected_caladj),col="red") + 
    facet_wrap(~city,scales="free_y")
  
  # rename columns for master table
  city_prev_mod0 <- prov_inc_prev_cali %>% 
    rename( origin_city=city,
            prevalence_o=travel_prev) %>% 
    mutate(scenario=name_scenario,
           sensitivity=sensitivity_group,
           description=scenario_description) %>% 
    select(origin_city,scenario,sensitivity,description,date,prevalence_o)
  
  # fill in missing dates for master table
  seq(from=ymd("2019-11-01"),to=ymd("2020-03-03"), by=1  ) -> dates_mt # 124 dates
  tidyr::expand_grid(origin_city=unique(city_prev_mod0$origin_city),
              scenario=name_scenario,
              sensitivity=sensitivity_group,
              description=scenario_description,
              date=dates_mt) %>% dplyr::left_join( city_prev_mod0, by=c("origin_city","scenario","sensitivity","description","date") ) %>% 
    mutate(prevalence_o=replace_na(prevalence_o,replace = 0)) -> city_prev_mod0
  
  # save
  if(save_outputs){
    save(city_prev_mod0, file = save_name )
  }
  
  all_outputs[[index]] <- city_prev_mod0
}

## Combine into one tibble
all_outputs <- do.call("bind_rows", all_outputs)
if(save_outputs){
  save(all_outputs, file=overall_save_name)
}

## Have a look at the different sensitivity analyses
p_main <- all_outputs %>% filter(sensitivity == "main") %>%
  ggplot() +
  geom_line(aes(x=date,y=prevalence_o,col=scenario)) +
  facet_wrap(~origin_city,scales="free_y") +
  export_theme +
  theme(legend.position=c(0.8,0.1))

p_prevalent <- all_outputs %>% filter(sensitivity == "days_prevalent" | scenario == "Scenario 1") %>%
  ggplot() +
  geom_line(aes(x=date,y=prevalence_o,col=scenario)) +
  facet_wrap(~origin_city,scales="free_y") +
  export_theme +
  theme(legend.position=c(0.8,0.1))

p_arr <- all_outputs %>% filter(sensitivity == "ARR" | scenario == "Scenario 1") %>%
  ggplot() +
  geom_line(aes(x=date,y=prevalence_o,col=scenario)) +
  facet_wrap(~origin_city,scales="free_y") +
  export_theme +
  theme(legend.position=c(0.8,0.1))

p_assignment <- all_outputs %>% filter(sensitivity == "assignment" | scenario == "Scenario 1") %>%
  ggplot() +
  geom_line(aes(x=date,y=prevalence_o,col=scenario)) +
  facet_wrap(~origin_city,scales="free_y") +
  export_theme +
  theme(legend.position=c(0.8,0.1))

p_main
p_prevalent
p_arr
p_assignment
