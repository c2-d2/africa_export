# Source: https://github.com/timktsang/covid19_casedef/blob/master/2020_03_10_figure3.R (timktsang)
library(ggplot2)
library(tidyverse)
library(data.table)
library(Rcpp)
library(stats)
library(patchwork)
library(RColorBrewer)

setwd("~/Documents/GitHub/africa_export/")## Read in MCMC outputs

source("./code/simpler_method_fun.R")
## Read in pre-computed MCMC chain and code
load("data/tsang2020/image_0.729555189609528.Rdata")
sourceCpp("data/tsang2020/nov.cpp")

####################################################
## CHECKPOINT 1.
## - Dates when case definitions changed
####################################################
## Get ascertainment rate periods
dates <- as.Date(as.character(data$date),origin="01/01/2019",format="%d/%m/%Y")
start_date <- c(min(dates), as.Date(c("2020-01-15","2020-01-25","2020-01-28","2020-02-01")))
end_date <- c(as.Date(c("2020-01-24","2020-01-27","2020-01-31","2020-02-05")),max(dates))
enumerated_asc_rates <- tibble(date=seq(as.Date("2019-11-01"), as.Date("2020-03-05"),by="1 day")) %>%
  mutate(indicator=5,
         indicator=ifelse(date < start_date[5], 4,indicator),
         indicator=ifelse(date < start_date[4], 3,indicator),
         indicator=ifelse(date < start_date[3], 2,indicator),
         indicator=ifelse(date < start_date[2], 1,indicator)
  )

## Data used by Tsang et al.
## Append some blank rows for longer predictions
data1 <- rbind(data1, matrix(0, ncol=4,nrow=14))

## Reshape data for plot
data2 <- as_tibble(data1)
colnames(data2) <- c("Wuhan","Hubei","China (non-Hubei)", "Total")
data2 <- data2[,1:3]
data2$date <- seq(as.Date("2019-12-02"),as.Date("2020-03-05"),by="1 day")
data1_melted <- data2 %>% pivot_longer(-date)

## Total cases in China by date of report
data_tot <- data1[,4] %>% as_tibble() %>% rename("Total" = "value")
data_tot$date <- seq(as.Date("2019-12-02"),as.Date("2020-03-05"),by="1 day")

####################################################
## CHECKPOINT 2.
## - Generating predicted trajectories using posterior draws
####################################################
## tt is the MCMC chain
chain <- tt[[1]]
## Burn in and thin
chain <- chain[seq(2000,nrow(chain),by=100),]
## plot(coda::as.mcmc(chain))

## Generate posterior trajectory estimates
all_preds <- NULL
all_fits <- NULL
dates <- seq(as.Date("2019-12-01"),as.Date("2019-12-01")+nrow(data1)-1,by="1 day")
for(i in 1:nrow(chain)){
  pars <- chain[i,]
  gg1 <- pred(data1, pars)
  
  ## Model fits
  fit_tmp <- gg1[[2]]
  fit_tmp <- as.data.frame(fit_tmp[,c(1,2,3)])
  #pred_tmp <- as.data.frame(pred_tmp[,c(5,10,15)])
  colnames(fit_tmp) <- c("Wuhan","Hubei","China (non-Hubei)")
  fit_tmp$samp <- i
  fit_tmp$date <- dates
  all_fits[[i]] <- fit_tmp
  
  ## Model predictions
  pred_tmp <- gg1[[4]]
  pred_tmp <- as.data.frame(pred_tmp[,c(5,10,15)])
  colnames(pred_tmp) <- c("Wuhan","Hubei","China (non-Hubei)")
  pred_tmp$samp <- i
  pred_tmp$date <- dates
  all_preds[[i]] <- pred_tmp
}
all_preds <- do.call("rbind", all_preds)
all_fits <- do.call("rbind", all_fits)

## Melt to long format
all_preds <- all_preds %>% as_tibble() %>% pivot_longer(-c(samp, date)) %>%
  left_join(data1_melted %>% rename(observed=value)) %>%
  mutate(observed=ifelse(is.na(observed),0,observed))

## Get posterior estimates on model trajectories
quants <- all_preds %>%
  group_by(name,date) %>%
  summarize(lower=quantile(value,0.025),
            median=quantile(value,0.5),
            upper=quantile(value,0.975),
            mean=mean(value))
## Get posterior estimates on model fits
quants_fit <- all_fits %>% as_tibble() %>% pivot_longer(-c(samp, date)) %>%
  group_by(name,date) %>%
  summarize(lower=quantile(value,0.025),
            median=quantile(value,0.5),
            upper=quantile(value,0.975),
            mean=mean(value))

## Get posterior estimates on ascertainment rates
quants_asc <- all_preds %>% 
  group_by(name,date) %>%
  summarize(lower=quantile(observed/value,0.025),
            median=quantile(observed/value,0.5),
            upper=quantile(observed/value,0.975),
            mean=mean(observed/value))

## Model predicted incidence based on Tsang et al. Fig 3
gg <- pred(data1,z1[,1])
pred_dat <- gg[[4]]
## Take posterior mean assuming version 5 reporting throughout
pred_final <- as.data.frame(pred_dat[,c(5,10,15)])
colnames(pred_final) <- c("Wuhan","Hubei","China (non-Hubei)")
pred_final$date <- seq(as.Date("2019-12-02"),as.Date("2020-03-05"),by="1 day")
pred_final <- pred_final %>% pivot_longer(-date)

## Combination of Figure 2 and 3 from Tsang et al.
p_tsang <- ggplot(quants) + 
  #geom_line(data=data_tot,aes(x=date,y=Total),col="grey40",linetype="dashed") +
  geom_bar(data=data1_melted,aes(x=date,y=value),fill="grey40",stat="identity") +
  geom_ribbon(aes(x=date,ymin=lower,ymax=upper),fill="blue",alpha=0.25) +
  geom_line(aes(x=date,y=median),col="blue") +
  geom_ribbon(data=quants_fit,aes(x=date,ymin=lower,ymax=upper),fill="red",alpha=0.25) +
  geom_line(data=quants_fit,aes(x=date,y=median),col="red") +
  geom_line(data=pred_final,aes(x=date,y=value),linetype="dashed") +
  theme_bw() +
  scale_x_date(breaks="7 days") +
  scale_y_log10() +
  xlab("Date of onset") +
  ylab("log10(Onsets)") +
  ggtitle("Red = model fits, blue = model predictions, grey = observed onsets, dashed line = Fig 3") +
  facet_wrap(~name,ncol=1,scales="free_y")
p_tsang

##############################
## GET ASCERTAINMENT RATES
##############################
## Get ascertainment rates per day
## This is the proportion of true onsets that were observed per day
comb_dat <- data1_melted %>% rename(observed=value) %>%
  left_join(quants %>% left_join(pred_final %>% rename("posterior_median"=value))) %>%
  mutate(median_asc = observed/median,
         tsang_median_asc=observed/posterior_median,
         tsang_mean_asc=observed/mean) %>%
  left_join(enumerated_asc_rates) %>%
  drop_na()


## This is just observed cases per day divided by reported cases per day
p_quants <- quants_asc %>% ggplot() +
  geom_ribbon(aes(x=date,ymin=lower,ymax=upper),fill="blue",alpha=0.25) +
  geom_line(aes(x=date,y=median)) +
  #geom_line(aes(x=date,y=mean),linetype="dashed") +
  geom_line(data=comb_dat, aes(x=date,y=tsang_median_asc),linetype="dashed") +
  ylab("Proportion of true onsets reported") +
  ggtitle("Dashed line is version from Tsang et al.") +
  xlab("Date of onset") +
  facet_wrap(~name, ncol=1) +
  geom_hline(yintercept=1,linetype="dashed") +
  theme_bw()
p_quants

## Get ascertainment rates per period, maximum of 1
per_period <- comb_dat %>% group_by(name, indicator) %>%
  summarize(sum_observed=sum(observed),sum_pred=sum(posterior_median)) %>%
  mutate(asc_rate=sum_observed/sum_pred) %>%
  mutate(asc_rate=pmin(1,asc_rate)) %>%
  left_join(enumerated_asc_rates)

p_per_period <- per_period %>% ggplot() + geom_line(aes(x=date,y=asc_rate,col=name))

#write_csv(enumerated_asc_rates, "data/tsang_ascertainment_rates2.csv")

####################################
## SCENARIO 10
####################################
## Filter by provinces we are interested in
provinces<-c('Hubei','Beijing','Shanghai','Guangdong','Henan',
             'Tianjin','Zhejiang','Hunan','Shaanxi','Jiangsu','Chongqing',
             'Jiangxi','Sichuan','Anhui','Fujian')

## All confirmed cases
confirmed_cases<-read.csv("./data/midas_data_final.csv",stringsAsFactors=FALSE) %>% as_tibble()
confirmed_cases$date_full <- as.Date(confirmed_cases$date_full) 
# match date indices to actual dates
dates_and_date=tibble(dates=seq(as.Date('2020-03-02')-122,max(confirmed_cases$date_full),by="day")) %>% 
  mutate(date=(1:n()-1))
confirmed_cases_final <- confirmed_cases
confirmed_cases_date <- left_join( confirmed_cases_final,dates_and_date, by="date" ) 

## Back-shift to get infection and onset incidence
## Only want onsets within date range of predictions
all_incidence_province <- shift_2_delays(confirmed_cases_date,incubation_period=-5,delay=-7) %>% 
  mutate(dates=date_full) %>% 
  filter(dates <= max(quants$date))

## Get non-Hubei provinces and label date periods
china_onsets <- all_incidence_province %>% 
  select(dates, province_raw, n_onset) %>% 
  rename(date=dates) %>%
  filter(province_raw != "Hubei") %>%
  left_join(enumerated_asc_rates) 

## See how our back-shifted onsets compare to WHO data
p_compare_onsets <- china_onsets %>% group_by(date) %>%
  summarize(n=sum(n_onset))%>%
  ggplot() +
  geom_line(aes(x=date,y=n)) +
  geom_line(data=data1_melted %>% filter(name == "China (non-Hubei)"),
            aes(x=date,y=value),col="red") +
  scale_x_date(breaks="7 days") +
  xlab("Date of onset") +
  ylab("Total onsets (excl Hubei)") +
  ggtitle("Red is total onsets from Tsang et al. Black is back-shifted from MIDAS data") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,hjust=1))
p_compare_onsets

####################################################
## VERSION 1: GROUPED INTO TIME PERIODS
####################################################
## Sum onsets in each time period
all_onsets <- china_onsets %>% 
  group_by(indicator) %>% 
  summarize(total=sum(n_onset)) %>%
  drop_na()

## Find proportion of cases attributed to each province in each window of time
prop_mod <- china_onsets %>% 
  left_join(all_onsets) %>% 
  left_join(enumerated_asc_rates) %>%
  group_by(indicator,province_raw) %>%
  summarize(n_tot=sum(n_onset,na.rm=TRUE)) %>%
  left_join(all_onsets) %>%
  mutate(n_prop=n_tot/total) %>%
  drop_na()

## Plot proportion of onsets from each province in each time period
p_prop_onsets_indicator <- prop_mod %>% ggplot() + 
  geom_line(aes(x=indicator,y=n_prop,col=province_raw)) + 
  xlab("Time period") +
  ylab("Proportion of onsets") +
  theme_bw() +
  facet_wrap(~province_raw)

## Predictions from Tsang et al.
tsang_predictions_ver1 <- quants %>% filter(name=="China (non-Hubei)") %>% 
  left_join(enumerated_asc_rates) %>% 
  left_join(prop_mod) %>%
  mutate(n_predict = n_prop*median)

## Combine with Wuhan predictions
tsang_predictions_ver1 <- tsang_predictions_ver1 %>% 
  filter(province_raw %in% provinces) %>%
  bind_rows(quants %>% filter(name == "Wuhan") %>% mutate(province_raw = "Hubei", n_predict=median))

####################################################
## VERSION 2: CONTRIBUTION OVER ALL TIME
####################################################
## Find proportion of cases attributed to each province overall
prop_mod_v2 <- confirmed_cases %>% filter(province_raw != "Hubei") %>% 
  group_by(province_raw) %>% summarize(n=sum(n)) %>%
  mutate(n_prop=n/sum(n),
         indicator=1)

## Plot proportion of onsets from each province in each time period
p_prop_onsets_overall <- prop_mod_v2 %>% ggplot() + 
  geom_bar(aes(y=n_prop,x=province_raw),stat="identity") + 
  xlab("Province") +
  ylab("Proportion of onsets") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,hjust=1))

## Predictions from Tsang et al.
tsang_predictions_ver2 <- quants %>% filter(name=="China (non-Hubei)") %>% 
  mutate(indicator=1) %>%
  left_join(prop_mod_v2) %>%
  mutate(n_predict = n_prop*median)
  

tsang_predictions_ver2 <- tsang_predictions_ver2 %>% 
  filter(province_raw %in% provinces) %>%
  bind_rows(quants %>% filter(name == "Wuhan") %>% mutate(province_raw = "Hubei", n_predict=median))

tsang_predictions_both <- tsang_predictions_ver1 %>% 
  select(date, province_raw, n_predict) %>%
  mutate(ver="Time-varying") %>% 
  bind_rows(tsang_predictions_ver2 %>%  select(date, province_raw, n_predict) %>% mutate(ver="Overall")) %>%
  select(-name)

p_apportioned_incidence <- tsang_predictions_both %>% ggplot() +
  geom_bar(data=china_onsets %>%filter(province_raw %in% provinces),
           aes(x=date,y=n_onset),
           stat="identity",fill="grey40",col="black",size=0.1) +
  geom_line(aes(x=date,y=n_predict,col=ver)) +
  theme_bw() +
  xlab("Date") +
  ylab("Onsets") +
  ggtitle("Lines show scaled onsets, grey bars show back-shifted MIDAS data") +
  scale_x_date(breaks="7 days",limits=c(as.Date("2019-12-01"),as.Date("2020-03-05"))) +
  theme(axis.text.x=element_text(angle=45,hjust=1),legend.position = c(0.9,0.1)) +
  facet_wrap(~province_raw,scales="free_y")
p_apportioned_incidence

## Comparison of Tsang fits and predictions against observed data
p_tsang

## Posterior medians, 95% CI and Tsang estimates on daily ascertainment rates
p_quants

## Posterior medians on per-period ascertainment rates
p_per_period 

## Comparison of onsets used for fitting in Tsang et al. and our back-shifted MIDAS data
p_compare_onsets

## Proportion of onsets allocated to each province by time period
p_prop_onsets_indicator

## Proportion of onsets allocated to each province overall
p_prop_onsets_overall

## Apportioned per-province estimates under time period and overall splitting against observed data (grey bars)
p_apportioned_incidence

## Daily ascertainment rates
quants_asc

## Overall ascertainment rates per period, max 1
per_period

## Tsang onset incidence estimates
tsang_predictions_both
write_csv(tsang_predictions_both %>% select(-name), "data/tsang_predictions_apportioned.csv")

