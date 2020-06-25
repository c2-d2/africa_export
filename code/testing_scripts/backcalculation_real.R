## TITLE: backcalculation_real
## Description: Backcalculation of onset and infection incidence in China
## Author: James Hay
## Date: 17 June 2020

set.seed(1234)

library(tidyverse)
library(patchwork)
library(ggthemes)

setwd("~/Documents/GitHub/africa_export/")
## Function to convert incidence to prevalence
source("code/functions/prevalence_calculation.R")
Rcpp::sourceCpp("~/Documents/GitHub/africa_export/code/functions/prevalence_calculation_cpp.cpp")
## This function reads in the Zhang et al. linelist data and fits gamma distributions in sliding windows
## to the distribution of reporting delays.
source("code/testing_scripts/fit_delay_distributions.R")
## Functions used for this script
source("code/functions/backcalculation_funcs.R")

##############################################################################################################
## 1. THE DATA
##############################################################################################################
## Read in confirmed case counts
confirmed_cases <- read_csv("data/midas_data_final.csv")

## Subset confirmed case counts to only provinces used in our analysis
provinces<-c('Hubei','Beijing','Shanghai','Guangdong','Henan',
             'Tianjin','Zhejiang','Hunan','Shaanxi','Jiangsu','Chongqing',
             'Jiangxi','Sichuan','Anhui','Fujian')
confirmed_cases_final<-confirmed_cases[confirmed_cases$province_raw%in%provinces,]

# match date indices to actual dates
times <- dates <- seq(as.Date('2019-11-01'),as.Date('2020-03-12'),by="day")
date=seq_along(dates)-1
dates_and_date=cbind.data.frame(dates,date)
confirmed_cases_date=merge(dates_and_date,confirmed_cases_final,by="date")
confirmed_cases_date <- as_tibble(confirmed_cases_date) %>% rename(Location=province_raw)


## Read in line list data from Zhang et al.
linelist_dat <- read_csv("~/Documents/GitHub/covback/data/real/zhang_appendix_lancet.csv")
linelist_dat <- linelist_dat %>% 
  group_by(Location) %>%
  mutate(report_date = as.Date(report_date, origin="2019/01/11"),
         onset_date= as.Date(onset_date, oprigin="2019/01/11"),
         report_delay = report_date - onset_date,
         stage=onset_date < as.Date("2020/01/27")) %>% 
  select(ID, onset_date, report_date,report_delay, stage) %>% drop_na() %>% ungroup()
linelist_dat <- linelist_dat %>% mutate(Location = ifelse(Location %in% c("Other cities in Guangdong","Shenzhen"),"Guangdong", Location))

## Overall mean delay
overall_mean_delay <- mean(linelist_dat$report_delay)

## Mean by day
daily_delays <- linelist_dat %>% 
  group_by(report_date) %>% 
  summarise(mean_delay=mean(report_delay))
p_daily_delay <- daily_delays %>% 
  ggplot(aes(x=report_date,y=mean_delay)) + 
  geom_line() +
  geom_smooth()
p_daily_delay

## Fill in date gaps
## Enumerate dates that are in confirmed case data and
## find the first and last date for which we have a reporting delay estimate
min_date <- min(confirmed_cases_date$dates)
min_date_linelist <- min(linelist_dat$report_date)
max_date <- max(confirmed_cases_date$dates)
max_date_linelist <- max(linelist_dat$report_date)
all_dates <- seq(min_date, max_date,by="1 day")
expanded_delays <- expand_grid(report_date=all_dates,mean_delay=NA)

## For dates before first linelist entry, use first mean delay. For dates after last linelist entry, use last mean delay
first_report_delay <- daily_delays %>% filter(report_date == min(report_date)) %>% pull(mean_delay)
last_report_delay <- daily_delays %>% filter(report_date == max(report_date)) %>% pull(mean_delay)
expanded_delays <- expanded_delays %>% mutate(mean_delay=ifelse(report_date <= min_date_linelist, first_report_delay, last_report_delay))
expanded_delays <- expanded_delays %>% filter(!(report_date %in% unique(daily_delays$report_date)))
daily_delays <- daily_delays %>% mutate(mean_delay=as.numeric(mean_delay)) %>% bind_rows(expanded_delays) %>% arrange(report_date)

## Mean by day and location
daily_loc_delays <- linelist_dat %>% 
  group_by(report_date,Location) %>% 
  summarise(mean_delay_loc=mean(report_delay))

p_daily_loc_delay <- daily_loc_delays %>% 
  ggplot(aes(x=report_date,y=mean_delay_loc)) + 
  geom_line() +
  geom_smooth() +
  facet_wrap(~Location) +
  coord_cartesian(ylim=c(0,30))
p_daily_loc_delay

## Fill in missing delays by date and location
expanded_dates <- expand_grid(report_date=all_dates, Location=unique(daily_loc_delays$Location))
daily_loc_delays <- daily_loc_delays %>% full_join(expanded_dates) %>% arrange(Location,report_date)
daily_loc_delays <- daily_loc_delays %>% group_by(Location) %>%
  left_join( daily_loc_delays %>% group_by(Location) %>% drop_na() %>% 
               filter(report_date==min(report_date)) %>% rename(first_measured_date=report_date,first_delay=mean_delay_loc)) %>%
  left_join( daily_loc_delays %>% group_by(Location) %>% drop_na() %>% 
               filter(report_date==max(report_date)) %>% rename(last_measured_date=report_date,last_delay=mean_delay_loc)) %>%
  mutate(mean_delay_loc = ifelse(report_date < first_measured_date,first_delay,mean_delay_loc),
         mean_delay_loc = ifelse(report_date > last_measured_date,last_delay,mean_delay_loc))


## Have a look at confirmed cases by province used
p_confirmed <- ggplot(confirmed_cases_date) + 
  geom_bar(aes(x=dates,y=n),stat="identity") + 
  facet_wrap(~Location,scales="free_y")
p_confirmed

##############################################################################################################
## 2. SHIFT BY MEANS
##############################################################################################################
## Incubation period parameters
incu_par1 <- 1.621
incu_par2 <- 0.418
incu_mean <- exp(incu_par1 + (incu_par2)^2/2)

## Read in fitted delay distributions. We have a gamma distribution for the confirmation delay
## going either forward or backward from each day of symptom onset or date of confirmation
forward_delay_dists <- read_csv("data/confirm_delay_dists_forward.csv")
backward_delay_dists <- read_csv("data/confirm_delay_dists_backward.csv")
forward_delay_dists <- forward_delay_dists %>% rename(date=date_onset_symptoms)
backward_delay_dists <- backward_delay_dists %>% rename(date=date_confirmation)

linelist_confirmed <- confirmed_cases_date %>% 
  select(-date) %>%
  filter(n > 0) %>%
  rename(date=dates) %>% 
  uncount(n) 


linelist_confirmed_merged <- linelist_confirmed %>%
  left_join(backward_delay_dists %>% select(date, gamma_mean_backward) %>% distinct()) %>% 
  left_join(daily_delays %>% rename(date=report_date)) %>%
  left_join(daily_loc_delays %>% rename(date=report_date)) %>%
  mutate(overall_delay=overall_mean_delay) 

## Symptom onsets
onsets_overall <- linelist_confirmed_merged %>% 
  mutate(date_onset=date-overall_delay,
         infection_date=date_onset - incu_mean) %>%
  group_by(Location, date_onset) %>% tally()%>% drop_na() %>% mutate(ver="Overall mean")
onsets_overall_daily <- linelist_confirmed_merged %>% 
  mutate(date_onset=date-mean_delay,
         infection_date=date_onset - incu_mean) %>%
  group_by(Location, date_onset) %>% tally()%>% drop_na() %>% mutate(ver="Daily mean")
onsets_loc_daily <- linelist_confirmed_merged %>% 
  mutate(date_onset=date-mean_delay_loc,
         infection_date=date_onset - incu_mean) %>%
  group_by(Location, date_onset) %>% tally() %>% drop_na() %>% mutate(ver="Daily per location mean")
onsets <- bind_rows(onsets_overall, onsets_overall_daily,onsets_loc_daily)

## Infections
infections_overall <- linelist_confirmed_merged %>% 
  mutate(date_onset=date-overall_delay,
         infection_date=date_onset - incu_mean) %>%
  group_by(Location, infection_date) %>% tally()%>% drop_na() %>% mutate(ver="Overall mean")
infections_overall_daily <- linelist_confirmed_merged %>% 
  mutate(date_onset=date-mean_delay,
         infection_date=date_onset - incu_mean) %>%
  group_by(Location, infection_date) %>% tally()%>% drop_na() %>% mutate(ver="Daily mean")
infections_loc_daily <- linelist_confirmed_merged %>% 
  mutate(date_onset=date-mean_delay_loc,
         infection_date=date_onset - incu_mean) %>%
  group_by(Location, infection_date) %>% tally() %>% drop_na() %>% mutate(ver="Daily per location mean")
infections <- bind_rows(infections_overall, infections_overall_daily,infections_loc_daily)

## 
p_mean_augments <- ggplot(confirmed_cases_date) + 
  geom_bar(aes(x=dates,y=n),stat="identity") + 
  geom_line(data=onsets,aes(x=date_onset,y=n,col=ver)) + 
  scale_x_date(limits=as.Date(c("2020-01-01","2020-03-03"))) +
  ggtitle("Bars show confirmed case counts, lines show augmented onset counts") +
  ylab("Count") +
  xlab("Date") +
  facet_wrap(~Location,scales="free_y",ncol=3)

p_mean_augments_infections <- ggplot(confirmed_cases_date) + 
  geom_bar(aes(x=dates,y=n),stat="identity") + 
  geom_line(data=infections,aes(x=infection_date,y=n,col=ver)) + 
  scale_x_date(limits=as.Date(c("2020-01-01","2020-03-03"))) +
  ggtitle("Bars show confirmed case counts, lines show augmented onset counts") +
  ylab("Count") +
  xlab("Date") +
  facet_wrap(~Location,scales="free_y",ncol=3)

png("figures/real_naive_shift_onsets.png",width=7,height=4,res=300,units="in")
p_mean_augments
dev.off()
png("figures/real_naive_shift_infections.png",width=7,height=4,res=300,units="in")
p_mean_augments_infections
dev.off()


##############################################################################################################
## 3. SHIFT USING BOOTSTRAP, FIXED ASCERTAINMENT
##############################################################################################################
tmax <- 32
dat1 <- linelist_dat %>% 
  rename(confirmation_delay=report_delay,onset_date=onset_date,report_date=report_date) %>%
  mutate(ascertainment_rate=1)

repeats <- 100
tmp_all_onsets <- NULL
tmp_all_infections <- NULL
tmp_all_onsets_naive <- NULL
tmp_all_infections_naive <- NULL

for(i in 1:repeats) {
  if(i %% 10 == 0) print(i)
  dat1_subset <- dat1 %>% sample_frac(1,replace=TRUE) 
  dat1_subset <- dat1
  #backwards_delays_this_res <- generate_gamma_distributions_backward(25, dat1, tmax,min_date=min(incidence_dat_long$confirm_date))
  backwards_delays_empirical <- generate_empirical_distributions_backward(25, dat1, tmax,min_date=as.Date("2020-01-16"))#min(linelist_dat$report_date))
  
  ## Precompute the sampling probabilities for each day
  all_probs_empirical <- NULL
  unique_dates <- unique(backwards_delays_empirical$date_confirmation)
  for(i in seq_along(unique_dates)){
    all_probs_empirical[[i]] <- backwards_delays_empirical %>% filter(date_confirmation==unique_dates[i]) %>% pull(prob)
  }
  precomputed_probs_empirical <- tibble(probs=all_probs_empirical,date_confirmation=unique_dates) %>% 
    left_join(backwards_delays_empirical %>% select(date_confirmation,max_delay) %>% unique(), by="date_confirmation")
  
  
  confirm_dat_mod_tmp <- confirmed_cases_date %>% rename(date_confirmation=dates) %>%
    filter(n > 0 & Location != "Hubei") %>%
    left_join(precomputed_probs_empirical,by="date_confirmation")
  
  tmp <- confirm_dat_mod_tmp %>%
    uncount(n) %>%
    mutate(id=1:n()) %>%
    group_by(id) %>%
    mutate(
      delay = sample(seq(0, max_delay,by=1),size=n(),
                     prob=unlist(probs)),
      onset_date = date - delay)
  
  tmp_onsets <- tmp %>%
    group_by(onset_date, Location) %>%
    tally() %>%
    rename(onsets=n) %>%
    mutate(i=i)
  
  
  tmp_infections <- tmp_onsets %>%
    uncount(onsets) %>%
    mutate(id=1:n()) %>%
    group_by(id) %>%
    mutate( incu_period = floor(incu_mean),
            infection_date = onset_date - incu_period) %>%
    group_by(infection_date, Location) %>%
    tally() %>%
    rename(infections=n) %>%
    mutate(i = i)
  
  tmp_all_onsets <- bind_rows(tmp_all_onsets, tmp_onsets)
  tmp_all_infections <- bind_rows(tmp_all_infections, tmp_infections)
}


augmented_onsets <- tmp_all_onsets %>% 
  group_by(onset_date,Location) %>%
  rename(old=onsets,onsets=onsets) %>%
  summarise(lower_quant=quantile(onsets, c(0.025)),
            mid_quant1=quantile(onsets, c(0.25)),
            median=median(onsets),
            mean=mean(onsets),
            mid_quant2=quantile(onsets, c(0.75)),
            upper_quant = quantile(onsets, c(0.975))) %>%
  mutate(ver="Symptom onsets") %>%
  rename(date=onset_date)

augmented_infections <- tmp_all_infections %>% 
  group_by(infection_date,Location) %>%
  summarise(lower_quant=quantile(infections, c(0.025)),
            mid_quant1=quantile(infections, c(0.25)),
            median=median(infections),
            mean=mean(infections),
            mid_quant2=quantile(infections, c(0.75)),
            upper_quant = quantile(infections, c(0.975))) %>%
  mutate(ver="Infections") %>%
  rename(date=infection_date)
all_augmented <- bind_rows(augmented_onsets, augmented_infections)
all_augmented <- all_augmented %>% ungroup() %>% mutate(date = as.Date(date, origin="2019-11-01"))
p_backcalc_overall <- all_augmented %>% 
  ggplot() +
  geom_bar(data=confirmed_cases_date %>% filter(n > 0),aes(x=dates,y=n),stat="identity",fill="grey60",col="grey20") +
  geom_line(data=onsets %>% filter(ver=="Overall mean"), aes(x=date_onset,y=n),col="#009e73") +
  geom_line(data=infections%>% filter(ver=="Overall mean"), aes(x=infection_date,y=n),col="red") +
  geom_line(aes(x=date, y= median,col=ver)) +
  geom_ribbon(aes(x=date, ymin= lower_quant,ymax=upper_quant,fill=ver),alpha=0.25) +
  scale_fill_manual(values=c("#E69F00","#56B4E9")) +
  facet_wrap(~Location,scales="free_y",ncol=3) +
  geom_vline(xintercept=as.Date("2020-01-23")) +
  theme_bw() +
  theme(legend.position = "bottom")
png("figures/real_augmented_shift.png",width=8,height=8,res=300,units="in")
p_backcalc_overall
dev.off()


## Check that forward-delays from the augmented infection curve make sense
onsets_forward <- infections %>% 
  filter(ver == "Overall mean") %>% 
  ungroup() %>%
  uncount(n) %>% 
  mutate(incu_period = floor(rlnorm(n(), incu_par1, incu_par2)),
         onset_date = infection_date + incu_period) %>%
  rename(date=onset_date) %>%
  select(Location, date) %>%
  group_by(Location, date) %>%
  tally() %>%
  mutate(date=lubridate::round_date(date))

confirmations_forward <- onsets_forward %>%
  left_join(forward_delay_dists %>% select(date, gamma_scale_forward,gamma_shape_forward)) %>%
  uncount(n) %>%
  mutate(confirm_delay = rdgamma(n(), gamma_shape_forward,scale=gamma_scale_forward),
         confirm_date = date + confirm_delay) %>%
  group_by(Location, confirm_date) %>%
  tally()

p_forward_sim_from_augmented_byoverallmean <- ggplot(confirmations_forward) +
  geom_bar(data=confirmed_cases_date %>% filter(n > 0),aes(x=dates,y=n),stat="identity",fill="grey60",col="grey20") +
  geom_line(aes(x=confirm_date,y=n),col="red") +
  facet_wrap(~Location,scales="free_y")
png("figures/real_forward_augment_check.png",width=8,height=8,res=300,units="in")
p_forward_sim_from_augmented_byoverallmean
dev.off()

all_dat_expand <- expand_grid()
