# Source: https://github.com/timktsang/covid19_casedef/blob/master/2020_03_10_figure3.R (timktsang)
library(ggplot2)
library(tidyverse)
library(data.table)
library(Rcpp)

setwd("~/Documents/GitHub/africa_export/")## Read in MCMC outputs

load("data/tsang2020/image_0.729555189609528.Rdata")
sourceCpp("data/tsang2020/nov.cpp")
## Data used by Tsang et al.
## Append some blank rows for longer predictions
data1 <- rbind(data1, matrix(0, ncol=4,nrow=14))

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
  fit_tmp <- gg1[[2]]
  fit_tmp <- as.data.frame(fit_tmp[,c(1,2,3)])
  #pred_tmp <- as.data.frame(pred_tmp[,c(5,10,15)])
  colnames(fit_tmp) <- c("Wuhan","Hubei","China (non-Hubei)")
  fit_tmp$samp <- i
  fit_tmp$date <- dates
  all_fits[[i]] <- fit_tmp
  
  pred_tmp <- gg1[[4]]
  pred_tmp <- as.data.frame(pred_tmp[,c(5,10,15)])
  colnames(pred_tmp) <- c("Wuhan","Hubei","China (non-Hubei)")
  pred_tmp$samp <- i
  pred_tmp$date <- dates
  all_preds[[i]] <- pred_tmp
}
all_preds <- do.call("rbind", all_preds)
all_fits <- do.call("rbind", all_fits)
quants <- all_preds %>% as_tibble() %>% pivot_longer(-c(samp, date)) %>%
  group_by(name,date) %>%
  summarize(lower=quantile(value,0.025),
            median=quantile(value,0.5),
            upper=quantile(value,0.975),
            mean=mean(value))
quants_fit <- all_fits %>% as_tibble() %>% pivot_longer(-c(samp, date)) %>%
  group_by(name,date) %>%
  summarize(lower=quantile(value,0.025),
            median=quantile(value,0.5),
            upper=quantile(value,0.975),
            mean=mean(value))

## Model predicted incidence
gg <- pred(data1,z1[,1])
pred_dat <- gg[[4]]
## Take posterior mean assuming version 5 reporting throughout
pred_final <- as.data.frame(pred_dat[,c(5,10,15)])
colnames(pred_final) <- c("Wuhan","Hubei","China (non-Hubei)")
pred_final$date <- seq(as.Date("2019-12-02"),as.Date("2020-03-05"),by="1 day")
pred_final <- pred_final %>% pivot_longer(-date)

## Reshape data for plot
data2 <- as_tibble(data1)
colnames(data2) <- c("Wuhan","Hubei","China (non-Hubei)", "Total")
data2 <- data2[,1:3]
data2$date <- seq(as.Date("2019-12-02"),as.Date("2020-03-05"),by="1 day")
data1_melted <- data2 %>% pivot_longer(-date)

data_tot <- data1[,4] %>% as_tibble() %>% rename("Total" = "value")
data_tot$date <- seq(as.Date("2019-12-02"),as.Date("2020-03-05"),by="1 day")

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
  #scale_y_log10() +
  xlab("Date of onset") +
  ylab("Onsets") +
  facet_wrap(~name,ncol=1,scales="free_y")

comb_dat <- data1_melted %>% rename(observed=value) %>%
  left_join(quants %>% left_join(pred_final %>% rename("posterior_median"=value))) %>%
  mutate(median_asc = observed/median,
         tsang_median_asc=observed/posterior_median,
         tsang_mean_asc=observed/mean)
comb_dat %>% ggplot() +
  geom_line(aes(x=date,y=median_asc,col=name))



## Store for later analysis
## write_csv(pred_final, "~/Documents/GitHub/africa_export/data/tsang_onset_predictions.csv")

data <- read.csv("./data/Epidemic_curve_China.csv")
data <- data[-82,]
data$dateid <- 1:nrow(data)
data_melted <- data %>% 
  mutate(date=as.Date(as.character(date), origin="2019-01-01",format="%d/%m/%Y"))%>%
  select(-c("china.asymptomatic","china.clindiag","china.confirmed",
            "china.onset","china.suspected","china.total","china.reported")) %>%
  pivot_longer(-c(date,dateid)) %>%
  mutate(region="Wuhan",
         region=ifelse(name %like% "hubeiexclwuhan","Hubei",region),
         region=ifelse(name %like% "chinaexclhubei","China (non-Hubei)",region),
         region=ifelse(name == "china.reported","china_overall",region)
  ) %>%
  mutate(name1="total",
         name1=ifelse(name %like% "asymptomatic", "asymptomatic",name1),
         name1=ifelse(name %like% "clindiag", "clindiag",name1),
         name1=ifelse(name %like% "confirmed", "confirmed",name1),
         name1=ifelse(name %like% "onset", "onset",name1),
         name1=ifelse(name %like% "reported", "reported",name1),
         name1=ifelse(name %like% "suspected", "suspected",name1),
         name1=ifelse(name == "china.reported","china_overall",name1)
  )
data_melted %>% ggplot() + geom_line(aes(x=date,y=value,col=name1)) + facet_wrap(~region,ncol=1,scales="free_y")

data1 <- data[,c("date","dateid","wuhan.confirmed","hubeiexclwuhan.confirmed","chinaexclhubei.confirmed")]
data1$date <- as.Date(as.character(data1$date),origin="2019-01-01",format="%d/%m/%Y")
#1/15 1/18 1/22 1/27 2/4 2/18
# 45   48   52   57   65  79

test <- read.csv("./data/pred_f3.csv")

## Name predictions and plot
colnames(test) <- c("wuhan_v1","wuhan_v1b","wuhan_v2","wuhan_v4","wuhan_v5",
                    "hubei_v1","hubei_v1b","hubei_v2","hubei_v4","hubei_v5",
                    "china_v1","china_v1b","china_v2","china_v4","china_v5"
)
## Melt and relabel location and case definition for plotting
predicted_dat <- as_tibble(test) %>% 
  mutate(t = 1:n()) %>% 
  mutate(date=data1$date) %>%
  pivot_longer(-c(t,date)) %>%
  mutate(loc="Wuhan",
         loc=ifelse(name %like% "hubei", "Hubei", loc),
         loc=ifelse(name %like% "china","China (non-Hubei)",loc)) %>%
  mutate(case_def = "V1",
         case_def=ifelse(name %like% "v2", "V2", case_def),
         case_def=ifelse(name %like% "v4", "V4", case_def),
         case_def=ifelse(name %like% "v5", "V5", case_def)
  )

## Compare Tsang predicted onsets to observed
predicted_dat %>% ggplot() + 
  geom_bar(data=data_melted %>% filter(name1 == "confirmed") %>% rename(loc=region),
           aes(x=date, y=value),stat="identity",fill="grey70") +
  geom_line(aes(x=date,y=value,col=case_def)) + 
  scale_x_date(breaks="7 days") +
  theme_bw() +
  facet_wrap(~loc, ncol=1,scales="free_y")

## Compare ascertainment rate of onsets under case version 5
dat_jh <- data_melted %>% filter(name1 == "confirmed") %>% select(date, value, region) %>%
  rename(observed=value, loc=region) %>%
  left_join(predicted_dat %>% filter(case_def == "V5") %>% select(-c(name,t))) %>%
  mutate(asc_rate = observed/value) 
dat_jh %>% ggplot() + geom_line(aes(x=date,y=asc_rate,col=loc))


## Save table of ascertainment rates
dates <- as.Date(as.character(data$date),origin="01/01/2019",format="%d/%m/%Y")
start_date <- c(min(dates), as.Date(c("2020-01-15","2020-01-25","2020-01-28","2020-02-01")))
end_date <- c(as.Date(c("2020-01-24","2020-01-27","2020-01-31","2020-02-05")),max(dates))
date_cases_both_2_summary %>% mutate(start_date=start_date,end_date=end_date)

enumerated_asc_rates <- tibble(date=seq(as.Date("2019-11-01"), as.Date("2020-03-04"),by="1 day")) %>%
  mutate(indicator=5,
         indicator=ifelse(date < start_date[5], 4,indicator),
         indicator=ifelse(date < start_date[4], 3,indicator),
         indicator=ifelse(date < start_date[3], 2,indicator),
         indicator=ifelse(date < start_date[2], 1,indicator)
  ) %>%
  left_join(date_cases_both_2_summary)

write_csv(enumerated_asc_rates, "data/tsang_ascertainment_rates2.csv")


####################################
## SCENARIO 10
####################################
## All confirmed cases
confirmed_cases<-read.csv("./data/midas_data_final.csv",stringsAsFactors=FALSE) %>% as_tibble()
# match date indices to actual dates
dates_and_date=tibble(dates=seq(as.Date('2020-03-02')-122,as.Date('2020-03-02'),by="day")) %>% 
  mutate(date=(1:n()-1))
confirmed_cases_final <- confirmed_cases
confirmed_cases_date <- left_join( confirmed_cases_final,dates_and_date, by="date" )
## Back-shift to get infection and onset incidence
all_incidence_province <- shift_2_delays(confirmed_cases_date,incubation_period=-5,delay=-7)

## Group by periods of time
dates <- as.Date(as.character(data$date),origin="01/01/2019",format="%d/%m/%Y")
start_date <- as.Date(c("2020-01-15","2020-01-18","2020-01-22","2020-02-14"))
indicator_tab <- tibble(dates=seq(as.Date("2019-11-01"), as.Date("2020-03-04"),by="1 day")) %>%
  mutate(indicator=5,
         indicator=ifelse(dates < start_date[4], 4,indicator),
         indicator=ifelse(dates < start_date[3], 3,indicator),
         indicator=ifelse(dates < start_date[2], 2,indicator),
         indicator=ifelse(dates < start_date[1], 1,indicator)
  )
#indicator_tab <- tibble(dates=seq(as.Date("2019-11-01"), as.Date("2020-03-04"),by="1 day")) %>%
#  mutate(indicator=2,
#         indicator=ifelse(dates < "2020-01-23", 1,indicator)
#  )

## Get non-Hubei provinces
china_onsets <- all_incidence_province %>% 
  select(dates, province_raw, n_onset) %>% 
  filter(province_raw != "Hubei") %>%
  left_join(indicator_tab)

## Sum onsets in each time period
all_onsets <- china_onsets %>% 
  group_by(indicator) %>% 
  summarize(total=sum(n_onset))

## Find proportion of cases attributed to each province in
## each window of time
prop_mod <- china_onsets %>% left_join(all_onsets) %>% 
  left_join(indicator_tab) %>%
  group_by(indicator,province_raw) %>%
  summarize(n_tot=sum(n_onset)) %>%
  left_join(all_onsets) %>%
  mutate(n_prop=n_tot/total) %>%
  drop_na()

prop_mod %>% ggplot() + 
  geom_line(aes(x=indicator,y=n_prop,col=province_raw)) + 
  facet_wrap(~province_raw)

## Predictions from Tsang et al.
tsang_predictions_raw <- read_csv("data/tsang_onset_predictions.csv")

## Apportion predicted cases to provinces proportional to fractional share in each period of time
tsang_predictions <- tsang_predictions_raw %>% 
  rename(dates=date) %>% 
  left_join(indicator_tab) %>% 
  left_join(prop_mod) %>% 
  mutate(n_predict = n_prop*China)

## Filter by provinces we are interested in
provinces<-c('Hubei','Beijing','Shanghai','Guangdong','Henan',
             'Tianjin','Zhejiang','Hunan','Shaanxi','Jiangsu','Chongqing',
             'Jiangxi','Sichuan','Anhui','Fujian')

tsang_predictions <- tsang_predictions %>% filter(province_raw %in% provinces)

final_dat <- tsang_predictions_raw %>% 
  select(Wuhan, date) %>% 
  rename(n_predict=Wuhan, dates=date) %>%
  mutate(province_raw="Hubei") %>%
  bind_rows(tsang_predictions %>% select(n_predict, dates, province_raw)) %>%
  rename(n=n_predict) %>%
  mutate(date_full=dates) %>%
  group_by(province_raw) %>%
  mutate(date=seq(0,n()-1, by=1)) %>% 
  ungroup()

final_dat %>% ggplot() +
  geom_line(aes(x=dates,y=n)) + 
  geom_line(data=china_onsets %>% filter(province_raw %in% provinces), aes(x=dates,y=n_onset),col="red") +
  facet_wrap(~province_raw,scales="free_y")

write_csv(final_dat,"data/tsang_predicted_onsets_byprovince.csv")

