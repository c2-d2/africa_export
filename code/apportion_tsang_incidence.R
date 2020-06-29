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
indicator_tab <- tibble(dates=seq(as.Date("2019-11-01"), as.Date("2020-03-04"),by="1 day")) %>%
  mutate(indicator=2,
         indicator=ifelse(dates < "2020-01-23", 1,indicator)
  )

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
