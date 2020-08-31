##### run additional Figures
library(stats)
library(tidyverse)
library(patchwork)
library(RColorBrewer)
library(tidyverse)
library(rio)
library(pals)
library(patchwork)

source("./code/functions/simpler_method_fun.R")
scenario_key <- readxl::read_excel("./data/scenario_key.xlsx")
asc_nonhubei_v_hubei <- 5.1
assignment <- "by_cases"
############################
## read in confirmed case data
############################
confirmed_cases<-read.csv("./data/midas_data_final.csv",stringsAsFactors=FALSE) %>% as_tibble()
# subset to only provinces used in our analysis
provinces<-c('Hubei','Beijing','Shanghai','Guangdong','Henan',
             'Tianjin','Zhejiang','Hunan','Shaanxi','Jiangsu','Chongqing',
             'Jiangxi','Sichuan','Anhui','Fujian') # 15
confirmed_cases_final<-confirmed_cases[confirmed_cases$province_raw%in%provinces,] # 124 unique dates

# match date indices to actual dates
dates_and_date=tibble(dates=seq(as.Date('2020-03-02')-122,as.Date('2020-03-02'),by="day")) %>% 
  mutate(date=(1:n()-1))
confirmed_cases_date <- left_join( confirmed_cases_final,dates_and_date, by="date" )

## Remove rows outside of our focal period
confirmed_cases_date <- confirmed_cases_date %>% filter(!is.na(dates))

# remove outliers in Wuhan
which(confirmed_cases_date$province_raw=="Hubei" & confirmed_cases_date$n>4000 ) -> which_replace
confirmed_cases_date$n[ c((which_replace[1]-1),(which_replace[2]+1)) ] -> put_instead
confirmed_cases_date$n[ which_replace ] <- put_instead

## df_city_pop, data frame with population of each City
load(file="./out/df_city_pop.Rdata")

# Possible ways to distribute cases across cities
prov_city_adjust <- get_prov_city_adjust(file="./out/frac_popn_city.Rdata" )

# backculation: shift by mean reporting delays & incubation period
all_incidence_province <- shift_2_delays(confirmed_cases_date,incubation_period=-5,delay=-7)

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
  left_join(df_city_pop,by=c("city"="asciiname")) %>% 
  mutate(n_infected_caladj=n_infected_caladj/population) %>% select(-population)

prov_inc_prev_cali <- comp_travel_rel_prev(city_n_inf_caladj_den, rel_dur = 5) 
prov_inc_prev_cali_raw <- comp_travel_rel_prev(city_n_inf_caladj, rel_dur = 5) 

## Plot example province
date_min <- as.Date("2020-01-01")
date_max <- as.Date('2020-03-02')
use_prov <- "Guangdong"
use_city <- "Guangzhou"
pA <- all_incidence_province %>% 
  filter(province_raw == use_prov,
         dates >= date_min & dates <= date_max) %>%
  ggplot() +
  geom_bar(data=confirmed_cases_date %>% 
             filter(province_raw==use_prov,
                    dates >= date_min & dates <= date_max),
           aes(x=dates,y=n),stat="identity",col="grey5", size=0.5,fill="grey50") +
  geom_ribbon(aes(x=dates,ymax=n_onset,ymin=0),alpha=0.2,fill="#56B4E9") +
  geom_line(aes(x=dates,y=n_onset),col="#56B4E9",size=0.75) +
  geom_ribbon(aes(x=dates,ymax=n_infected,ymin=0),alpha=0.2,fill="#E69F00") +
  geom_line(aes(x=dates,y=n_infected),col="#E69F00",size=0.75) +
  scale_y_continuous(expand=c(0,0),limits=c(0,150),breaks=seq(0,150,by=25)) +
  scale_x_date(limits=c(date_min, date_max+5),labels=seq(date_min, date_max+5,by="7 days"),breaks=seq(date_min, date_max+5,by="7 days")) +
  xlab("Date") +
  ylab("Incidence (number of cases)") +
  export_theme+
  ggtitle("Guangdong") +
  theme(#axis.text.x=element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x=element_blank(),
        plot.margin = margin(0,0,0,0, "cm")) + 
  labs(tag="A")

pB <- prov_inc_prev_cali %>% filter(city == use_city) %>%
  ggplot() + 
  geom_line(aes(x=date,y=travel_prev*1000000),col="grey5",size=0.75) + 
  geom_ribbon(aes(x=date,ymax=travel_prev*1000000,ymin=0),fill="#009E73") + 
  geom_line(data=city_n_inf_caladj_den %>% filter(city == use_city), 
            aes(x=date,y=n_infected_caladj*1000000),col="grey5",size=0.75) +
  geom_ribbon(data=city_n_inf_caladj_den %>% filter(city == use_city),
              aes(x=date,ymax=n_infected_caladj*1000000,ymin=0),fill="#E69F00") +
  scale_x_date(limits=c(date_min, date_max+5),labels=seq(date_min, date_max+5,by="7 days"),breaks=seq(date_min, date_max+5,by="7 days")) +
  scale_y_continuous(expand=c(0,0),limits=c(0,0.000012*1000000),breaks=seq(0,0.000012*1000000,by=0.000002*1000000)) +
  ylab("Prevalence/incidence per million") +
  xlab("Date") +
  ggtitle("Guangzhou (Guangdong)") +
  export_theme +
  theme(#axis.text.x=element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x=element_blank(),
        plot.margin = margin(0,0,0,0, "cm")) + 
  labs(tag="B")

pA | pB

date_min <- as.Date("2019-12-01")

load(file="./out/all_prev_mods.Rdata")
prev_all <- all_outputs %>% select(prevalence_o, date, scenario,origin_city) %>% distinct() %>% mutate(is_wuhan = ifelse(origin_city == "Wuhan",1, 0))

prev_summary <- prev_all %>% 
  group_by(scenario) %>%
  mutate(prevalence_o = prevalence_o/max(prevalence_o)) %>%
  group_by(date, scenario,is_wuhan) %>% 
  summarize(prev_average=mean(prevalence_o)) %>%
  rename(Scenario=scenario) %>%
  filter(!(Scenario %in% c("Scenario 10","Scenario 11")))

prev_summary$Scenario <- factor(prev_summary$Scenario, levels=c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5", 
                                                                "Scenario 6", "Scenario 7", "Scenario 8", "Scenario 9"))


prev_summary1 <- prev_summary %>% 
  pivot_wider(values_from=prev_average,names_from = is_wuhan) %>%
  rename(wuhan=`1`,non_wuhan=`0`) %>% 
  group_by(Scenario) #%>%
  #mutate(wuhan_scaled = wuhan/max(wuhan)) %>%
  #mutate(non_wuhan_scaled = non_wuhan/max(wuhan))

# pC <- prev_summary %>% filter(is_wuhan ==1) %>%
#   ggplot() + 
#   geom_line(aes(x=date,y=prev_average,col=Scenario)) +
#   #geom_jitter(aes(x=date,y=prev_average,col=Scenario)) +
#   scale_x_date(limits=c(date_min, date_max+5),labels=seq(date_min, date_max+5,by="7 days"),breaks=seq(date_min, date_max+5,by="7 days")) +
#   scale_y_continuous(expand=c(0,0),limits=c(0,0.005),breaks=seq(0,0.005,by=0.001)) +
#   export_theme +
#   ylab("Prevalence (per capita)") +
#   xlab("Date") +
#   theme(legend.position=c(0.2,0.7),
#         legend.title=element_blank(),
#         plot.margin = margin(0,0,0,0, "cm"))+
#   ggtitle("Wuhan") +
#   labs(tag="C")
# 
# pD <- prev_summary %>% filter(is_wuhan ==0) %>%
#   ggplot() + geom_line(aes(x=date,y=prev_average,col=Scenario))+
#   scale_x_date(limits=c(date_min, date_max+5),labels=seq(date_min, date_max+5,by="7 days"),breaks=seq(date_min, date_max+5,by="7 days")) +
#   scale_y_continuous(expand=c(0,0),limits=c(0,0.0002),breaks=seq(0,0.0002,by=0.00005)) +
#   xlab("Date") +
#   ylab("Prevalence (per capita)") +
#   ggtitle("China (excluding Wuhan)") +
#   export_theme +
#   theme(legend.position=c(0.2,0.7),
#         legend.title=element_blank(),
#         plot.margin = margin(0,0,0,0, "cm"))+
#   labs(tag="D")


fill_cols <- as.vector(polychrome(10)[c(1,3:10)])
pC <- prev_summary1 %>% 
  filter(Scenario == "Scenario 1") %>% 
  ggplot() + 
  geom_line(aes(x=date,y=wuhan),col="grey5",size=0.75) + 
  geom_ribbon(aes(x=date,ymax=wuhan,ymin=0),fill="#009E73") + 
  scale_x_date(limits=c(date_min, date_max+5),labels=seq(date_min, date_max+5,by="7 days"),breaks=seq(date_min, date_max+5,by="7 days")) +
  scale_y_continuous(limits=c(0,1.02), expand=c(0,0),breaks=seq(0,1,by=0.1)) +
  #scale_color_manual(values=fill_cols[c(1,2,4,5)])+
  geom_vline(xintercept=as.Date("2020-01-23"),linetype="dashed") +
  export_theme +
  ylab("Prevalence indicator") +
  xlab("Date") +
  theme(legend.position=c(0.2,0.7),
        legend.title=element_blank(),
        plot.margin = margin(0,0,0,0, "cm"))+
  ggtitle("Wuhan") +
  labs(tag="C")
pD <- prev_summary1 %>% 
  filter(Scenario == "Scenario 1") %>%
  ggplot() + 
  geom_line(aes(x=date,y=non_wuhan),col="grey5",size=0.75) + 
  geom_ribbon(aes(x=date,ymax=non_wuhan,ymin=0),fill="#009E73") + 
  scale_x_date(limits=c(date_min, date_max+5),labels=seq(date_min, date_max+5,by="7 days"),breaks=seq(date_min, date_max+5,by="7 days")) +
  scale_y_continuous(expand=c(0,0),limits=c(0,0.003),breaks=seq(0,0.003,by=0.0005)) +
  #scale_color_manual(values=fill_cols)+
  geom_vline(xintercept=as.Date("2020-01-23"),linetype="dashed") +
  xlab("Date") +
  ylab("Prevalence indicator") +
  ggtitle("China (excluding Wuhan)") +
  export_theme +
  theme(legend.position=c(0.2,0.7),
        legend.title=element_blank(),
        plot.margin = margin(0,0,0,0, "cm"))+
  labs(tag="D")
#pdf("figures/Fig1_test2.pdf",height=3,width=8)
pC | pD
#dev.off()


pdf("figures/Fig1_base.pdf",height=5,width=8)
(pA | pB)/ (pC | pD)
dev.off()


#######
## Fig S2
## Plot example province
date_min <- as.Date("2020-01-01")
date_max <- as.Date('2020-03-02')
pS2 <- all_incidence_province %>% 
  filter(dates >= date_min & dates <= date_max) %>%
  ggplot() +
  geom_bar(data=confirmed_cases_date %>% 
             filter(dates >= date_min & dates <= date_max),
           aes(x=dates,y=n),stat="identity",col="grey5", size=0.1,fill="grey50") +
  geom_ribbon(aes(x=dates,ymax=n_onset,ymin=0),alpha=0.2,fill="#56B4E9") +
  geom_line(aes(x=dates,y=n_onset),col="#56B4E9",size=0.75) +
  geom_ribbon(aes(x=dates,ymax=n_infected,ymin=0),alpha=0.2,fill="#E69F00") +
  geom_line(aes(x=dates,y=n_infected),col="#E69F00",size=0.75) +
  #scale_y_continuous(expand=c(0,0)) +
  scale_x_date(limits=c(date_min, date_max+5),labels=seq(date_min, date_max+5,by="14 days"),breaks=seq(date_min, date_max+5,by="14 days")) +
  xlab("Date") +
  scale_color_manual(values=fill_cols)+
  geom_vline(xintercept=as.Date("2020-01-23"),linetype="dashed") +
  ylab("Incidence (number of cases)") +
  export_theme+
  facet_wrap(~province_raw,scales="free_y") +
  theme(#axis.text.x=element_blank(),
    #axis.ticks.x = element_blank(),
    axis.title.x=element_blank())
pdf("figures/FigS2.pdf",height=6,width=8)
pS2
dev.off()
png("figures/FigS2.png",height=6,width=8,res=300,units="in")
pS2
dev.off()

#######
## Fig S3
fill_cols <- as.vector(polychrome(10)[c(1,3:10)])

date_min <- as.Date("2019-12-01")

prev_summary <- prev_all %>% 
  group_by(date, scenario,origin_city) %>% 
  summarize(prev_average=mean(prevalence_o)) %>%
  rename(Scenario=scenario) %>%
  mutate(Scenario = as.character(Scenario)) %>%
  filter(!(Scenario %in% c("Scenario 10","Scenario 11")))

#prev_summary$Scenario <- factor(prev_summary$Scenario, levels=c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5", 
#                                                                "Scenario 6", "Scenario 7", "Scenario 8", "Scenario 9"))


max_prevs <- prev_summary %>% filter(origin_city == "Wuhan") %>% group_by(Scenario) %>%
  summarize(max_prev_wuhan=max(prev_average))


## Scenarios 2, 
pS3a <- prev_summary1 %>% 
  #filter(Scenario == "Scenario 1") %>% 
  filter(Scenario %in% c("Scenario 1","Scenario 2","Scenario 3","Scenario 4")) %>%
  #mutate(Scenario = as.character(Scenario)) %>%
  #mutate(Scenario = ifelse(Scenario %in% c("Scenario 1*","Scenario 3"," Scenario 6", "Scenario 7","Scenario 8","Scenario 9"), 
  #                         "Scenarios 1*, 3, 6-9",Scenario)) %>%
  #mutate(Scenario = factor(Scenario, levels=c("Scenarios 1*, 3, 6-9","Scenario 2","Scenario 4","Scenario 5"))) %>%
  ggplot() + 
  geom_line(aes(x=date,y=wuhan,col=Scenario)) +
  #geom_jitter(aes(x=date,y=prev_average,col=Scenario)) +
  scale_x_date(limits=c(date_min, date_max+5),labels=seq(date_min, date_max+5,by="7 days"),breaks=seq(date_min, date_max+5,by="7 days")) +
  scale_y_continuous(limits=c(0,1.02), expand=c(0,0),breaks=seq(0,1,by=0.1)) +
  scale_color_manual(values=fill_cols)+
  export_theme +
  ylab("Prevalence indicator") +
  xlab("Date") +
  theme(legend.position=c(0.2,0.7),
        legend.title=element_blank(),
        plot.margin = margin(0,0,0,0, "cm"))+
  ggtitle("Wuhan") +
  labs(tag="A")
pS3a
pS3b <- prev_summary1 %>% 
  #filter(Scenario == "Scenario 1") %>%
  ggplot() + geom_line(aes(x=date,y=non_wuhan,col=Scenario))+
  scale_x_date(limits=c(date_min, date_max+5),labels=seq(date_min, date_max+5,by="7 days"),breaks=seq(date_min, date_max+5,by="7 days")) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.025),breaks=seq(0,0.025,by=0.005)) +
  scale_color_manual(values=fill_cols)+
  xlab("Date") +
  ylab("Prevalence indicator") +
  ggtitle("China (excluding Wuhan)") +
  export_theme +
  theme(legend.position=c(0.2,0.7),
        legend.title=element_blank(),
        plot.margin = margin(0,0,0,0, "cm"))+
  labs(tag="B")


date_min <- as.Date("2019-12-01")
pS3c <- prev_summary %>% 
  left_join(max_prevs) %>%
  mutate(prev_average_scaled = prev_average/max_prev_wuhan) %>%
  ggplot() + 
  geom_line(aes(x=date,y=prev_average_scaled,col=Scenario),show.legend = FALSE) +
  #geom_jitter(aes(x=date,y=prev_average,col=Scenario)) +
  scale_x_date(limits=c(date_min, date_max+5),labels=seq(date_min, date_max+5,by="14 days"),breaks=seq(date_min, date_max+5,by="14 days")) +
  #scale_y_continuous(expand=c(0,0),limits=c(0,0.005),breaks=seq(0,0.005,by=0.001)) +
  export_theme +
  scale_color_manual(values=fill_cols)+
  ylab("Prevalence indicator") +
  geom_vline(xintercept=as.Date("2020-01-23"),linetype="dashed") +
  xlab("") +
  theme(legend.position=c(0.8,0.05),
        legend.direction = "horizontal",
        legend.title=element_blank())+
  facet_wrap(~origin_city,scales="free_y", ncol=5)+
  labs(tag="C")

((pS3a | pS3b) / pS3c) + plot_layout(heights=c(1,1.5))

pdf("figures/FigS3.pdf",height=8,width=8)
((pS3a | pS3b) / pS3c) + plot_layout(heights=c(1,1.5))
dev.off()
png("figures/FigS3.png",height=8,width=8,res=300,units="in")
((pS3a | pS3b) / pS3c) + plot_layout(heights=c(1,1.5))
dev.off()
