#####
## NOTE - NEED TO RUN SIMPLER_METHOD.R FIRST
library(pals)
library(patchwork)

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
  scale_y_continuous(expand=c(0,0),limits=c(0,0.00002*1000000),breaks=seq(0,0.00002*1000000,by=0.000005*1000000)) +
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

mt <- read_csv("./data/all_prevalence_estimates.csv",guess_max = Inf)
prev_all <- mt %>% select(prevalence_o, date, scenario,origin_city) %>% distinct() %>% mutate(is_wuhan = ifelse(origin_city == "Wuhan",1, 0))

prev_summary <- prev_all %>% 
  group_by(date, scenario,is_wuhan) %>% 
  summarize(prev_average=mean(prevalence_o)) %>%
  rename(Scenario=scenario)

scenario_key <- c("Scenario 1"="Scenario 3", 
                  "Scenario 2"="Scenario 1*", 
                  "Scenario 3"="Scenario 4", 
                  "Scenario 4"="Scenario 5", 
                  "Scenario 5"="Scenario 6", 
                  "Scenario 6"="Scenario 7", 
                  "Scenario 7"="Scenario 8", 
                  "Scenario 8"="Scenario 9", 
                  "Scenario 9"="Scenario 10", 
                  "Scenario 10" = "Scenario 2")
prev_summary$Scenario <- scenario_key[prev_summary$Scenario]
prev_summary$Scenario <- factor(prev_summary$Scenario, levels=c("Scenario 1*", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5", 
                                                                "Scenario 6", "Scenario 7", "Scenario 8", "Scenario 9", "Scenario 10"))


prev_summary1 <- prev_summary %>% 
  pivot_wider(values_from=prev_average,names_from = is_wuhan) %>%
  rename(wuhan=`1`,non_wuhan=`0`) %>% 
  group_by(Scenario) %>%
  mutate(wuhan_scaled = wuhan/max(wuhan)) %>%
  mutate(non_wuhan_scaled = non_wuhan/max(wuhan))

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
  filter(Scenario %in% c("Scenario 1*","Scenario 2","Scenario 4","Scenario 5")) %>%
  mutate(Scenario = as.character(Scenario)) %>%
  mutate(Scenario = ifelse(Scenario %in% c("Scenario 1*","Scenario 3"," Scenario 6", "Scenario 7","Scenario 8","Scenario 9"), 
                           "Scenarios 1*, 3, 6-9",Scenario)) %>%
  mutate(Scenario = factor(Scenario, levels=c("Scenarios 1*, 3, 6-9","Scenario 2","Scenario 4","Scenario 5"))) %>%
  ggplot() + 
  geom_line(aes(x=date,y=wuhan_scaled,col=Scenario)) +
  #geom_jitter(aes(x=date,y=prev_average,col=Scenario)) +
  scale_x_date(limits=c(date_min, date_max+5),labels=seq(date_min, date_max+5,by="7 days"),breaks=seq(date_min, date_max+5,by="7 days")) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,1,by=0.1)) +
  scale_color_manual(values=fill_cols[c(1,2,4,5)])+
  export_theme +
  ylab("Prevalence indicator") +
  xlab("Date") +
  theme(legend.position=c(0.2,0.7),
        legend.title=element_blank(),
        plot.margin = margin(0,0,0,0, "cm"))+
  ggtitle("Wuhan") +
  labs(tag="C")

pD <- prev_summary1 %>% 
  ggplot() + geom_line(aes(x=date,y=non_wuhan_scaled,col=Scenario))+
  scale_x_date(limits=c(date_min, date_max+5),labels=seq(date_min, date_max+5,by="7 days"),breaks=seq(date_min, date_max+5,by="7 days")) +
  scale_y_continuous(expand=c(0,0),limits=c(0,0.25),breaks=seq(0,0.25,by=0.02)) +
  scale_color_manual(values=fill_cols)+
  xlab("Date") +
  ylab("Prevalence indicator") +
  ggtitle("China (excluding Wuhan)") +
  export_theme +
  theme(legend.position=c(0.2,0.7),
        legend.title=element_blank(),
        plot.margin = margin(0,0,0,0, "cm"))+
  labs(tag="D")
pdf("figures/Fig1_test2.pdf",height=3,width=8)
pC | pD
dev.off()


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

prev_summary <- prev_all %>% 
  group_by(date, scenario,origin_city) %>% 
  summarize(prev_average=mean(prevalence_o)) %>%
  rename(Scenario=scenario)

scenario_key <- c("Scenario 1"="Scenario 3", 
                  "Scenario 2"="Scenario 1*", 
                  "Scenario 3"="Scenario 4", 
                  "Scenario 4"="Scenario 5", 
                  "Scenario 5"="Scenario 6", 
                  "Scenario 6"="Scenario 7", 
                  "Scenario 7"="Scenario 8", 
                  "Scenario 8"="Scenario 9", 
                  "Scenario 9"="Scenario 10", 
                  "Scenario 10" = "Scenario 2")
prev_summary$Scenario <- scenario_key[prev_summary$Scenario]
prev_summary$Scenario <- factor(prev_summary$Scenario, levels=c("Scenario 1*", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5", 
                                                                "Scenario 6", "Scenario 7", "Scenario 8", "Scenario 9", "Scenario 10"))


max_prevs <- prev_summary %>% filter(origin_city == "Wuhan") %>% group_by(Scenario) %>%
  summarize(max_prev_wuhan=max(prev_average))


date_min <- as.Date("2019-12-01")
pS3 <- prev_summary %>% 
  left_join(max_prevs) %>%
  mutate(prev_average_scaled = prev_average/max_prev_wuhan) %>%
  ggplot() + 
  geom_line(aes(x=date,y=prev_average_scaled,col=Scenario),show.legend = F) +
  #geom_jitter(aes(x=date,y=prev_average,col=Scenario)) +
  scale_x_date(limits=c(date_min, date_max+5),labels=seq(date_min, date_max+5,by="14 days"),breaks=seq(date_min, date_max+5,by="14 days")) +
  #scale_y_continuous(expand=c(0,0),limits=c(0,0.005),breaks=seq(0,0.005,by=0.001)) +
  export_theme +
  scale_color_manual(values=fill_cols)+
  ylab("Prevalence indicator") +
  geom_vline(xintercept=as.Date("2020-01-23"),linetype="dashed") +
  xlab("") +
  theme(legend.position=c(0.8,0.05),
        legend.title=element_blank())+
  facet_wrap(~origin_city,scales="free_y", ncol=4)
pS3
pdf("figures/FigS3.pdf",height=7*0.9,width=8*0.9)
pS3
dev.off()
png("figures/FigS3.png",height=7,width=8,res=300,units="in")
pS3
dev.off()
