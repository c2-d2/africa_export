## TITLE: AllFigures
## Description: Generating results figures
## Author: Tigist Menkir (Center for Communicable Disease Dynamics, Harvard T.H. Chan School of Public Health)
## Date: 9 May 2020

library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
library(stringr)
library(ggplot2)
library(data.table)
library(formattable)
library(dplyr)
library(tidyr)
library(ggthemes)
library(ggpubr)
library(stringr)
library(pals)
library(lubridate)

# plot theme, courtesy of James Hay
export_theme <- theme_tufte() + 
  theme(
    ## Axis text and titles
    axis.text.x = element_text(size=8,angle = 45, hjust = 1,family="sans"),
    axis.text.y=element_text(size=8,family="sans"),
    axis.title.x=element_text(size=10,family="sans",vjust=-1),
    axis.title.y=element_text(size=10,family="sans"),
    ## Axis lines
    axis.line = element_line(colour="black"),
    axis.ticks = element_line(),
    ## Legends
    legend.title=element_text(size=10,family="sans",face="italic"),
    legend.text=element_text(size=10,family="sans"),
    legend.key.size= unit(0.2, "cm"),
    legend.margin = margin(0,0,0,0, "cm"),
    ## Strips for facet_wrap
    strip.text=element_text(size=8,family="sans"),
    strip.background=element_rect(fill="#f0f0f0"))

######## Map

# Map code based on: https://geocompr.github.io/geocompkg/articles/solutions08.html
dest_countries_africa_list<-c('Mauritania','Mauritius','South Africa','Kenya','Egypt','Ethiopia','Morocco',
                              'Algeria','Nigeria','Ghana','Tanzania','Senegal','Guinea',
                              'Zimbabwe','Congo (Kinshasa)','Sudan','Angola','Zambia',
                              'Gabon','Madagascar','Equatorial Guinea','Tunisia',
                              'Uganda','Mozambique','Seychelles','Cote D\'Ivoire')
worldbank_df$name<-str_replace_all(worldbank_df$name,'Democratic Republic of the Congo',
                                   'Congo (Kinshasa)')
worldbank_df$name<-str_replace_all(worldbank_df$name,'Cote d\'Ivoire',
                                   'Cote D\'Ivoire')
map_data<-worldbank_df%>%
  subset(name%in%dest_countries_africa_list)
colnames(map_data)[1]<-"Destination Country"

# read in exports table for African destination countries, under the intermediate prev scenario
exports_table_africa<-read.csv("./out/risk_all_cities_africa_table_Intermediate.csv")
colnames(exports_table_africa)[1]<-"Destination Country"
colnames(exports_table_africa)[2]<-"Number Exported"

# merge map data & exports table
map_data_final<-merge(map_data,exports_table_africa,by="Destination Country")
# filter for destination country & number exported columns
africa_risk = world %>% 
  filter(continent == "Africa", !is.na(iso_a2)) %>% 
  left_join(map_data_final, by = "iso_a2") %>% 
  dplyr::select(`Destination Country`, `Number Exported`) %>% 
  st_transform("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25")

# plot map
map<-ggplot(data = africa_risk) +
  geom_sf(aes(fill = `Number Exported`)) +
  scale_fill_viridis_c(option="plasma",trans = "sqrt",na.value="white",
                       breaks = c(0.5,2.5,5.5,10),
                       name="Number of imported cases",direction=-1)+
  theme_map()+
  theme(legend.title=element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 10))

ggsave("./figures/map_exports_africa_countries.pdf")

######## Line Plot
## estimated number of cases exported from all cities in China to destination countries
## estimated number of cases exported from all cities in China to destination countries
dest_countries_africa_list<-c('Mauritius','Mauritania','South Africa','Kenya','Egypt','Ethiopia','Morocco',
                              'Algeria','Nigeria','Ghana','Tanzania','Senegal','Guinea',
                              'Zimbabwe','Congo (Kinshasa)','Sudan','Angola','Zambia',
                              'Gabon','Madagascar','Equatorial Guinea','Tunisia',
                              'Uganda','Mozambique','Seychelles','Cote D\'Ivoire')

# read in combined prevalence & flight data for all origin cities, under the Intermediate prev scenario
flights_prev_combined_ALL<-read.csv("./out/prev_flight_all_cities_FINAL_2_Intermediate.csv")

risk_all_cities_africa_line<-flights_prev_combined_ALL%>%
  mutate(prev_vol_product=((daily_prevalence*daily_volume)))%>%
  group_by(destination_country,origin_city,date)%>%
  summarise(export_risk=sum(prev_vol_product))%>%
  mutate(risk_importation=scaling_factor*export_risk)%>%
  group_by(destination_country,date)%>% 
  summarise(num_exp_dest=sum(risk_importation))%>%
  subset(destination_country%in%dest_countries_africa_list)

## plot case counts over time
risk_all_cities_africa_line$date<-as.Date(risk_all_cities_africa_line$date)
risk_all_cities_africa_line$weeks <- cut(risk_all_cities_africa_line$date, 
                                         breaks="week")

risk_all_cities_africa_line_weekly <- risk_all_cities_africa_line %>% 
  group_by(weeks,destination_country) %>% 
  summarise(aggregated_num_exp_dest=sum(num_exp_dest,na.rm=TRUE))

# standardize aggregated_num_exp_dest #s
sum_num_exp_dest=sum(risk_all_cities_africa_line_weekly$aggregated_num_exp_dest)
risk_all_cities_africa_line_weekly$standardized_aggregated_num_exp_dest=risk_all_cities_africa_line_weekly$aggregated_num_exp_dest/sum_num_exp_dest

# replace DRC name
risk_all_cities_africa_line_weekly$destination_country<-ifelse(risk_all_cities_africa_line_weekly$destination_country=='Congo (Kinshasa)',
      "Democratic Republic of Congo",as.character(risk_all_cities_africa_line_weekly$destination_country))

# plot line plot
line_plot<-ggplot(risk_all_cities_africa_line_weekly,
                  aes(x=as.Date(weeks), y=standardized_aggregated_num_exp_dest,
                      col=reorder(destination_country,standardized_aggregated_num_exp_dest)))+
  geom_line(show.legend = F)+
  scale_colour_manual(values=as.vector(polychrome(26)))+
  scale_x_date(breaks = seq(as.Date("2019-12-08"), as.Date("2020-02-29"), by="1 week"))+
  theme(axis.text.x = element_text(size=15,angle = 45, hjust = 1,family="sans"),
        panel.grid.major = element_line(size=0.25,color="#f0f0f0"),
        panel.background =element_rect(fill="white"),
        #panel.grid.minor = element_blank(),
        #panel.background = element_blank(),
        axis.title.y=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y=element_blank(),
        axis.title.x= element_blank(),
        axis.line.x = element_line(colour="black"),
        axis.ticks.x = element_line(),
        legend.title=element_text(color = "black", size = 20,family="sans",face="italic"),
        legend.text = element_text(color = "black", size = 20,family="sans"),
        legend.position="bottom",
        legend.key.size= unit(0.8, "cm"),
        legend.margin = margin(0,0,0,0, "cm"),
        legend.key.width = unit(1,"cm"))+
  #geom_vline(xintercept=as.Date('2020-01-12'),linetype='dotted')+
  #geom_vline(xintercept=as.Date('2020-02-12'),linetype='dotted')+
  ylab("Predicted number of imported cases")+ guides(col=guide_legend(nrow=4))

line_plot$labels$colour<-"Country"
line_plot
#save(risk_all_cities_africa_line_weekly,file="risk_all_cities_africa_line_weekly.Rdata")
ggsave("./figures/line_plot.pdf",width=20*0.6,height=6*0.6)
library(tidyverse)
mt <- read_csv("./data/master_table_0630.csv",guess_max = Inf)
mt %>% mutate( fvolume_od = ifelse( is.na(fvolume_od), 0 , fvolume_od ) ) ->mt
mt %>% count(scenario)

mt %>% 
  # filter
  filter(is_africa_d==1) %>% 
  filter(scenario=="Scenario 1") %>% 
  filter(date>"2019-11-01") %>% 
  #
  mutate( exp_risk=fvolume_od*prevalence_o*alpha ) %>% 
  arrange( destination_country,date ) %>% # date, destination_country
  group_by(date, destination_country) %>% summarise(exp_risk_daily=sum(exp_risk) ) %>% ungroup() %>% 
  mutate( year=year(date),week=week(date) ) %>%
  group_by( destination_country, year, week ) %>% 
  mutate(n=n() ) %>%
  filter( n==7 ) %>% 
  mutate( exp_risk_weekly=mean(exp_risk_daily)) %>% slice(1) %>% ungroup() -> pf
#
pf %>% group_by( destination_country ) %>% 
  summarise( sum=sum(exp_risk_weekly)  ) %>% 
  arrange(desc(sum)) %>% slice(1:5) %>% pull(destination_country) -> top_5_c

pf %>% filter( year=="2020" | week=="52"  ) %>% 
  mutate( destination_country_new=ifelse(destination_country%in%top_5_c,destination_country,"other") ) %>% 
  ggplot( aes(x=date,y=exp_risk_weekly,
              color=(destination_country_new), group=destination_country ) ) +
  #geom_vline(xintercept=ymd('2020-01-01'),linetype='dotted')+
  geom_line(show.legend = F) +
  # colours from : https://davidmathlogic.com/colorblind/#%23332288-%23117733-%2344AA99-%2388CCEE-%23DDCC77-%23CC6677-%23AA4499-%23882255
  scale_color_manual(values=c("Egypt"="#332288",
                              "South Africa"="#117733",
                              "Ethiopia"="#88CCEE",
                              "Kenya"="#DDCC77",
                              "Morocco"="#CC6677",
                              "other"="lightgrey")) +
  export_theme +
  theme( axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank())

ggsave("./figures/line_plot2.pdf",width=8*0.85,height=1.8*0.85)

#
mt %>% 
  filter(is_africa_d==1) %>% 
  #filter(scenario=="Scenario 1") %>% 
  mutate( imp_number=prevalence_o*fvolume_od*alpha ) %>% 
  #
  group_by(date,scenario) %>% 
  summarise( sum_daily=sum(imp_number) ) %>% ungroup() %>% 
  arrange( scenario,desc(sum_daily) ) %>% 
  group_by(scenario) %>% 
  #normalise
  mutate( sum_daily=sum_daily/sum(sum_daily) ) %>% 
  mutate( cumsum_daily=cumsum(sum_daily) ) %>% 
  mutate( in_interval=as.numeric(cumsum_daily<=0.90) ) -> pf1
pf1 %>% 
  filter( in_interval==1 ) %>% 
  summarise( int_start=min(date),
             int_end=max(date)) -> pf_dates
pf_dates %>% mutate( s_n=n():1  ) -> pf2
#
mscale <- 12
(p <- pf %>% filter( year=="2020" | week=="52"  ) %>% 
  ggplot( aes(x=date,y=exp_risk_weekly ) ) +
  #geom_vline(xintercept=ymd('2020-01-01'),linetype='dotted')+
  geom_line(show.legend = F, col=NA) +
  geom_segment( data=pf2, aes(y=s_n/mscale-0.3,yend=s_n/mscale-0.3, x=int_start, xend=int_end) ) +
  export_theme +
  theme( axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank()))
ggsave("./figures/line_plot2_dates.pdf",width=8*0.85,height=1.8*0.85)
range_x <- ggplot_build(p)$layout$panel_scales_x[[1]]$range$range
rnage_y <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range
ggplot_build(p)$layout$coord$limits

############################
## rug plot with confirmed cases
############################
df2 %>% filter( cases!=0 ) %>% 
  filter( date>="2019-12-23",
          date<="2020-02-26") %>% 
  filter(destination_country%in%african_countries) -> obs_cases

mscale <- 17
pf %>% filter( year=="2020" | week=="52"  ) %>% 
  ggplot( ) +
  #geom_vline(xintercept=ymd('2020-01-01'),linetype='dotted')+
  geom_line(aes(x=date,y=exp_risk_weekly ),show.legend = F, col=NA) +
  geom_segment( data=pf2, aes(y=s_n/mscale-0.3,yend=s_n/mscale-0.3, x=int_start, xend=int_end) ) +
  geom_rug(data=obs_cases,aes(x=date,col=destination_country),length =unit(0.2, "npc"),show.legend = F) +
  #scale_x_continuous(limits =range_x ) + scale_y_continuous(limits =rnage_y ) +
  scale_color_manual(values=c("Egypt"="#332288",
                              "South Africa"="#117733",
                              "Ethiopia"="#88CCEE",
                              "Kenya"="#DDCC77",
                              "Morocco"="#CC6677",
                              "Algeria"="lightgrey")) +
  export_theme +
  theme( axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank())
ggsave("./figures/line_plot2_dates_abc.pdf",width=8*0.85,height=1.8*0.85)


mt %>% 
  filter(scenario=="Scenario 1") %>% 
  filter(destination_country=="Spain") %>%  # any 1 country
  filter(date>"2019-11-01") %>% 
  group_by(date) %>% summarise( pr_mean=mean(prevalence_o) ) %>% # 123
  # make it weekly
  mutate( year=year(date),week=week(date) ) %>%
  group_by( year,week ) %>% 
  arrange(date) %>% 
  mutate(n=n() ) %>%
  filter( n==7 ) %>% 
  mutate( pr_mean_week=mean(pr_mean) ) %>% 
  slice(  1  ) %>% ungroup() %>% 
  mutate(pr_mean_week=pr_mean_week/max(pr_mean_week)) -> pf_pr
#
mt %>% 
  # filter
  filter(is_africa_d==1) %>% 
  filter(scenario=="Scenario 1") %>% 
  filter(date>"2019-11-01") %>% 
  group_by(date) %>% summarise( fv_sum=sum(fvolume_od) ) %>% 
  mutate( year=year(date),week=week(date) ) %>% 
  #
  group_by( year,week ) %>% 
  arrange(date) %>% 
  mutate(n=n() ) %>%
  filter( n==7 ) %>% 
  mutate( fv_sum_week=sum(fv_sum) ) %>% 
  slice(  1  ) %>% ungroup() %>% 
  mutate(fv_sum_week=fv_sum_week/max(fv_sum_week)) -> pf_fly
#
pf_fly %>% filter( year=="2020" | week=="52"  ) %>% 
  ggplot( aes(x=date,y=fv_sum_week ) ) +
  #geom_vline(xintercept=ymd('2020-01-01'),linetype='dotted')+
  geom_line(linetype="dashed") +
  geom_line( data=filter(pf_pr,year=="2020" | week=="52" ), aes(y=pr_mean_week), 
             linetype="dotted" ) + 
  export_theme +
  theme( axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.y = element_blank())
ggsave("./figures/line_plot2_vol.pdf",width=8*0.85,height=1.05*0.85)


# proportion over time ----------------------------------------------------
mt %>% 
  # filter
  filter(is_global_d==1) %>% 
  filter(date>"2019-11-01") %>% 
  mutate( force_imp=prevalence_o*fvolume_od*alpha ) %>% 
  # 
  group_by(date,is_wuhan,scenario) %>% summarise( force_imp_day=sum(force_imp) ) %>% 
  mutate( year=year(date),week=week(date) ) %>% ungroup() %>% 
  # by scenario and week
  group_by(is_wuhan,scenario,year,week) %>% 
  arrange(date) %>% 
  mutate(n=n()) %>% 
  filter( n==7 ) %>% 
  mutate( force_imp_week=sum(force_imp_day) ) %>% 
  slice(  1  ) %>% ungroup() %>% dplyr::select(-year,-week,-force_imp_day,-n) %>% 
  #
  pivot_wider(names_from = is_wuhan, values_from = force_imp_week) %>% 
  set_names( "date", "scenario", "non_W" , "W"  ) %>% 
  mutate( tot_imp=(W+non_W),
          min_tot_imp=min(tot_imp[tot_imp!=0]),
          prop_wuhan=W/(tot_imp + min_tot_imp ) ) %>% 
  dplyr::select(-W,-non_W) -> pf_probt
# from where to start
pf_probt %>% 
  group_by( scenario ) %>% 
  mutate( prob_1 = ppois(q=1, lower.tail=F, lambda =tot_imp ) ) %>% 
  #filter(prob_1>0.01) %>% 
  ungroup() -> pf_probt
pf_probt %>% group_by(date) %>% 
  mutate( n=n() ) %>% 
  mutate( prob_W_lower=min(prop_wuhan),
          prob_W_upper=max(prop_wuhan)) -> pf_probt_rib
#
pf_probt_rib %>% ggplot( aes(x=date)  )+
  geom_ribbon( aes(ymin=prob_W_lower,ymax=prob_W_upper) , fill="#473F44",alpha=0.5 ) +
  geom_line( aes(x=date,y=prop_wuhan, color=scenario) ) +
  #geom_ribbon( aes(ymin=1-prob_W_upper,ymax=1-prob_W_lower), fill="#DDD9DC", alpha=0.7 ) +
  export_theme +
  theme( axis.title.x = element_blank())
ggsave("./figures/frac_time_plot.pdf",width=4*0.85,height=2)

# proportion import - africa ----------------------------------------------
mt %>% 
  # filter
  filter(is_africa_d==1) %>% 
  filter(date>"2019-11-01") %>% 
  mutate( force_imp=prevalence_o*fvolume_od*alpha ) %>% 
  # 
  group_by(date,is_wuhan,scenario) %>% summarise( force_imp_day=sum(force_imp) ) %>% 
  mutate( year=year(date),week=week(date) ) %>% ungroup() %>% 
  # by scenario and week
  group_by(is_wuhan,scenario,year,week) %>% 
  arrange(date) %>% 
  mutate(n=n()) %>% 
  filter( n==7 ) %>% 
  mutate( force_imp_week=sum(force_imp_day) ) %>% 
  slice(  1  ) %>% ungroup() %>% dplyr::select(-year,-week,-force_imp_day,-n) %>% 
  #
  pivot_wider(names_from = is_wuhan, values_from = force_imp_week) %>% 
  set_names( "date", "scenario", "non_W" , "W"  ) %>% 
  mutate( tot_imp=(W+non_W),
          min_tot_imp=min(tot_imp[tot_imp!=0]),
          prop_wuhan=W/(tot_imp + min_tot_imp ) ) %>% 
  dplyr::select(-W,-non_W) -> pf_probt
# from where to start
pf_probt %>% 
  group_by( scenario ) %>% 
  mutate( prob_1 = ppois(q=1, lower.tail=F, lambda =tot_imp ) ) %>% 
  filter(prob_1>0.01) %>% ungroup() -> pf_probt
pf_probt %>% group_by(date) %>% 
  mutate( n=n() ) %>% 
  mutate( prob_W_lower=min(prop_wuhan),
          prob_W_upper=max(prop_wuhan)) %>% 
  filter(n==5) -> pf_probt_rib
#
pf_probt_rib %>% ggplot( aes(x=date)  )+
  geom_ribbon( aes(ymin=prob_W_lower,ymax=prob_W_upper) , fill="#473F44",alpha=0.5 ) +
  geom_ribbon( aes(ymin=1-prob_W_upper,ymax=1-prob_W_lower), fill="#DDD9DC", alpha=0.7 ) +
  export_theme +
  theme( axis.title.x = element_blank())
ggsave("./figures/frac_time_plot_africa.pdf",width=4*0.85,height=2)






# calculate the % of all cases that occurred during Jan 12 - February 2nd 
date_range<-seq(as.Date('2020-01-12'),as.Date('2020-02-12'),by="day")

risk_all_cities_africa_date_range<-risk_all_cities_africa_line%>%
  subset(date%in%date_range)%>%
  summarise(sum_n=round(sum(num_exp_dest),1))

percent_date_range<-(sum(risk_all_cities_africa_date_range$sum_n)/sum(risk_all_cities_africa_line$num_exp_dest))*100
percent_date_range

######## Stacked bar plot

# read in prev data by city under Intermediate scenario, in order to rank cities by prevalence
prev_cities_median<-prev_all_scenarios_combined[prev_all_scenarios_combined$Scenario=="Intermediate",]
prevalence_all_cities_1<-prev_cities_i[,-c(1,126)]
colnames(prevalence_all_cities_1)<-dates_prev
prevalence_all_cities_final<-cbind(prev_cities_i$cities,prevalence_all_cities_1)
prevalence_all_cities_final<-prevalence_all_cities_final[duplicated(prevalence_all_cities_final$`prev_cities_i$cities`)==FALSE,]
colnames(prevalence_all_cities_final)[1]<-"cities"
prevalence_all_cities_final_2<-prevalence_all_cities_final[,c(1,39:122)]
# construct matrix to evaluate the max prev for each city across dates
max_prev_all<-data.frame(matrix(0,ncol=2,
                                nrow=nrow(prevalence_all_cities_final_2)))
max_prev_all[,1]<-prevalence_all_cities_final_2$cities
for (i in 1:nrow(prevalence_all_cities_final_2)){
  max_prev_i<-max(prevalence_all_cities_final_2[i,2:length(prevalence_all_cities_final_2)])
  max_prev_all[i,2]<-max_prev_i
}
colnames(max_prev_all)<-c("city","max_prev")
# rank cities by their max prev
max_prev_all<-max_prev_all[rev(order(max_prev_all$max_prev)),]
prev_order<-as.character(max_prev_all$city)

risk_all_cities_africa_chinese_cities<-prev_flight_all_cities_FINAL_2%>%
  mutate(prev_vol_product=((daily_prevalence*daily_volume)))%>%
  group_by(destination_country,origin_city,date)%>%
  summarise(export_risk=sum(prev_vol_product))%>%
  mutate(risk_importation=scaling_factor*export_risk)%>%
  group_by(destination_country,date)%>% 
  subset(destination_country%in%dest_countries_africa_list)%>%
  group_by(destination_country,origin_city)%>%
  summarise(num_exported=sum(risk_importation))%>%
  group_by(destination_country)%>%
  mutate(fraction=num_exported/sum(num_exported))

# replace Xi and DRC names
risk_all_cities_africa_chinese_cities$origin_city<-str_replace_all(risk_all_cities_africa_chinese_cities$origin_city,
                                                                   "Xi An", "Xi'an")

# order countries by number of imports
ordered_countries<-exports_table_africa[order(exports_table_africa$`Number Exported`),]
order_countries<-ordered_countries$`Destination Country`                                     
order_countries<-ifelse(order_countries=='Congo (Kinshasa)',
         "Democratic Republic of Congo",as.character(order_countries))                                

#risk_all_cities_africa_chinese_cities$destination_country<-
  #factor(risk_all_cities_africa_chinese_cities$destination_country,levels=order_countries)

risk_all_cities_africa_chinese_cities$destination_country<-
  ifelse(risk_all_cities_africa_chinese_cities$destination_country=='Congo (Kinshasa)',
         "Democratic Republic of Congo",
         as.character(risk_all_cities_africa_chinese_cities$destination_country))
# shorten DRC 
risk_all_cities_africa_chinese_cities %>% ungroup() %>% 
  mutate( destination_country=ifelse(destination_country=="Democratic Republic of Congo",
                                     "DRC",destination_country) ) -> pf_risk_all_cities
order_countries[order_countries=="Democratic Republic of Congo"] <- "DRC"
stacked_bar_plot<-ggplot(pf_risk_all_cities,
                         aes(x=factor(destination_country,levels=order_countries),y=fraction,
                             fill=factor(origin_city,levels=prev_order)))+
  scale_fill_manual(values=as.vector(polychrome(26)))+
  geom_bar(position="stack",stat="identity",show.legend = F)+
  ylab("")+ # Proportion of imported cases
  xlab("")+ # Destination Country
  export_theme # + theme(legend.position=NULL)

stacked_bar_plot$labels$fill<-"Origin City"
stacked_bar_plot

ggsave("./figures/stacked_bar_plot.pdf",width=6.8*0.9,height=2.8*0.9)

## ratio plot - code courtesy of Rene Niehus
### for all destinations
merged_lower <- read.csv(file="./out/export_curves_wuh_all_lower.csv")%>%
  dplyr::select("frac_wuhan","frac_all")
colnames(merged_lower)[1]<-c("frac_wuhan_l")
colnames(merged_lower)[2]<-c("frac_all_l")

merged_median <- read.csv(file="export_curves_wuh_all_intermediate.csv")%>%
  dplyr::select("frac_wuhan","frac_all")
colnames(merged_median)[1]<-c("frac_wuhan")
colnames(merged_median)[2]<-c("frac_all")

merged_upper <- read.csv(file="export_curves_wuh_all_upper.csv")%>%
  dplyr::select("frac_wuhan","frac_all")
colnames(merged_upper)[1]<-c("frac_wuhan_h")
colnames(merged_upper)[2]<-c("frac_all_h")

merged3<-cbind(merged_lower,merged_median,merged_upper)
merged3<-merged3%>%
  mutate(prop_wuhan_l = frac_wuhan_l / (frac_wuhan_l + frac_all_l),
         prop_wuhan_h = frac_wuhan_h / (frac_wuhan_h + frac_all_h),
         prop_wuhan = frac_wuhan / (frac_wuhan+frac_all)) -> merged3

file_dates<-read.csv(file="export_curves_wuh_all_upper.csv")

merged3$date.y<-as.Date(file_dates$date.y)

ggplot(data=merged3)+ #geom_line( aes(x=date.y,y=prop_wuhan,col="Wuhan") )+
  stat_smooth(aes(x=date.y,y = prop_wuhan, colour="Wuhan (intermediate)"), 
              formula = y ~ s(x, k = 10), method = "gam",se=FALSE)+
  stat_smooth(aes(x=date.y,y = prop_wuhan_l, colour="Wuhan (low)"), 
              formula = y ~ s(x, k = 10), method = "gam",se=FALSE)+ 
  stat_smooth(aes(x=date.y,y = prop_wuhan_h, colour="Wuhan (high)"), 
              formula = y ~ s(x, k = 10), method = "gam",se=FALSE)+
  stat_smooth(aes(x=date.y,y = 1-prop_wuhan, colour="Wuhan (intermediate)"), linetype=2,
              formula = y ~ s(x, k = 10), method = "gam",se=FALSE)+
  geom_line(aes(x=date.y,y=1-prop_wuhan_l,group=1, col="Wuhan (low)") , linetype=2,
            formula = y ~ s(x, k = 10), method = "gam",se=FALSE)+
  geom_line(aes(x=date.y,y=1-prop_wuhan_h,group=1,col="Wuhan (high)") , linetype=2,
            formula = y ~ s(x, k = 10), method = "gam",se=FALSE)+
  xlab("Time")+
  ylab("Global")+
  scale_x_date(breaks = "2 weeks") +
  scale_color_manual(name = "Legend", values = c("Wuhan (intermediate)"="green4", 
                                                 "Wuhan (low)"="brown",  
                                                 "Wuhan (high)"="magenta")) + 
  export_theme

ggsave("./figures/ratio_plot.pdf")

### for African destinations
merged_lower_afr <- read.csv(file="export_curves_wuh_all_africa_lower.csv")%>%
  dplyr::select("frac_wuhan","frac_all")
colnames(merged_lower_afr)[1]<-c("frac_wuhan_l")
colnames(merged_lower_afr)[2]<-c("frac_all_l")

merged_median_afr <- read.csv(file="export_curves_wuh_all_africa_intermediate.csv")%>%
  dplyr::select("frac_wuhan","frac_all")
colnames(merged_median_afr)[1]<-c("frac_wuhan")
colnames(merged_median_afr)[2]<-c("frac_all")

merged_upper_afr <- read.csv(file="export_curves_wuh_all_africa_upper.csv")%>%
  dplyr::select("frac_wuhan","frac_all")
colnames(merged_upper_afr)[1]<-c("frac_wuhan_h")
colnames(merged_upper_afr)[2]<-c("frac_all_h")

merged3_afr<-cbind(merged_lower_afr,merged_median_afr,merged_upper_afr)
merged3_afr<-merged3_afr%>%
  mutate(prop_wuhan_l = frac_wuhan_l / (frac_wuhan_l + frac_all_l),
         prop_wuhan_h = frac_wuhan_h / (frac_wuhan_h + frac_all_h),
         prop_wuhan = frac_wuhan / (frac_wuhan+frac_all)) -> merged3_afr

file_dates_afr<-read.csv(file="export_curves_wuh_all_africa_upper.csv")

merged3_afr$date.y<-as.Date(file_dates_afr$date.y)

ggplot(data=merged3_afr)+ #geom_line( aes(x=date.y,y=prop_wuhan,col="Wuhan") )+
  stat_smooth(aes(x=date.y,y = prop_wuhan, colour="Wuhan (intermediate)"), 
              formula = y ~ s(x, k = 10), method = "gam",se=FALSE)+
  stat_smooth(aes(x=date.y,y = prop_wuhan_l, colour="Wuhan (low)"), 
              formula = y ~ s(x, k = 10), method = "gam",se=FALSE)+ 
  stat_smooth(aes(x=date.y,y = prop_wuhan_h, colour="Wuhan (high)"), 
              formula = y ~ s(x, k = 10), method = "gam",se=FALSE)+
  stat_smooth(aes(x=date.y,y = 1-prop_wuhan, colour="Wuhan (intermediate)"), linetype=2,
              formula = y ~ s(x, k = 10), method = "gam",se=FALSE)+
  geom_line(aes(x=date.y,y=1-prop_wuhan_l,group=1, col="Wuhan (low)") , linetype=2,
            formula = y ~ s(x, k = 10), method = "gam",se=FALSE)+
  geom_line(aes(x=date.y,y=1-prop_wuhan_h,group=1,col="Wuhan (high)") , linetype=2,
            formula = y ~ s(x, k = 10), method = "gam",se=FALSE)+
  xlab("Time")+
  ylab("African destination countries")+
  scale_x_date(breaks = "2 weeks") +
  scale_color_manual(name = "Legend", values = c("Wuhan (intermediate)"="green4", 
                                                 "Wuhan (low)"="brown",  
                                                 "Wuhan (high)"="magenta")) + 
  export_theme

ggsave("./figures/ratio_plot_africa.pdf")

