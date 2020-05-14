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
  geom_line()+
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
  geom_vline(xintercept=as.Date('2020-01-12'),linetype='dotted')+
  geom_vline(xintercept=as.Date('2020-02-12'),linetype='dotted')+
  ylab("Predicted number of imported cases")+ guides(col=guide_legend(nrow=4))

line_plot$labels$colour<-"Country"
line_plot
#save(risk_all_cities_africa_line_weekly,file="risk_all_cities_africa_line_weekly.Rdata")
ggsave("./figures/line_plot.pdf",width=20,height=10)

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

stacked_bar_plot<-ggplot(risk_all_cities_africa_chinese_cities,
                         aes(x=factor(destination_country,levels=order_countries),y=fraction,
                             fill=factor(origin_city,levels=prev_order)))+
  scale_fill_manual(values=as.vector(polychrome(26)))+
  geom_bar(position="stack",stat="identity")+
  ylab("Proportion of imported cases")+
  xlab("Destination Country")+
  export_theme +
  theme(legend.position="bottom")

stacked_bar_plot$labels$fill<-"Origin City"
stacked_bar_plot

ggsave("./figures/stacked_bar_plot.pdf")

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

