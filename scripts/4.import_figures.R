## 29th October 2020
## SCRIPT 4 - Reads in master table and produces all of the import figures in the manuscript
## Specifically, produces the panels to create Figures 2, 3, S4 and S5

## Set the global working directory
setwd(main_wd)

# read in data from master table
mt <- read_csv(file='./data/master_table_1029.csv')

######## Map of imported cases in African destination countries
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

# create table of estimated imported # of cases in African countries under Scenario 1
mt_africa=mt%>%
  filter(is_africa_d==1)%>%
  mutate(imp_number=prevalence_o*fvolume_od*alpha)%>%
  group_by(destination_country,scenario)%>%
  summarise(sum=sum(imp_number))%>%
  subset(scenario=="Scenario 1")

mt_africa_final<-mt_africa[,c(1,3)]
colnames(mt_africa_final)[1]<-"Destination Country"
colnames(mt_africa_final)[2]<-"Number Exported"

# merge map data & exports table
map_data_final<-merge(map_data,mt_africa_final,by="Destination Country")

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
  theme_map() + theme(legend.position="none") #+
#theme(legend.title=element_text(color = "black", size = 10),
#legend.text = element_text(color = "black", size = 10))
map
ggsave("./figures/map_exports_africa_countries_scen1.pdf")

######## Plot of flight volume & prev over time

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
         #axis.text.x = element_blank(),
         axis.ticks.y = element_blank())
ggsave("./figures/line_plot_vol_scen1.pdf",width=7*0.85,height=1.5*0.85)
# extract peak total prev
date_peak_fv=pf_fly[which(pf_fly$fv_sum_week==max(pf_fly$fv_sum_week)),"date"]
date_peak_prev=pf_pr[which(pf_pr$pr_mean_week==max(pf_pr$pr_mean_week)),"date"]

######## Line plot of exported case trajectories in African destination countries

mt %>% 
  # filter
  filter(is_africa_d==1) %>% 
  filter(scenario=="Scenario 1") %>% 
  filter(date>"2019-11-01") %>% 
  mutate( exp_risk=fvolume_od*prevalence_o*alpha ) %>% 
  arrange( destination_country,date ) %>% # date, destination_country
  group_by(date, destination_country) %>% summarise(exp_risk_daily=sum(exp_risk) ) %>% ungroup() %>% 
  mutate( year=year(date),week=week(date) )   %>%
  group_by( destination_country, year, week ) %>% 
  mutate(n=n() ) %>%
  filter( n==7 ) %>% 
  mutate( exp_risk_weekly=mean(exp_risk_daily)) %>% slice(1) %>% ungroup() -> pf
#
pf %>% group_by( destination_country ) %>% 
  summarise( sum=sum(exp_risk_weekly)  ) %>% 
  arrange(desc(sum)) %>% slice(1:5) %>% pull(destination_country) -> top_5_c

pf %>% filter( year=="2020" | week=="52"  ) %>% 
  mutate( destination_country_new=ifelse(destination_country%in%top_5_c,as.character(destination_country),"other") ) %>% 
  ggplot( aes(x=date,y=exp_risk_weekly,
              color=(destination_country_new), group=destination_country ) ) +
  #geom_vline(xintercept=ymd('2020-01-01'),linetype='dotted')+
  geom_line(show.legend = F) +
  # colours from : https://davidmathlogic.com/colorblind/#%23332288-%23117733-%2344AA99-%2388CCEE-%23DDCC77-%23CC6677-%23AA4499-%23882255
  scale_color_manual(values=c("South Africa"="#332288",
                              "Algeria"="#117733",
                              "Kenya"="#88CCEE",
                              "Zambia"="#DDCC77",  
                              "Egypt"="#CC6677",
                              "other"="gray")) +
  export_theme +
  theme( axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.x=element_blank())

ggsave("./figures/line_plot_scen1.pdf",width=8*0.85,height=1.8*0.85)

# horizontal lines marking the window for each scenario 
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
pf2_2=pf2%>%filter(scenario!='Scenario 10' & scenario!='Scenario 11')
pf2_2$s_n=rev(seq(1,9))
(p <- pf %>% filter( year=="2020" | week=="52"  ) %>% 
    ggplot( aes(x=date,y=exp_risk_weekly ) ) +
    #geom_vline(xintercept=ymd('2020-01-01'),linetype='dotted')+
    geom_line(show.legend = F, col=NA) +
    geom_segment( data=pf2_2, 
                  aes(y=s_n/mscale-0.3,yend=s_n/mscale-0.3, x=int_start, xend=int_end )) +
    export_theme +
    theme( axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks.y = element_blank(),
           axis.text.x = element_text(size=10,angle = 45, hjust = 1,family="sans")))
range_x <- ggplot_build(p)$layout$panel_scales_x[[1]]$range$range
rnage_y <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range
ggplot_build(p)$layout$coord$limits

ggsave("./figures/some_lines.pdf",width=8*0.85,height=1.8*0.85)

######## Stacked bar plot of imported cases by African destination countries, by origin city

# rank cities by their max prev
mt_max_prev<-mt%>%
  group_by(origin_city)%>%
  summarise(max_prev_city=max(prevalence_o))%>%
  arrange(desc(max_prev_city))
colnames(mt_max_prev)<-c("city","max_prev")
prev_order<-as.character(mt_max_prev$city)

# create table # of imported cases by origin/country pair & fraction each city contributes 
# to total prev
mt_africa_date=mt%>%
  filter(is_africa_d==1)%>%
  mutate(imp_number=prevalence_o*fvolume_od*alpha)%>%
  group_by(destination_country,origin_city,scenario,date)%>%
  subset(scenario=="Scenario 1")%>%
  group_by(destination_country,origin_city)%>%
  summarise(num_exported=sum(imp_number))%>%
  group_by(destination_country)%>%
  mutate(fraction=num_exported/sum(num_exported))

# order countries by number of imports
ordered_countries<-mt_africa[order(mt_africa$sum),]
order_countries<-ordered_countries$destination_country                              
order_countries<-ifelse(order_countries=='Congo (Kinshasa)',
                        "Democratic Republic of Congo",as.character(order_countries))                                

mt_africa_date$destination_country<-
  ifelse(mt_africa_date$destination_country=='Congo (Kinshasa)',
         "Democratic Republic of Congo",
         as.character(mt_africa_date$destination_country))

# shorten DRC 
mt_africa_date %>% ungroup() %>% 
  mutate( destination_country=ifelse(destination_country=="Democratic Republic of Congo",
                                     "DRC",destination_country) ) -> pf_risk_all_cities
order_countries[order_countries=="Democratic Republic of Congo"] <- "DRC"

# plot stacked barplot
stacked_bar_plot<-ggplot(pf_risk_all_cities,
                         aes(x=factor(destination_country,levels=order_countries),y=fraction,
                             fill=factor(origin_city,levels=prev_order)))+
  scale_fill_manual(values=as.vector(polychrome(26)))+
  geom_bar(position="stack",stat="identity",show.legend = F)+
  ylab("")+ # Proportion of imported cases
  xlab("") + # Destination Country
  export_theme 

stacked_bar_plot$labels$fill<-"Origin City"
stacked_bar_plot

ggsave("./figures/stacked_bar_plot_scen1.pdf",width=6.8*0.9,height=2.8*0.9)



##################
# ratio plot - international
ratio_plot_1_all=mt %>% 
  # filter
  filter(is_global_d==1) %>% 
  filter(date>"2019-11-01") %>% 
  mutate( force_imp=prevalence_o*fvolume_od*alpha ) %>% 
  # 
  group_by(date,is_wuhan,scenario) %>% 
  summarise( force_imp_day=sum(force_imp) ) %>% 
  mutate( year=year(date),week=week(date) ) %>% ungroup() %>% 
  # by scenario and week
  group_by(is_wuhan,scenario,year,week) %>% 
  arrange(date) %>% 
  mutate(n=n()) %>% 
  filter( n==7 ) %>% 
  mutate( force_imp_week=sum(force_imp_day) ) %>% 
  slice(  1  ) %>% ungroup() %>% dplyr::select(-year,-week,-force_imp_day,-n) %>% 
  pivot_wider(names_from = is_wuhan, values_from = force_imp_week) 

colnames(ratio_plot_1_all)=c("date", "scenario", "non_W" , "W" ) 

ratio_plot_1_all%>% 
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
  filter(n==11) -> pf_probt_rib
#
pf_probt_rib %>% ggplot( aes(x=date)  )+
  #geom_ribbon( aes(ymin=prob_W_lower,ymax=prob_W_upper) , fill="gray",alpha=0.5 ) +
  #geom_ribbon( aes(ymin=1-prob_W_upper,ymax=1-prob_W_lower), fill="#DDD9DC", alpha=0.7 ) +
  export_theme+
  theme( axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.ticks.y = element_blank())+
  geom_path(aes(x=date,y=prop_wuhan,col=scenario))
ggsave(".figures/frac_time_wuhan_all.pdf",width=4*0.85,height=2)

ggplot(pf_probt_rib)+
  geom_ribbon(aes(x=date,ymin=prob_W_lower,ymax=prob_W_upper) , fill="gray",alpha=0.5 ) +
  #geom_ribbon( aes(ymin=1-prob_W_upper,ymax=1-prob_W_lower), fill="#DDD9DC", alpha=0.7 ) +
  export_theme+
  theme( axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.ticks.y = element_blank())+
  geom_line(aes(x=date,y=prop_wuhan,col=scenario)) 

# proportion over time ----------------------------------------------------
fill_cols <- as.vector(polychrome(10)[c(1,3:10)])
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
  set_names( "date", "scenario2", "non_W" , "W"  ) %>% 
  mutate( tot_imp=(W+non_W),
          min_tot_imp=min(tot_imp[tot_imp!=0]),
          prop_wuhan=W/(tot_imp + min_tot_imp ) ) %>% 
  dplyr::select(-W,-non_W) -> pf_probt
(p <- pf_probt %>% filter(date>=ymd("2020-01-01")) %>% 
    filter(!scenario2%in%c("Scenario 10","Scenario 11") ) %>% 
    ggplot(  )+
    geom_line(aes(y=prop_wuhan,x=date,col=(scenario2)),show.legend=F) +
    scale_y_continuous(breaks=c(0,0.5,1)) +
    scale_color_manual(values=fill_cols) +
    labs(x="",y="") +
    export_theme  ) 
ggsave("./figures/frac_time_plot_each_scen_afr.pdf",width=4*0.85*0.9*0.92,height=2*0.9)
# proportion over time global--------------------------------------
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
  set_names( "date", "scenario2", "non_W" , "W"  ) %>% 
  mutate( tot_imp=(W+non_W),
          min_tot_imp=min(tot_imp[tot_imp!=0]),
          prop_wuhan=W/(tot_imp + min_tot_imp ) ) %>% 
  dplyr::select(-W,-non_W) -> pf_probt
(p <- pf_probt %>% filter(date>=ymd("2020-01-01")) %>% 
    filter(!scenario2%in%c("Scenario 10","Scenario 11") ) %>% 
    ggplot(  )+
    geom_line(aes(y=prop_wuhan,x=date,col=(scenario2)),show.legend=F) +
    scale_y_continuous(breaks=c(0,0.5,1)) +
    scale_color_manual(values=fill_cols) +
    labs(x="",y="") +
    export_theme  ) 
ggsave("./figures/frac_time_plot_each_scen_global.pdf",width=4*0.85*0.9*0.92,height=2*0.9)


#### 
fill_cols <- as.vector(polychrome(10)[c(1,3:10)])

load("./data/df_country_cont_ecdc.Rdata")

mt %>% as_tibble() %>% 
  mutate( imp_number=prevalence_o*fvolume_od*alpha ) %>% 
  filter( scenario=="Scenario 2" ) %>% 
  # by scenario
  group_by(destination_country) %>% 
  summarise( sum=sum(imp_number) ) %>% ungroup() %>% 
  right_join( df_country_cont_ecdc, by="destination_country" ) %>% 
  filter(destination_country%in%global_countries) %>% 
  mutate( continentExp=ifelse(destination_country%in%c("United States","Canada"),"North America",continentExp))  %>% 
  mutate( continentExp=ifelse(destination_country%in%c("Chile","Argentina","Brazil"),"South America",continentExp))  %>% 
  mutate(continentExp=factor(continentExp,levels = c("Asia",
                                                     "Europe",
                                                     "Africa",
                                                     "North America",
                                                     "South America",
                                                     "Oceania")) ) %>% 
  arrange(continentExp,desc(sum)) %>% 
  mutate(n=1:n()) -> pf

pf %>% ggplot( aes(x=fct_inorder(destination_country),y=(sum),col=continentExp)  ) +
  geom_point() +
  geom_segment(aes(xend = fct_inorder(destination_country),
                   y = min(pf$sum), yend = sum, col=continentExp)) +
  scale_y_log10() +
  scale_color_manual(values=c("#F6222E","#5A5156","#FE00FA","#3283FE","#1CFFCE","#FEAF16") ) +
  labs(x="",y="",col="Continent") +
  export_theme
ggsave("./figures/diff_continents_cases.pdf",width=9,height=6, units="cm")

# repeat with more countries
all_used <- c(highsurv_countries,african_countries,global_countries) %>% unique()
mt %>% as_tibble() %>% 
  mutate( imp_number=prevalence_o*fvolume_od*alpha ) %>% 
  #filter( scenario=="Scenario 2" ) %>% 
  # by scenario
  group_by(scenario,destination_country,description) %>% 
  summarise( sum=sum(imp_number) ) %>% ungroup() %>% 
  right_join( df_country_cont_ecdc, by="destination_country" ) %>% 
  mutate( continentExp=ifelse(destination_country%in%c("United States","Canada"),"North America",continentExp))  %>% 
  mutate( continentExp=ifelse(destination_country%in%c("Chile",
                                                       "Argentina",
                                                       "Brazil",
                                                       # new relative to above code
                                                       "Colombia",
                                                       "Peru",
                                                       "Venezuela",
                                                       "Ecuador",
                                                       "Bolivia",
                                                       "Paraguay",
                                                       "Uruguay"),"South America",continentExp)) %>%  
  mutate(continentExp=factor(continentExp,levels = c("Asia",
                                                     "Europe",
                                                     "Africa",
                                                     "North America",
                                                     "South America",
                                                     "Oceania")) ) %>% 
  filter(!is.na(continentExp)) %>% 
  filter(!is.na(sum)) %>% 
  filter(destination_country%in%all_used) -> pf
pf %>% filter( description=="Main scenario"  ) %>% 
  arrange(desc(sum)) %>% 
  mutate( n=1:n() ) %>% rename(sum_s1 = sum) %>% select(-scenario,-description) -> pf_1
pf %>% filter( description!="Main scenario"  ) %>% 
  group_by( destination_country ) %>% 
  summarise( lower=range(sum)[1],
             upper=range(sum)[2] ) %>% 
  left_join(pf_1,by="destination_country") %>% 
  arrange(n)-> pf_2
smallest_v <- min(pf_2$lower)

pf_2 %>% arrange(continentExp) %>% print(n=Inf) # 43
pf_2 %>% count(continentExp,wt = n())

pf_2 %>% ggplot( aes(x=fct_inorder(destination_country))  ) +
  geom_point( aes(y=sum_s1,col=continentExp) ) +
  geom_segment(aes(xend = fct_inorder(destination_country),
                   y = lower, yend = upper, col=continentExp)) +
  scale_y_log10(labels=comma) +
  scale_color_manual(values=c("#F6222E","#5A5156","#FE00FA","#3283FE","#1CFFCE","#FEAF16") ) +
  labs(x="",y="",col="Continent") +
  export_theme
ggsave("./figures/diff_continents_cases_more.pdf",width=24,height=7, units="cm")



