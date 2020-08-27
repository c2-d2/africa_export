############################
## libraries
############################
library(tidyverse)
library(gam)
library(data.table)
library(Bolstad2)
library(lubridate)
library(ggthemes)
library(stats)
library(tidyverse)
library(patchwork)
library(RColorBrewer)

# settings
select <- dplyr::select
tibble.print_max <- 5
tibble.print_min <- 5


# plot theme, courtesy of James Hay
export_theme <- theme_tufte() + 
  theme(
    ## Axis text and titles
    axis.text.x = element_text(size=6,angle = 45, hjust = 1,family="sans"),
    axis.text.y=element_text(size=6,family="sans"),
    axis.title.x=element_text(size=8,family="sans",vjust=-1),
    axis.title.y=element_text(size=8,family="sans"),
    ## Axis lines
    axis.line = element_line(),
    axis.ticks = element_line(),
    ## Legends
    legend.title=element_text(size=10,family="sans",face="italic"),
    legend.text=element_text(size=6,family="sans"),
    legend.key.size= unit(0.2, "cm"),
    legend.margin = margin(0,0,0,0, "cm"),
    ## Strips for facet_wrap
    strip.text=element_text(size=8,family="sans"),
    strip.background=element_rect(fill="#f0f0f0"),
    ## Tags
    plot.tag=element_text(size=10,family="sans",face="bold",hjust=0,vjust=-3),
    plot.title=element_text(size=8,family="sans",hjust=0.5,face="bold",vjust=-3)
    )


shift_2_delays <- function(confirmed_cases_date,incubation_period,delay) {
                confirmed_cases_date%>%
                                arrange( province_raw,dates ) %>%  # helps to check the df visually
                                group_by(province_raw)%>%
                                mutate(n_onset=data.table::shift(n,n=delay)) %>% 
                                mutate(n_infected=data.table::shift(n_onset,n=incubation_period)) %>% 
                                ungroup() %>% 
                                relocate( dates, province_raw,n_infected,n_onset ) ->df
                return(df)
}

plot_conf_onset <- function(all_incidence_province,confirmed_cases_date) {
                # --subset to only relevant columns -- #
                columns<-c("date","dates","province_raw","n_onset","n_infected")
                
                # all dates subset
                all_incidence_province_subset<-all_incidence_province %>% select(any_of(columns) )
                # lineplot of confirmed cases, symptom onset and infection incidence (all dates)
                # all_incidence_province_subset_long=melt(all_incidence_province_subset,
                #                                         id.vars=c("date","dates","province_raw"), 
                #                                         variable.name="number_individuals")
                all_incidence_province_subset_long <- all_incidence_province_subset %>% pivot_longer( cols=c("n_onset","n_infected"),
                                                                                                      names_to ="number_individuals")
                
                lineplot<-ggplot(all_incidence_province_subset_long,
                                 aes(x=dates,y=value))+
                                geom_point(data=confirmed_cases_date,aes(x=dates,y=n),size=0.25) +
                                geom_line(aes(col=number_individuals))+scale_x_date(breaks="2 weeks")+
                                theme(axis.text.x = element_text(angle = 45, hjust = 1))+
                                scale_color_discrete(labels = c("infection incidence","symptom_onset_incidence","infection_incidence"))+
                                labs(colour="Legend")+ylab("Number of individuals")
                p <- lineplot+facet_wrap(.~province_raw,scales="free")
                return(p)
}

plot_conf_onset_2 <- function(all_incidence_province,confirmed_cases_date) {
  # --subset to only relevant columns -- #
  columns<-c("dates","province_raw","n_onset","n_infected_cal")
  
  # all dates subset
  all_incidence_province_subset<-all_incidence_province %>% select(any_of(columns) )
  all_incidence_province_subset_long <- all_incidence_province_subset %>% 
    pivot_longer( cols=c("n_onset","n_infected_cal"),names_to ="number_individuals")
  
  lineplot<-ggplot(all_incidence_province_subset_long,
                   aes(x=dates,y=value))+
    geom_point(data=confirmed_cases_date,aes(x=dates,y=n),size=0.25) +
    geom_line(aes(col=number_individuals))+
    scale_x_date(breaks="2 weeks",
                 limits=c(as.Date('2020-01-01'),as.Date('2020-02-29')))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    #scale_color_discrete(labels = c("infection incidence",
    # "symptom onset incidence"))+
    labs(colour="Legend")+ylab("Number of individuals")+export_theme+
    theme(axis.title.y=element_blank(),legend.position="none")
  p <- lineplot+facet_wrap(.~province_raw,scales="free")
  return(p)
}

comp_cum_incidence <- function(all_incidence_province, filen) {
                all_incidence_province %>% 
                                filter(!is.na(n_infected)) %>% 
                                group_by( province_raw ) %>% 
                                summarise(  cum_inc=sum(n_infected),.groups="keep") -> df
                popn_size_provinces<-read.csv(filen,stringsAsFactors=FALSE) %>% as_tibble()
                left_join( df,popn_size_provinces, by=c("province_raw"="province") ) -> df
                df %>% mutate(cum_inc_percap=cum_inc/popn_size_province  ) ->df
                return(df)
}



comp_travel_rel_prev <- function(city_n_inf_caladj_den, rel_dur) {
                # compute travel relevant prevalence
                if(rel_dur == 2){
                                city_n_inf_caladj_den %>% filter( !is.na(n_infected_caladj) ) %>% 
                                                group_by( city ) %>% 
                                                select( province,city,date,n_infected_caladj ) %>% 
                                                mutate( 
                                                                shift1=n_infected_caladj,
                                                                shift2=replace_na(lag(n_infected_caladj,1) , replace = 0))  %>% 
                                                mutate( travel_prev=shift1+shift2) %>%  # here could weigh according to incub distri
                                                select( province,city,date,travel_prev ) %>% ungroup() -> df
                } else if (rel_dur == 5) {
                                city_n_inf_caladj_den %>% filter( !is.na(n_infected_caladj) ) %>% 
                                                group_by( city ) %>% 
                                                select( province,city,date,n_infected_caladj ) %>% 
                                                mutate( 
                                                                shift1=n_infected_caladj,
                                                                shift2=replace_na(lag(n_infected_caladj,1) , replace = 0),
                                                                shift3=lag(n_infected_caladj,2) %>% replace_na(replace=0),
                                                                shift4=lag(n_infected_caladj,3) %>% replace_na(replace=0),
                                                                shift5=lag(n_infected_caladj,4) %>% replace_na(replace=0))  %>% 
                                                mutate( travel_prev=shift1+shift2+shift3+shift4+shift5 ) %>%  # here could weigh according to incub distri
                                                select( province,city,date,travel_prev ) %>% ungroup() -> df
                } else if (rel_dur == 7) {
                                city_n_inf_caladj_den %>% filter( !is.na(n_infected_caladj) ) %>% 
                                                group_by( city ) %>% 
                                                select( province,city,date,n_infected_caladj ) %>% 
                                                mutate( 
                                                                shift1=n_infected_caladj,
                                                                shift2=replace_na(lag(n_infected_caladj,1) , replace = 0),
                                                                shift3=lag(n_infected_caladj,2) %>% replace_na(replace=0),
                                                                shift4=lag(n_infected_caladj,3) %>% replace_na(replace=0),
                                                                shift5=lag(n_infected_caladj,4) %>% replace_na(replace=0),
                                                                shift6=lag(n_infected_caladj,5) %>% replace_na(replace=0),
                                                                shift7=lag(n_infected_caladj,6) %>% replace_na(replace=0))  %>% 
                                                mutate( travel_prev=shift1+shift2+shift3+shift4+shift5+shift6+shift7 ) %>%  # here could weigh according to incub distri
                                                select( province,city,date,travel_prev ) %>% ungroup() -> df
                }
                return(df)
}


comp_travel_rel_prev_nonwuh_gap <- function(city_n_inf_caladj_den){
                # 2 day prevalent for non-Wuhan (after 3 days)
                non_wuh <- city_n_inf_caladj_den %>% filter( !is.na(n_infected_caladj) & city != "Wuhan") %>% 
                                group_by( city ) %>% 
                                select( province,city,date,n_infected_caladj ) %>% 
                                mutate( 
                                                shift1=n_infected_caladj,
                                                shift2=replace_na(lag(n_infected_caladj,1) , replace = 0),
                                                shift3=lag(n_infected_caladj,2) %>% replace_na(replace=0),
                                                shift4=lag(n_infected_caladj,3) %>% replace_na(replace=0),
                                                shift5=lag(n_infected_caladj,4) %>% replace_na(replace=0))  %>% 
                                mutate( travel_prev=shift4+shift5 ) %>%  # here could weigh according to incub distri
                                select( province,city,date,travel_prev ) %>% ungroup()
                
                # 5 day prevalent for Wuhan
                wuh <- city_n_inf_caladj_den %>% filter( !is.na(n_infected_caladj) & city == "Wuhan") %>% 
                                group_by( city ) %>% 
                                select( province,city,date,n_infected_caladj ) %>% 
                                mutate( 
                                                shift1=n_infected_caladj,
                                                shift2=replace_na(lag(n_infected_caladj,1) , replace = 0),
                                                shift3=lag(n_infected_caladj,2) %>% replace_na(replace=0),
                                                shift4=lag(n_infected_caladj,3) %>% replace_na(replace=0),
                                                shift5=lag(n_infected_caladj,4) %>% replace_na(replace=0))  %>% 
                                mutate( travel_prev=shift1+shift2+shift3+shift4+shift5 ) %>%  # here could weigh according to incub distri
                                select( province,city,date,travel_prev ) %>% ungroup()
                
                df <- rbind(non_wuh, wuh)
                
                return(df)
}

get_prov_city_adjust <- function(file) {
                load( file )
                df <- prov_city_adjust <- frac_popn_city2 %>% 
                                mutate( f_guangdong3_zhejiang2=1,
                                        f_guangdong3_zhejiang2=ifelse(province=="Guangdong",1/3,f_guangdong3_zhejiang2),
                                        f_guangdong3_zhejiang2=ifelse(province=="Zhejiang",1/2,f_guangdong3_zhejiang2)) 
                return(df)
}

adjust_prov_prev_by_city <- function(prov_inc_calibrated , prov_city_adjust, assignment="by_city"){
                date_v <- prov_inc_calibrated$dates %>% unique()
                table_key <- prov_city_adjust %>% select(province,city) %>% 
                                expand_grid(date=date_v)
                
                ## Attribute all cases from province to that city, split evenly by constituent cities
                if(assignment=="by_city"){
                  print("Assigning all province cases to cities")
                  table_key %>% left_join( prov_city_adjust, by=c("province","city") ) %>% 
                                  left_join( prov_inc_calibrated, by=c("province"="province_raw","date"="dates")  ) %>% 
                                  mutate( n_infected_caladj=n_infected_cal*by_city ) %>% 
                                  select( province,city,date,n_infected_caladj ) -> df
                  
                  ## Aportion cases proportional to fractional share of province population
                } else if(assignment == "by_pop"){
                  print("Assigning province cases to cities equal to share of population")
                  table_key %>% left_join( prov_city_adjust, by=c("province","city") ) %>% 
                    left_join( prov_inc_calibrated, by=c("province"="province_raw","date"="dates")  ) %>% 
                    mutate( n_infected_caladj=n_infected_cal*by_pop ) %>% 
                    select( province,city,date,n_infected_caladj ) -> df
                  
                  ## Aportion cases proportional to fractional share of province's cases
                } else if(assignment == "by_cases"){
                  print("Assigning province cases to cities equal to reported share of cases")
                  table_key %>% left_join( prov_city_adjust, by=c("province","city") ) %>% 
                    left_join( prov_inc_calibrated, by=c("province"="province_raw","date"="dates")  ) %>% 
                    mutate( n_infected_caladj=n_infected_cal*by_cases ) %>% 
                    select( province,city,date,n_infected_caladj ) -> df
                  
                  ## Default is all to cities
                } else {
                  print("Invalid assignment option")
                  table_key %>% left_join( prov_city_adjust, by=c("province","city") ) %>% 
                    left_join( prov_inc_calibrated, by=c("province"="province_raw","date"="dates")  ) %>% 
                    mutate( n_infected_caladj=n_infected_cal*by_city ) %>% 
                    select( province,city,date,n_infected_caladj ) -> df
                  
                }
                return(df)
}

############
generate_alphas <- function( all_dat,file_obs_cnt ) {
                # get the observed counts
                cases_high_cap_loc <-read.csv(file_obs_cnt,stringsAsFactors=FALSE) 
                cases_high_cap_loc$cases_scaled <- ifelse(cases_high_cap_loc$Country!='Singapore',
                                                          round(cases_high_cap_loc$Cases_lm*2.5),
                                                          cases_high_cap_loc$Cases_lm) 
                cases_high_cap_loc <- cases_high_cap_loc %>% select(Country, cases_scaled) %>% 
                                mutate(Country=ifelse(Country=="Rep  Korea","Korea (South)",Country),
                                       Country=ifelse(Country=="UK","United Kingdom",Country),
                                       Country=ifelse(Country=="US","United States",Country))
                # then go through scenarios and add
                scenario_v <- all_dat$scenario %>% unique()
                list_save <- list()
                for (i in 1:length(scenario_v)) {
                                scenario_i <- scenario_v[i]
                                #
                                # 6 scenarios
                                # for each scenario select Wuhan and pre lockdown
                                all_dat %>% filter(scenario==scenario_i) %>% 
                                                filter( is_wuhan==1,is_prelockdown_date==1,is_highsurv_d==1 ) %>% 
                                                mutate(force_imp=fvolume_od*prevalence_o) %>% 
                                                filter(date%in%dates_seq)%>%

                                                #
                                                group_by(destination_country) %>% summarise( force_imp_sum=sum(force_imp) ) %>% 
                                                #
                                                left_join( cases_high_cap_loc, by=c("destination_country"="Country") ) -> df_lin_fit
                                
                                fit_wuhan <-glm(cases_scaled~offset(log(force_imp_sum)),
                                                data=df_lin_fit,
                                                family=poisson(link="log")) 
                                alpha_i <- coefficients(fit_wuhan) %>% exp()
                                tibble( scenario=scenario_i , alpha=alpha_i  ) -> list_save[[i]]
                }
                list_save %>% bind_rows() -> df_alphas
                return(df_alphas)
}
