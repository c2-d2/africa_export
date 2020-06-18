############################
## libraries
############################
library(tidyverse)
library(gam)
library(data.table)
library(Bolstad2)
library(lubridate)
select <- dplyr::select

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

comp_cum_incidence <- function(all_incidence_province) {
                all_incidence_province %>% 
                                filter(!is.na(n_infected)) %>% 
                                group_by( province_raw ) %>% 
                                summarise(  cum_inc=sum(n_infected) ) -> df
                return(df)
}

add_prov_pop <- function(prov_cum_incidence,file_prov_pop) {
                popn_size_provinces<-read.csv(file_prov_pop) %>% as_tibble()
                left_join( prov_cum_incidence,popn_size_provinces, by=c("province_raw"="province") ) -> df
                return(df)
}

comp_travel_rel_prev <- function(prov_inc_calibrated) {
                # compute travel relevant prevalence
                prov_inc_calibrated %>% filter( !is.na(n_infected_cal) ) %>% 
                                group_by( province_raw ) %>% 
                                select( dates,province_raw,n_infected_cal ) %>% 
                                mutate( 
                                                shift1=n_infected_cal,
                                                shift2=replace_na(lag(n_infected_cal,1) , replace = 0),
                                                shift3=lag(n_infected_cal,2) %>% replace_na(replace=0),
                                                shift4=lag(n_infected_cal,3) %>% replace_na(replace=0),
                                                shift5=lag(n_infected_cal,4) %>% replace_na(replace=0))  %>% 
                                mutate( travel_prev=shift1+shift2+shift3+shift4+shift5 ) %>%  # here could weigh according to incub distri
                                select( dates,province_raw,n_infected_cal,travel_prev ) -> df
                return(df)
}
