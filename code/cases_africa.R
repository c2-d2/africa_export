# load confirmed cases from all over the world

#these libraries need to be loaded
library(utils)
library(tidyverse)
library(lubridate)
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
data %>% glimpse()

#
data %>% 
                mutate( dateRep=dmy(dateRep),
                        countriesAndTerritories=as.character(countriesAndTerritories)) %>% 
                select(year,month,day,dateRep,continentExp,countriesAndTerritories,popData2018, 
                       cases,deaths) %>% as_tibble() -> df
                
#
df %>% 
                filter(continentExp =="Africa") %>% 
                select(-continentExp) -> df

df %>% count(countriesAndTerritories) %>% print(n=Inf)

df %>% arrange(countriesAndTerritories,dateRep) %>% 
                # need to filter our 28 locations
                group_by(countriesAndTerritories) %>% 
                filter(cases>0) %>% 
                summarise( earliest_case = min(dateRep) ) %>% 
                arrange(earliest_case) %>% 
                mutate( rel_case_found = as.numeric(earliest_case-ymd("2020-02-15")) ) ->df_case_first

df %>% arrange(countriesAndTerritories,dateRep) %>% 
                # need to filter our 28 locations
                group_by(countriesAndTerritories) %>% 
                summarise( cases_cum = sum(cases) ) %>% 
                arrange(desc(cases_cum)) -> df_case_tot
df %>% arrange(countriesAndTerritories,dateRep) %>% 
                # need to filter our 28 locations
                group_by(countriesAndTerritories) %>% 
                summarise( deaths_cum = sum(deaths) ) %>% 
                arrange(desc(deaths_cum)) -> df_death_tot

left_join(df_case_tot,df_case_first,by="countriesAndTerritories") %>% 
                left_join(df_death_tot,by="countriesAndTerritories") %>% 
                ggplot( aes(x=rel_case_found,y=log(cases_cum) ) ) + 
                geom_point() +
                geom_point(aes(y=log(deaths_cum)),color="red")
                                

                