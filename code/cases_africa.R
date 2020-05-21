# load confirmed cases from all over the world

# run create_master_table.R first

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

############################
## all current cases
############################
df %>% summarise( sum(cases) )
df %>% filter(countriesAndTerritories=="China") %>% summarise( sum(cases) )

df %>% filter(continentExp=="Africa") %>% summarise( sum(cases) )
df %>% filter(continentExp=="Africa") %>% count(countriesAndTerritories)

df %>% filter(cases!=0) %>%  count(countriesAndTerritories)

# create has_detected_d ---------------------------------------------------
df %>% 
                select( dateRep,countriesAndTerritories,cases ) %>% 
                rename( date=dateRep, destination_country=countriesAndTerritories ) %>% 
                complete( date, nesting(destination_country ), fill=list(cases=0) ) %>% 
                arrange( destination_country, date ) %>% 
                mutate(destination_country=ifelse(destination_country=="South_Africa","South Africa",destination_country),
                       destination_country=ifelse(destination_country=="United_Republic_of_Tanzania","Tanzania",destination_country),
                       destination_country=ifelse(destination_country=="Democratic_Republic_of_the_Congo","Congo (Kinshasa)",destination_country),
                       destination_country=ifelse(destination_country=="Equatorial_Guinea","Equatorial Guinea",destination_country),
                       destination_country=ifelse(destination_country=="Cote_dIvoire","Cote D'Ivoire",destination_country)) %>% 
                group_by( destination_country ) %>% 
                mutate( cases_cumsum=cumsum(cases) ) %>% 
                mutate( has_detected_d = as.numeric(cases_cumsum!=0) ) %>% 
                select(-cases,-cases_cumsum) %>% ungroup() -> df_hasdetected
save( df_hasdetected, file="./out/hasdetected.Rdata" )

               
                
# for case counts from Wuhan
df %>% mutate( countriesAndTerritories=ifelse( countriesAndTerritories=="United_States_of_America","United States",countriesAndTerritories ),
                 countriesAndTerritories=ifelse( countriesAndTerritories=="United_Kingdom","United Kingdom",countriesAndTerritories ),
                 countriesAndTerritories=ifelse(countriesAndTerritories=="South_Korea","Korea (South)",countriesAndTerritories )) -> df_names

df_names %>% count(countriesAndTerritories) %>% pull(countriesAndTerritories) ->countr_ecdc 

glimpse(df_names)
df_names %>% select(dateRep, cases,countriesAndTerritories ) %>% 
                filter(countriesAndTerritories%in%highsurv_countries ) %>% 
                arrange( countriesAndTerritories,dateRep ) %>% 
                #
                group_by(countriesAndTerritories) %>% 
                filter( dateRep<= "2020-02-04" ) %>% summarise( sum=sum(cases) ) %>% print(n=Inf)


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
                                

                