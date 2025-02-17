## 29th October 2020
## PRE-COMPUTATION SCRIPT A - extract global case data from ECDC and get summaries for manuscript
##          - also identifies which countries have identified cases on each date, used for discussion text

setwd(main_wd)

# load ECDC data
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
data %>% glimpse()

# initial wrangle
data %>% 
                mutate( dateRep=dmy(dateRep),
                        countriesAndTerritories=as.character(countriesAndTerritories)) %>% 
                select(year,month,day,dateRep,continentExp,countriesAndTerritories,popData2019, 
                       cases,deaths) %>% as_tibble() %>%
  ## Filter by date reported in manuscript
  filter(dateRep <= "2020-08-31") -> df

# explore
df$dateRep %>% max() # get most current data
df$cases %>% sum() # total cases 
df$cases[df$countriesAndTerritories=="China"] %>% sum() # cases in China
df$countriesAndTerritories[df$cases!=0] %>% unique() %>% length() # how many different locations
# As of 31st August 2020, 1.1 million confirmed cases of COVID-19 have been registered worldwide, with 85 thousand detected in mainland China and the remainder detected internationally in 209 other locations

############################
## all current cases
############################
df %>% summarise( sum(cases) )
df %>% filter(countriesAndTerritories=="China") %>% summarise( sum(cases) )

df %>% filter(continentExp=="Africa") %>% summarise( sum(cases) )
df %>% filter(continentExp=="Africa",cases!=0) %>% count(countriesAndTerritories)

df %>% filter(cases!=0) %>%  count(countriesAndTerritories)

# create has_detected_d ---------------------------------------------------
df %>% 
                select( dateRep,countriesAndTerritories,continentExp,cases ) %>% 
                rename( date=dateRep, destination_country=countriesAndTerritories ) %>% 
                complete( date, nesting(destination_country,continentExp ), fill=list(cases=0) ) %>% 
                arrange( destination_country, date ) %>% 
                mutate(destination_country=ifelse(destination_country=="South_Africa","South Africa",destination_country),
                       destination_country=ifelse(destination_country=="United_Republic_of_Tanzania","Tanzania",destination_country),
                       destination_country=ifelse(destination_country=="Democratic_Republic_of_the_Congo","Congo (Kinshasa)",destination_country),
                       destination_country=ifelse(destination_country=="Equatorial_Guinea","Equatorial Guinea",destination_country),
                       destination_country=ifelse(destination_country=="Cote_dIvoire","Cote D'Ivoire",destination_country),
                       #
                       destination_country=ifelse(destination_country=="South_Korea","Korea (South)",destination_country),
                       destination_country=ifelse(destination_country=="New_Zealand","New Zealand",destination_country),
                       destination_country=ifelse(destination_country=="United_Arab_Emirates","United Arab Emirates",destination_country),
                       destination_country=ifelse(destination_country=="United_Kingdom","United Kingdom",destination_country),
                       destination_country=ifelse(destination_country=="United_States_of_America","United States",destination_country),
                       destination_country=ifelse(destination_country=="Vietnam","Viet Nam",destination_country)) -> df2

df2             %>% group_by( destination_country ) %>% 
                mutate( cases_cumsum=cumsum(cases) ) %>% 
                mutate( has_detected_d = as.numeric(cases_cumsum!=0) ) %>% 
                select(-cases,-cases_cumsum) %>% ungroup() -> df_hasdetected
save( df_hasdetected, file="./data/hasdetected.Rdata" )

df2 %>% group_by(destination_country) %>% 
                slice(1) %>% ungroup() %>% select(destination_country,continentExp) -> df_country_cont_ecdc
save( df_country_cont_ecdc, file="./data/df_country_cont_ecdc.Rdata" )

############################
## cases in africa
############################

highsurv_countries <-c('United States','Australia','Canada','Korea (South)',
                       'United Kingdom',
                       'Netherlands','Sweden',
                       'Germany','Spain','Singapore')
african_countries<-c('Mauritius','Mauritania','South Africa','Kenya','Egypt','Ethiopia','Morocco',
                     'Algeria','Nigeria','Ghana','Tanzania','Senegal','Guinea',
                     'Zimbabwe','Congo (Kinshasa)','Sudan','Angola','Zambia',
                     'Gabon','Madagascar','Equatorial Guinea','Tunisia',
                     'Uganda','Mozambique','Seychelles', "Cote D'Ivoire")

df2 %>% filter( destination_country%in%african_countries ) %>% 
                group_by(destination_country) %>% summarise( sum=sum(cases),date=max(date) ) %>% print(n=Inf)
               
                
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
                                

                