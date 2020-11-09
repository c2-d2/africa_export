## 29th October 2020
## Script: Estimate daily flight volume from 18 Chinese cities to international destinations from 1 December 2019 to 28 February 2020

Packages <- c("reshape2", "ggplot2", "ggpubr", "tidyr", "dplyr", "lubridate", "scales",
              "zoo", "caret", "RColorBrewer", "broom", "gridExtra", "stringr")

lapply(Packages, library, character.only = TRUE)

#' 
#' # SET UP DATA #
#' 
#' ## load data - Bluedot - and process
## ------------------------------------------------------------------------
bluedot <- read.csv("./Inputs/china_city_queries_all.csv")

## add yearMonth
bluedot$yearMonth <- as.Date(with(bluedot, paste(year, month, "01", sep="-")), "%Y-%m-%d")

# create column for time period
bluedot$period[bluedot$yearMonth >= "2018-12-01" & bluedot$yearMonth <= "2019-02-01"] <- 2019
bluedot$period[bluedot$yearMonth >= "2019-12-01" & bluedot$yearMonth <= "2020-02-01"] <- 2020

## joint column for NAY/PKX since one replaced the other
bluedot$origAirportCode <- as.character(bluedot$origAirportCode)
bluedot$origAirportCode_orig <- bluedot$origAirportCode

bluedot$origAirportCode <- ifelse((bluedot$origAirportCode_orig == "PKX" | bluedot$origAirportCode_orig == "NAY"), 
                                  "NAY/PKX", 
                                  bluedot$origAirportCode_orig)


## append continent column
continent <- read.csv("./Inputs/countries_continent_match.csv")
bluedot$arrival_continent <- continent$Continent[match(bluedot$destCtryName, continent$Country)]

# fix non-matches
continent$Country <- as.character(continent$Country)
continent$Country[continent$Country == "United States of America"] <- "United States"
continent$Country[continent$Country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
continent$Country[continent$Country == "Russian Federation"] <- "Russia"
continent$Country[continent$Country == "Côte d’Ivoire"] <- "Cote D'Ivoire"
continent$Country[continent$Country == "Republic of Korea"] <- "Korea (South)"
continent$Country[continent$Country == "United Republic of Tanzania"] <- "Tanzania"
continent$Country[continent$Country == "Democratic Republic of the Congo"] <- "Congo (Kinshasa)"
continent$Country[continent$Country == "China, Hong Kong Special Administrative Region"] <- "Hong Kong (SAR)"
continent$Country[continent$Country == "China, Macao Special Administrative Region"] <- "Macao (SAR)"

# missing Taiwan
bluedot$arrival_continent <- continent$Continent[match(bluedot$destCtryName, continent$Country)]
# manually fix
bluedot$arrival_continent[bluedot$destCtryName=="Taiwan"] <- "Asia"

#' 
#' ## load airport data to correct Cirium data arrival columns
## ------------------------------------------------------------------------
airports <- read.csv("./cirium/all_aiports.csv")

## manually fix Copenhagen CPH (Europe) and HNI (CN)
airports$countryName <- as.character(airports$countryName)
airports$countryName[airports$fs == "CPH"] <- "Denmark"
airports$countryName[airports$fs == "HMI"] <- "China"

airports <- airports %>% select(fs, city, countryName)

#' 
#' ## load and merge data - Cirium
## ------------------------------------------------------------------------

load_flight_data <- function(wd){
                setwd(wd)
                files <- list.files(path = ".", full.names = TRUE, pattern = "flights", recursive = TRUE)
                flightstats <- data_frame(filename = files) %>%
                                mutate(file_contents = map(filename,        
                                                           ~ readr::read_csv(file.path(".", .), col_types = readr::cols(.default ="c")))) %>% unnest()
                
                flightstats$origin <- do.call(rbind, strsplit(flightstats$filename, split="[_.]"))[,4]
                flightstats$date <- do.call(rbind, strsplit(flightstats$filename, split="[_.]"))[,5]
                flightstats$date <- as.Date(flightstats$date)
                # dedupe based on flightid (~5000 rows)
                flightstats <- flightstats %>% distinct(flightId, .keep_all = TRUE)
                
                return(flightstats)
}

flightstats_2019 <- load_flight_data("./cirium/cn_2018-2019/")
flightstats_2020 <- load_flight_data("./cirium/cn_2019-2020/")

flightstats <- bind_rows(flightstats_2019, flightstats_2020)

# remove unnecessary columns
flightstats <- flightstats[,!grepl(pattern="operationalTimes",colnames(flightstats))]
flightstats <- flightstats[,!grepl(pattern="airportResources",colnames(flightstats))]
flightstats <- flightstats[,!grepl(pattern="flightDurations",colnames(flightstats))]
flightstats <- flightstats[,!grepl(pattern="Carrier",colnames(flightstats))]
flightstats <- flightstats[,!grepl(pattern="delays",colnames(flightstats))]
flightstats <- flightstats[,!grepl(pattern="codeshares",colnames(flightstats))]
flightstats <- flightstats[,!grepl(pattern="flightEquipment",colnames(flightstats))]

flightstats <- flightstats %>% select(-c(irregularOperations, schedule.serviceClasses, schedule.restrictions, schedule.uplines, schedule.downlines, 
                                         departureDate.dateUtc, departureDate.dateLocal, arrivalDate.dateUtc, arrivalDate.dateLocal))

## joint column for NAY/PKX airport
flightstats$departureAirportFsCode_orig <- flightstats$departureAirportFsCode
flightstats$departureAirportFsCode <- ifelse(flightstats$departureAirportFsCode_orig == "PKX" | flightstats$departureAirportFsCode_orig == "NAY", 
                                             "NAY/PKX", 
                                             flightstats$departureAirportFsCode_orig)


## CORRECT ARRIVAL COUNTRY NAME - overwrite column
flightstats$arrCountryName <- airports$countryName[match(flightstats$arrivalAirportFsCode, airports$fs)]

# manually fix missing ones
check_missing_countries <- flightstats %>% filter(is.na(arrCountryName))
flightstats$arrCountryName <- as.character(flightstats$arrCountryName)
flightstats$arrCountryName[flightstats$arrivalAirportFsCode == "AMM"] <- "Jordan"
flightstats$arrCountryName[flightstats$arrivalAirportFsCode == "LEA"] <- "Australia"
flightstats$arrCountryName[flightstats$arrivalAirportFsCode == "SUU"] <- "United States"
flightstats$arrCountryName[flightstats$arrivalAirportFsCode == "RIV"] <- "United States"

## remove wrong arrival columns
flightstats <- flightstats %>% select(-c(arrLatitue, arrLongitude, arrAirportName,	arrCity, arrCountryCode, arrRegionName))



#' 
#' # STEP 1: Align Bluedot/IATA and Cirium data arrival names
## ------------------------------------------------------------------------
bluedot_dest <- bluedot %>% 
                distinct(destCtryName) %>% 
                pull()

bluedot_dest <- as.character(bluedot_dest)

flighstats_dest <- 
                flightstats %>% 
                distinct(arrCountryName) %>% 
                arrange(desc(arrCountryName)) %>% 
                pull()

flighstats_dest <- as.character(flighstats_dest)

arrival_country_names <- bluedot_dest
arrival_country_names[arrival_country_names =="Russia"] <- "Russian Federation"
arrival_country_names[arrival_country_names =="Spain"] <- "Spain and Canary Islands"
arrival_country_names[arrival_country_names =="Korea (South)"] <- "Republic of Korea"
arrival_country_names[arrival_country_names =="Hong Kong (SAR)"] <- "Hong Kong SAR"
arrival_country_names[arrival_country_names =="Macao (SAR)"] <- "Macau SAR"

## arrival_country_names represents Cirium spelling of 52 destinations

## append continent column to Cirium
flightstats$arrival_continent <- continent$Continent[match(flightstats$arrCountryName, continent$Country)]
flightstats %>% filter(is.na(arrival_continent)) %>% distinct(arrCountryName)

# fix non-matches
continent$Country <- as.character(continent$Country)
continent$Country[continent$Country == "Korea (South)"] <- "Republic of Korea"
continent$Country[continent$Country == "Russia"] <- "Russian Federation"
continent$Country[continent$Country == "Hong Kong (SAR)"] <- "Hong Kong SAR"
continent$Country[continent$Country == "Iran (Islamic Republic of)"] <- "Iran"
continent$Country[continent$Country == "Macao (SAR)"] <- "Macau SAR"
continent$Country[continent$Country == "Spain"] <- "Spain and Canary Islands"
continent$Country[continent$Country == "Czechia"] <- "Czech Republic"

# manually fix two and rematch
flightstats$arrival_continent <- continent$Continent[match(flightstats$arrCountryName, continent$Country)]
flightstats %>% filter(is.na(arrival_continent)) %>% distinct(arrCountryName)

flightstats$arrival_continent[flightstats$arrCountryName == "Taiwan"] <- "Asia"
flightstats$arrival_continent[flightstats$arrCountryName == "Reunion"] <- "Africa"
flightstats %>% filter(is.na(arrival_continent)) %>% distinct(arrCountryName)

## check continents
flightstats %>% select(arrivalAirportFsCode, arrCountryName, arrival_continent)

# filter for landed flights; passenger flights; arrival countries in analysis 
china_origin_cities <- unique(bluedot$origAirportCode)

flightstats_filter <- flightstats %>% 
    filter(arrCountryName %in% arrival_country_names & departureAirportFsCode %in% china_origin_cities & status == "L" & (schedule.flightType == "J" | schedule.flightType == "G"))

flightstats_filter$month <- month(flightstats_filter$date)

#' 
#' # STEP 2: transform from monthly bluedot data to daily - function
## ------------------------------------------------------------------------

month_to_day <- function(date_start, date_end, no_day){
    month <- bluedot %>% filter(yearMonth == date_start) %>% dplyr::select(-c(year, month, period))
    
    # change to daily df
    daily <- month[rep(seq_len(nrow(month)), each = no_day), ]
    
    # add dates
    daily$flight_date <- rep(seq(as.Date(date_start), as.Date(date_end), "days"), times=nrow(month))
    
    return(daily)
}

dec <- month_to_day("2019-12-01", "2019-12-31", 31)
jan <- month_to_day("2020-01-01", "2020-01-31", 31)
feb <- month_to_day("2020-02-01", "2020-02-29", 29) 

daily_bluedot <- rbind(dec, jan, feb)


#' 
#' # STEP 3: Distribute into daily using daily flight departure proportions from 2020 
## ------------------------------------------------------------------------
month_analysis <- unique(flightstats_filter$month)

flightstats_prop_2020 <- NULL
## estimate daily fractions for each origin 
for (i in 1:length(china_origin_cities)){
    for(j in 1:length(month_analysis)){
        df <- flightstats_filter %>% filter((date >= "2019-12-01" & date <= "2020-02-29") & 
                                                departureAirportFsCode == china_origin_cities[i] &
                                                month == month_analysis[j])
        tot_flights <- df %>% summarise(n=n()) %>% pull(n)
        prop_2020 <- as.data.frame(df %>% group_by(date) %>% summarise(prop_2020=n()/tot_flights))
        prop_2020$origin <- china_origin_cities[i]
        prop_2020$month <- month_analysis[j]
        
        flightstats_prop_2020 <- rbind(flightstats_prop_2020, prop_2020)
    }
}


## remove WUH
flightstats_prop_2020_exwuh <- flightstats_prop_2020 %>% filter(origin != "WUH")


### repeat for 2019  to add line to Figure S1
flightstats_prop_2019 <- data.frame()
for (i in 1:length(china_origin_cities)){
    for(j in 1:length(month_analysis)){
        df <- flightstats_filter %>% filter((date >= "2018-12-01" & date <= "2019-02-28") & 
                                                departureAirportFsCode == china_origin_cities[i] &
                                                month == month_analysis[j])
        tot_flights <- df %>% summarise(n=n()) %>% pull(n)
        prop_2019 <- as.data.frame(df %>% group_by(date) %>% summarise(prop_2019=n()/tot_flights))
        
        if (nrow(prop_2019) < 1) {
            break
            }
        
        prop_2019$origin <- china_origin_cities[i]
        prop_2019$month <- month_analysis[j]
        
        flightstats_prop_2019 <- rbind(flightstats_prop_2019, prop_2019)
    }
}

### spline function for Wuhan
wuh_spline <- 
    flightstats_filter %>% 
    filter((date >= "2019-12-01" & date <= "2020-02-29") & departureAirportFsCode == "WUH") %>%
    group_by(date) %>% 
    summarise(n=n())


dates <- seq(as.Date("2019-12-01"), as.Date("2020-02-29"), by = "days")

wuh_spline_all_dates <- data.frame(dates)
wuh_spline_all_dates$n <- wuh_spline$n[match(wuh_spline_all_dates$dates, wuh_spline$date)]
wuh_spline_all_dates$n[is.na(wuh_spline_all_dates$n)] <- 0

## time-series cross validation
## adapted from code provided in Section 4-CrossValidation (Lina Song), API 222B
no_weeks <- length(seq(as.Date("2019-12-01"), as.Date("2020-02-29"), by = "7 days"))

spar_seq<-seq(from = 0.1, to = 1, length.out = 1000)
CV_error_mtx  <- matrix(0, nrow = length(spar_seq), ncol = no_weeks)

j=1
for (spar_value in spar_seq){
    for (i in 1:12){
        train_index <- 7*i
        train <- wuh_spline_all_dates[1:train_index,]
        test_start_index <- train_index+1
        test_end_index <- test_start_index+6
        test <- wuh_spline_all_dates[test_start_index:test_end_index,]
        spline_mod <- smooth.spline(train$dates, train$n, 
                                    spar=spar_value, 
                                    cv=FALSE) # don't need this parameter
        predictions <- predict(spline_mod, as.integer(test$dates))$y
        rmse <- Metrics::rmse(test$n, predictions)
        CV_error_mtx[j,i] <- rmse
    }
    j=j+1
}
spar_optimal_index=which.min(rowMeans(CV_error_mtx[,1:12]))
spar_optimal=spar_seq[spar_optimal_index] 


###
spline_fit<-smooth.spline(wuh_spline_all_dates$dates,wuh_spline_all_dates$n,
                          spar=spar_optimal,nknots=30)

predict_prop <- data.frame(spline_fit$y)
predict_prop$date <- dates
predict_prop$month <- month(predict_prop$date)
colnames(predict_prop) <- c("y", "date", "month")
predict_prop$y <- ifelse(predict_prop$y <0, 0,  predict_prop$y)

## estimate wuhan proportions using spline predictions
month_wuh <- (unique(predict_prop$month))
wuh_prop <- NULL

for (i in 1:length(month_wuh)){
    month_df <- predict_prop %>%
                    filter(month == month_wuh[i])
    tot <- month_df %>% summarise(sum_y = sum(y)) %>% pull(sum_y)
    daily_prop <- as.data.frame(month_df %>% group_by(date) %>% mutate(prop = y/tot))  
    
    wuh_prop <- rbind(wuh_prop, daily_prop)  
}

wuh_prop %>% group_by(month(date)) %>% summarise(sum(prop)) 

### add WUH proportions from predictions from spline
wuh_prop_append <- wuh_prop %>% select(-c(y)) # remove prediction column
wuh_prop_append$month <- month(wuh_prop_append$date)
wuh_prop_append$origin <- "WUH"
colnames(wuh_prop_append) <- c("date", "month", "prop_2020", "origin")

flightstats_prop_2020_exwuh$month <- month(flightstats_prop_2020_exwuh$date)

## append to flightstats
flightstats_prop_2020_combined <- bind_rows(flightstats_prop_2020_exwuh, wuh_prop_append)


#' 
#' # STEP 5: Estimate daily volume
## ------------------------------------------------------------------------

# join proportions to df
daily_volume_final <- left_join(daily_bluedot, 
                                flightstats_prop_2020_combined %>% select(-c(month)), 
                                by=c("origAirportCode" = "origin", "flight_date" = "date")) 

## make NA's 0 (represents prop_2020 == 0 because no flights on those dates)
daily_volume_final$prop_2020 <- ifelse(is.na(daily_volume_final$prop_2020), 0, daily_volume_final$prop_2020)

daily_volume_final$dailyvol <- daily_volume_final$prop_2020*daily_volume_final$totalVol

#write.csv(daily_volume_final, "./data/flights_adjusted_200506.csv")

flights <- daily_volume_final

## process flight data -- validation step
colnames(flights)[2]<-"iata"
flights$flight_date<-as.Date(flights$flight_date)
#flights[,c("flight_date","origCityName","destCtryName","dailyvol")]

flights_all_cities_duplicated<-flights%>%
    subset(iata=='SZX'|iata=='HGH')%>%
    mutate(dailyvol=dailyvol/2)

flights_all_cities_duplicated_copy<-flights_all_cities_duplicated
flights_all_cities_duplicated_copy$origCityName<-str_replace_all(flights_all_cities_duplicated_copy$origCityName,'Hangzhou','Jiaxing')
flights_all_cities_duplicated_copy$origCityName<-str_replace_all(flights_all_cities_duplicated_copy$origCityName,'Shenzhen','Dongguan')

iata_not_duplicated<-c('CAN','CGO','CKG','CSX','CTU','FOC','HFE','KHN','NAY/PKX',
                       'NKG','PEK','PVG','SHA',"TSN",'XIY','WUH')
flights_all_cities_not_duplicated<-flights%>%
    subset(iata%in%iata_not_duplicated)

flights_all_cities_final<-data.frame(rbind(flights_all_cities_not_duplicated,
                                           flights_all_cities_duplicated,
                                           flights_all_cities_duplicated_copy))

flights_date_all_cities<-cbind.data.frame(flights_all_cities_final$flight_date,flights_all_cities_final$origCityName,
                                          flights_all_cities_final$destCtryName, flights_all_cities_final$dailyvol)
colnames(flights_date_all_cities)<-c("date","origin_city","destination_country","daily_volume")

#write.csv(flights_date_all_cities,"all_flight_pairs_0630.csv",row.names=F)


flights_all_cities2<-flights_date_all_cities %>%
    group_by(date, origin_city, destination_country) %>%
    summarise(daily_volume=sum(daily_volume))

flights_all_cities2$origin_city<- ifelse(flights_all_cities2$origin_city == "Xi An","Xi'an",
                                         as.character(flights_all_cities2$origin_city))

write.csv(flights_all_cities2, "./data/flights_all_cities2.csv")

# =============

### new dataset for 2019 - repeat steps to add gray line to Figure S1

dec_2019 <- month_to_day("2018-12-01", "2018-12-31", 31)
jan_2019 <- month_to_day("2019-01-01", "2019-01-31", 31)
feb_2019 <- month_to_day("2019-02-01", "2019-02-28", 28) 

daily_bluedot_2019 <- rbind(dec_2019, jan_2019, feb_2019)

daily_volume_final_2019 <- left_join(daily_bluedot_2019, 
                                     flightstats_prop_2019 %>% select(-c(month)), 
                                     by=c("origAirportCode" = "origin", "flight_date" = "date")) 


daily_volume_final_2019$prop_2019 <- ifelse(is.na(daily_volume_final_2019$prop_2019), 0, daily_volume_final_2019$prop_2019)
daily_volume_final_2019$dailyvol <- daily_volume_final_2019$prop_2019*daily_volume_final_2019$totalVol
daily_volume_final_2019$flight_date_2020 <- as.Date(with(daily_volume_final_2019, paste(year(flight_date)+1, month(flight_date), day(flight_date),sep="-")), "%Y-%m-%d")

plot <- daily_volume_final_2019 %>% 
    group_by(flight_date_2020) %>% 
    summarise(sum = sum(dailyvol))

write.csv(plot, "./data/2019_flight_line.csv")

