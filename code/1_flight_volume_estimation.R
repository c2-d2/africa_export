
## TITLE: Estimation of daily flight volume from 18 Chinese cities to 16 international destinations from 1 December 2019 to 28 February 2020
## Author: Taylor Chin (Center for Communicable Disease Dynamics, Harvard T.H. Chan School of Public Health)
## Date: 23 March 2020


Packages <- c("deSolve", "bbmle", "reshape2", "ggplot2", 
              "fitdistrplus", "ggpubr", "grid", "plotrix", "tsiR", "tidyr", "dplyr", "lubridate",
              "kableExtra", "scales", "pracma", "zoo", "caret", "knitr",
              "RColorBrewer", "broom", "psych", "gridExtra", 
              "ggmap", "rgdal", "stringi", "purrr", "maptools", "stringr", "tmap")

lapply(Packages, library, character.only = TRUE)

#' 
#' #===============================================
#' # FLIGHT VOLUME
#' #===============================================
#' 
#' # SET UP DATA #
#' 
#' ## load data - Bluedot - and process
## ------------------------------------------------------------------------
bluedot <- read.csv("./Flight Volume Estimation/Inputs/PAX_China_to_World_Mar2_2020.csv")

# use year and month columns to create year-month
bluedot$yearMonth <- as.Date(with(bluedot, paste(year, month, "01", sep="-")), "%Y-%m-%d")

# create column for time period
bluedot$period[bluedot$yearMonth >= "2017-12-01" & bluedot$yearMonth <= "2018-02-01"] <- 2018
bluedot$period[bluedot$yearMonth >= "2018-12-01" & bluedot$yearMonth <= "2019-02-01"] <- 2019

## joint column for NAY/PKX
bluedot$origAirportCode <- as.character(bluedot$origAirportCode)
bluedot$origAirportCode_orig <- bluedot$origAirportCode

bluedot$origAirportCode <- ifelse((bluedot$origAirportCode_orig == "PKX" | bluedot$origAirportCode_orig == "NAY"), 
                                  "NAY/PKX", 
                                  bluedot$origAirportCode_orig)

## remove Thailand from destinations
bluedot <- bluedot %>% filter(destCtryName != "Thailand")

# 16 cities in bluedot data because Dongguan is considered Shenzhen (share airport SZX) and Jiaxing is considered Shenzhen (share airport HGH)



#' 
#' ## load and merge data - Flightstats - and save on drive for all
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

flightstats_2019 <- load_flight_data("./cov2019_flightdata/cirium/cn_2018-2019/")
flightstats_2020 <- load_flight_data("./cov2019_flightdata/cirium/cn_2019-2020/")

# rbind
flightstats <- rbind(flightstats_2019, flightstats_2020)

# remove unnecessary columns
flightstats <- flightstats[,!grepl(pattern="operationalTimes",colnames(flightstats))]
flightstats <- flightstats[,!grepl(pattern="airportResources",colnames(flightstats))]
flightstats <- flightstats[,!grepl(pattern="flightDurations",colnames(flightstats))]

iata_city <- read.csv("./Flight Volume Estimation/Inputs/cn_iata_code_shortlist.csv")
colnames(iata_city) <- c("origin_city_analysis", "origin_airport_name", "iata_codes")

flightstats <- left_join(flightstats, iata_city, by=c("departureAirportFsCode"="iata_codes"))

## joint column for NAY/PKX airport
flightstats$departureAirportFsCode_orig <- flightstats$departureAirportFsCode
flightstats$departureAirportFsCode <- ifelse(flightstats$departureAirportFsCode_orig == "PKX" | flightstats$departureAirportFsCode_orig == "NAY", "NAY/PKX", flightstats$departureAirportFsCode_orig)


#' 
#' # STEP 1: Explore how total volume in three months is distributed - Cirium data
#' 
#' ## 2018 Dec - 2019 Feb data
## ------------------------------------------------------------------------
arrival_country_names <- c('United States','Australia','Canada','Republic of Korea','United Kingdom',
                           'Netherlands','Sweden', 'Germany','Spain and Canary Islands', 'Singapore', 
                           'Mauritius','South Africa','Kenya','Egypt','Ethiopia','Morocco' )

flightstats_filter <- flightstats %>% filter(arrCountryName %in% arrival_country_names &
                                                             status == "L" & (schedule.flightType == "J" | schedule.flightType == "G"))
# filter for landed flights; passenger flights


flightstats_daily_2019 <- flightstats_filter %>% 
                filter(date < "2019-12-01") %>% 
                group_by(date, departureAirportFsCode) %>% 
                summarise(n=n())

# plot for individual origins
A <- ggplot(data=(flightstats_daily_2019), aes(x=date, y=n, fill=factor(departureAirportFsCode))) +
                geom_bar(stat = "identity") +
                theme_classic() +
                scale_x_date(breaks = "1 week") +
                scale_y_continuous(labels=comma) +
                geom_vline(xintercept=as.Date("2020-01-23")) + 
                ggtitle("Dec 2018 - Feb 2019") +
                theme(text = element_text(size=8), axis.text.x = element_text(angle = 45, hjust = 1)) + 
                guides(fill=guide_legend(ncol=2)) +
                ylab("Number of flight departures (daily)")

A

#' 
#' ## 2019 Dec - 2020 Feb data:
## ------------------------------------------------------------------------

flightstats_daily_2020 <- flightstats_filter %>% 
                filter(date >= "2019-12-01") %>% 
                group_by(date, departureAirportFsCode) %>% 
                summarise(n=n())

# plot for individual origins
B <- ggplot(data=(flightstats_daily_2020), aes(x=date, y=n, fill=factor(departureAirportFsCode))) +
                geom_bar(stat = "identity") +
                theme_classic() +
                scale_x_date(breaks = "1 week") +
                scale_y_continuous(labels=comma) +
                geom_vline(xintercept=as.Date("2020-01-23")) + 
                ggtitle("Dec 2019 - Feb 2020") +
                theme(text = element_text(size=8), axis.text.x = element_text(angle = 45, hjust = 1)) + 
                guides(fill=guide_legend(ncol=2)) +
                ylab("Number of flight departures (daily)")

B


#' 
#' ## compare plots
## ------------------------------------------------------------------------
combine_plots <- ggpubr::ggarrange(A, B, nrow = 2, labels = c("A", "B"), common.legend=TRUE, legend="right") 

combine_plots


#' 
#' # STEP 2: impute daily flight volume - Bluedot data
#' ## function to go from monthly Bluedot data to daily
## ------------------------------------------------------------------------

month_to_day <- function(filter_date, change_date_start, change_date_end, no_day){
                month <- bluedot %>% filter(yearMonth == filter_date) %>% select(-c(year, month, period))
                
                month$yearMonth <- change_date_start # change date
                month$totalVol2020 <- month$totalVol
                
                # change to daily df
                daily <- month[rep(seq_len(nrow(month)), each = no_day), ]
                
                # add dates
                daily$flight_date <- rep(seq(as.Date(change_date_start), as.Date(change_date_end), "days"), times=nrow(month))
                
                return(daily)
}

dec <- month_to_day("2018-12-01", "2019-12-01", "2019-12-31", 31)
jan <- month_to_day("2019-01-01", "2020-01-01", "2020-01-31", 31)
feb <- month_to_day("2019-02-01", "2020-02-01", "2020-02-28", 28)

daily_volume_final <- rbind(dec, jan, feb)


## STEP 2A: Calculate 3 month totals for each origin-destination pair
daily_volume_final$origin_dest <- paste0(daily_volume_final$origAirportCode, "_", daily_volume_final$destAirportCode)
daily_volume_monthly_distinct <- daily_volume_final %>% filter(yearMonth == flight_date) 
daily_volume_monthly_distinct$origin_dest <- paste0(daily_volume_monthly_distinct$origAirportCode, "_", daily_volume_monthly_distinct$destAirportCode)

origin_dest_2020Vol <- 
                daily_volume_monthly_distinct %>% 
                group_by(origin_dest) %>% 
                summarise(totalVol2020 = sum(totalVol2020))

daily_volume_final$totalVol2020_dec_feb <- origin_dest_2020Vol$totalVol2020[match(daily_volume_final$origin_dest, origin_dest_2020Vol$origin_dest)]

#' 
#' STEP 2B: Calculate % flight reduction in three month volume
## ------------------------------------------------------------------------
no_flight_2019 <- flightstats_filter %>% 
                filter(date >= "2018-12-01" & date <= "2019-02-28") %>% 
                group_by(departureAirportFsCode) %>% 
                summarize(n_2019=n()) 

no_flight_2020 <- flightstats_filter %>% 
                filter(date >= "2019-12-01" & date <= "2020-02-28") %>% 
                group_by(departureAirportFsCode) %>% 
                summarize(n_2020=n())


vol_reduc <- left_join(no_flight_2020, no_flight_2019, by = "departureAirportFsCode")
vol_reduc$janfeb_vol_reduc <- vol_reduc$n_2020/vol_reduc$n_2019 

# use no reduction for NAY/PKX as assumption 
# (new airport - have 0 flights in time period in 2019 because only domestic Chinese destinations for NAY airport)
vol_reduc$janfeb_vol_reduc[which(vol_reduc$departureAirportFsCode == "NAY/PKX")] <- 1

daily_volume_final$janfeb_vol_reduc <- vol_reduc$janfeb_vol_reduc[match(daily_volume_final$origAirportCode, vol_reduc$departureAirportFsCode)]

#' 
#' STEP 2C: Multiply STEPS 2A and 2B: three month volume * % reduction due to restrictions
## ------------------------------------------------------------------------
daily_volume_final$totalVol2020_dec_feb_reduc <- daily_volume_final$totalVol2020_dec_feb * daily_volume_final$janfeb_vol_reduc

#' 
#' STEP 2D: distribute into daily using daily flight departure proportions from 2020 
## ------------------------------------------------------------------------
china_origin_cities <- unique(bluedot$origAirportCode)

flightstats_prop_2020 <- data.frame()
for (i in 1:length(china_origin_cities)){
                df <- flightstats_filter %>% filter((date >= "2019-12-01" & date <= "2020-02-28") & departureAirportFsCode == china_origin_cities[i])
                tot_flights <- df %>% summarise(n=n()) %>% pull(n)
                prop_2020 <- as.data.frame(df %>% group_by(date) %>% summarise(prop_2020=n()/tot_flights))
                prop_2020$origin <- china_origin_cities[i]
                
                flightstats_prop_2020 <- rbind(flightstats_prop_2020, prop_2020)
}

flightstats_prop_2020 %>% group_by(origin) %>% summarise(sum(prop_2020)) # check if all 1s


# join proportions to df
daily_volume_final <- left_join(daily_volume_final, flightstats_prop_2020, by=c("origAirportCode" = "origin", "flight_date" = "date")) 

## make NA's 0 (represents prop_2020 == 0 because no flights on those dates)
daily_volume_final$prop_2020 <- ifelse(is.na(daily_volume_final$prop_2020), 0, daily_volume_final$prop_2020)

## estimate daily volume as daily prop * three month volume
daily_volume_final$dailyvol <- daily_volume_final$prop_2020*daily_volume_final$totalVol2020_dec_feb_reduc

sum(daily_volume_final$dailyvol)


#' 
#' ## export csv for all
## ------------------------------------------------------------------------
#write.csv(daily_volume_final, "./Flight Volume Estimation/Output/flights_adjusted_200323_v9.csv")


#' 
# daily # passengers estimated by origin city/airport
## ------------------------------------------------------------------------
daily_flights <- ggplot(data=(daily_volume_final), aes(x=flight_date, y=dailyvol, fill=factor(origAirportCode))) +
                geom_bar(stat = "identity") +
                theme_classic() +
                scale_x_date(breaks = "1 week", expand = c(0,0)) +
                scale_y_continuous(labels=comma, expand=c(0,0), limits = c(0,50000)) +
                geom_vline(xintercept=as.Date("2020-01-23")) + 
                labs(x = "Date", y = "Number of passengers (daily)", fill = "Origin Airport") + 
                theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1)) +
                guides(fill=guide_legend(ncol=2))

ggsave("./Figures and Results/Output/daily_estimated_flights.png", plot = daily_flights, width = 15, height = 8, units = c("in"), dpi = 300)


#' 
#' ## plot for only African destinations
## ------------------------------------------------------------------------

dest_countries_test <- c('Mauritius','South Africa','Kenya','Egypt','Ethiopia','Morocco')
daily_volume_africa <- daily_volume_final %>% filter(destCtryName %in% dest_countries_test)

levels(daily_volume_africa$destCtryName)[levels(daily_volume_africa$destCtryName)=="Mauritius"] <- "Mauritania"

african_countries <- ggplot(data=(daily_volume_africa), aes(x=flight_date, y=dailyvol, fill=factor(destCtryName))) +
                geom_bar(stat = "identity") +
                theme_classic() +
                # scale_fill_brewer(palette = "Set2")+
                scale_x_date(breaks = "1 week", expand = c(0,0)) +
                scale_y_continuous(limits = c(0, 2000), labels=comma, expand=c(0,0)) +
                geom_vline(xintercept=as.Date("2020-01-23")) + 
                labs(x = "Date", y = "Number of passengers (daily)", fill = "Destination Country") + 
                theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1)) +
                guides(fill=guide_legend(ncol=2))

ggsave("./Figures and Results/Output/african_countries_flights.png", plot = african_countries, width = 15, height = 8, units = c("in"), dpi = 300)


#' 
#' ## specific numbers for manuscript
## ------------------------------------------------------------------------

# compare LNY period
jan_LNY_2019 <- flightstats_filter %>% filter(date >= "2019-01-10" & date <= "2019-01-22") %>% summarise(n=n()) %>% pull(n)
jan_LNY_2020 <- flightstats_filter %>% filter(date >= "2020-01-10" & date <= "2020-01-22") %>% summarise(n=n()) %>% pull(n) 

jan_LNY_2020/jan_LNY_2019 

# compare travel ban period
travel_ban_2019 <- flightstats_filter %>% filter(date >= "2019-01-23" & date <= "2019-02-28") %>% summarise(n=n()) %>% pull(n)
travel_ban_2020 <- flightstats_filter %>% filter(date >= "2020-01-23" & date <= "2020-02-28") %>% summarise(n=n()) %>% pull(n) #6275

1-(travel_ban_2020/travel_ban_2019)
