## Manuscript Title: Estimating internationally imported cases during the early COVID-19 pandemic
## Objective: Produce supplementary figure 1
## Date: 29 October 2020

daily_volume_final <- read_csv("./data/flights_adjusted_200506.csv")

#' ## final histogram - daily # passengers estimated by origin city/airport
## ------------------------------------------------------------------------


### 18 chinese cities (split volume in 2 airports)
iata_codes <- read.csv("./data/cn_iata_code_shortlist.csv")
daily_volume_final$origCity <- iata_codes$city[match(daily_volume_final$origAirportCode, iata_codes$iata_codes)]

## eventually append to this
flight_sub <- 
  daily_volume_final %>% 
  filter(origAirportCode != "SZX" & origAirportCode != "HGH")

## divide SZX volume airport by 2 since shared by 2 cities
SZX_half_vol <- daily_volume_final %>% 
  filter(origAirportCode == "SZX") %>% 
  mutate(dailyvol = dailyvol/2) 

SZX_half_vol_double <- SZX_half_vol[rep(1:nrow(SZX_half_vol), times = 2),]
SZX_half_vol_double$origCity <- c(rep("Shenzhen", times = nrow(SZX_half_vol)), 
                                  rep("Dongguan", times = nrow(SZX_half_vol)))

## divide HGH volume airport by 2 since shared by 2 cities
HGH_half_vol <- daily_volume_final %>% 
  filter(origAirportCode == "HGH") %>% 
  mutate(dailyvol = dailyvol/2) 

HGH_half_vol_double <- HGH_half_vol[rep(1:nrow(HGH_half_vol), times = 2),]
HGH_half_vol_double$origCity <- c(rep("Hangzhou", times = nrow(HGH_half_vol)), 
                                  rep("Jiaxing", times = nrow(HGH_half_vol)))

## append
flight_final <- rbind(flight_sub, SZX_half_vol_double, HGH_half_vol_double)

## rolling 7-day average for 2019 gray line
plot_2019 <- read_csv("./data/2019_flight_line.csv") %>%
  mutate(roll = zoo::rollmean(sum, k = 7, fill=NA, align="right")) 

# align factor order with manuscript figures
flight_final$origCity <- factor(flight_final$origCity, 
                                levels = c("Wuhan", "Jiaxing", "Nanchang", "Changsha",
                                           "Zhengzhou", "Hefei", "Fuzhou",  "Chongqing", 
                                           "Beijing", "Shenzhen", "Hangzhou", "Guangzhou", 
                                           "Shanghai", "Nanjing", "Xi'an", "Dongguan", 
                                           "Chengdu","Tianjin"))

flight_final$flight_date <- as.Date(flight_final$flight_date)
plot_2019$flight_date_2020 <- as.Date(plot_2019$flight_date_2020)

daily_flights <- 
  ggplot() +
  geom_bar(data=(flight_final), 
           aes(x=flight_date, y=dailyvol, fill=factor(origCity)), 
           stat = "identity") +
  scale_fill_manual(values=as.vector(pals::polychrome(length(unique(flight_final$origCity))))) + 
  geom_vline(xintercept=as.Date("2020-01-23"), colour = "blue", size=1.5) + 
  geom_vline(xintercept=as.Date("2020-01-10"), colour= "yellow", size=1.5) + 
  scale_x_date(breaks = "1 week", expand = c(0,0)) +
  scale_y_continuous(labels=comma, expand=c(0,0), limits = c(0,160000)) +
  labs(x = "Date", y = "Number of passengers (daily)", fill = "Origin City") + 
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_line(data=plot_2019, aes(x=flight_date_2020, y=roll), color="black" , size = 0.75) +
  guides(fill=guide_legend(ncol=2)) + 
  export_theme

ggsave("./figures/daily_estimated_flights.png", plot = daily_flights, width = 15, height = 8, units = c("in"), dpi = 300)


