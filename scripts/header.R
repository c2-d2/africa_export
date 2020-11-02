## 29th October 2020
## HEADER SCRIPT
## - Run this before running all other scripts. It loads all required libraries, 
## - sets the global working directory and sets some global settings
## - NOTE: change the `main_wd` object to the full file path of the git repo

## All packages needed
Packages <- c("bbmle", "Bolstad2", "broom", "caret", "cartogram", 
              "data.table", "deSolve", "dplyr", "dvmisc", "fitdistrplus", "formattable", 
              "gam", "ggmap", "ggplot2", "ggpubr", "ggthemes", "grid", "gridExtra", 
              "janitor", "kableExtra", "knitr", "leaflet", "lubridate", "maptools", 
              "pals", "patchwork", "plotrix", "pracma", "psych", "purrr", "Rcpp","RColorBrewer", 
              "readr", "reshape2", "rgdal", "rio", "scales", "spData", "stats", 
              "stringi", "stringr", "tidyr", "tidyverse", "tmap", "tsiR", "zoo")

lapply(Packages, library, character.only = TRUE)

## Set main working directory
main_wd <- "~/Documents/Github/COVID_allchina_export/"
setwd(main_wd)

## Source auxiliary functions
source("./code/functions.R")

# settings
select <- dplyr::select
left_join <- dplyr::left_join
tibble.print_max <- 5
tibble.print_min <- 5

# plot theme
export_theme <- theme_tufte() + 
  theme(
    ## Axis text and titles
    axis.text.x = element_text(size=9,angle = 45, hjust = 1,family="sans"),
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
    strip.background=element_rect(fill="#f0f0f0"),
    ## Tags
    plot.tag=element_text(size=10,family="sans",face="bold",hjust=0,vjust=-3),
    plot.title=element_text(size=8,family="sans",hjust=0.5,face="bold",vjust=-3))

## Need all destinations
destination_countries <- c("Spain", "United States", "Algeria", "Nigeria", "United Kingdom", 
                           "Ethiopia", "Australia", "Netherlands", "Ghana", "New Zealand", 
                           "Cote D'Ivoire", "Russia", "Japan", "United Arab Emirates", "Malaysia", 
                           "Philippines", "Indonesia", "Sweden", "Germany", "Thailand", 
                           "Viet Nam", "Egypt", "Brazil", "Tanzania", "Senegal", "South Africa", 
                           "Guinea", "Argentina", "Korea (South)", "Morocco", "Hong Kong (SAR)", 
                           "Zimbabwe", "Congo (Kinshasa)", "Sudan", "Taiwan", "Angola", 
                           "Zambia", "Mauritius", "Cambodia", "Gabon", "Mauritania", "Madagascar", 
                           "Singapore", "Canada", "Chile", "Seychelles", "Equatorial Guinea", 
                           "Tunisia", "Kenya", "Macao (SAR)", "Uganda", "Mozambique")

highsurv_countries <-c('United States','Australia','Canada','Korea (South)',
                       'United Kingdom',
                       'Netherlands','Sweden',
                       'Germany','Spain','Singapore')
african_countries<-c('Mauritius','Mauritania','South Africa','Kenya','Egypt','Ethiopia','Morocco',
                     'Algeria','Nigeria','Ghana','Tanzania','Senegal','Guinea',
                     'Zimbabwe','Congo (Kinshasa)','Sudan','Angola','Zambia',
                     'Gabon','Madagascar','Equatorial Guinea','Tunisia',
                     'Uganda','Mozambique','Seychelles', "Cote D'Ivoire")

global_countries_old <- c("New Zealand", "Russia", "Japan", "United Arab Emirates", "Malaysia", 
                          "Philippines", "Indonesia", "Thailand", "Viet Nam", "Brazil", 
                          "Argentina", "Hong Kong (SAR)", "Taiwan", "Cambodia", "Chile", 
                          "Macao (SAR)") 
# new global countries
global_countries <- c("New Zealand","Australia", # only 2 destinations for Oceania
                      "United Kingdom","Germany","Russia", # top 3 dest in Europe (not Spain,Holland,Sweden)
                      "Japan","Thailand","Korea (South)", # top 3 dest in Asia
                      "United States","Canada", # only 2 in North America
                      "Brazil","Argentina","Chile", # top 3 in South America
                      "Egypt","Ethiopia","South Africa") # top 3 in Africa
countries_we_exclude <- c("United Arab Emirates","Malaysia","Philippines",
                          "Indonesia","Viet Nam","Hong Kong (SAR)",
                          "Taiwan","Cambodia","Macao (SAR)")


## Need all origins - each Chinese city
origin_cities <- c("Hefei", "Beijing", "Chongqing", "Fuzhou", "Guangzhou", "Dongguan", 
                   "Shenzhen", "Zhengzhou", "Wuhan", "Changsha", "Nanjing", "Nanchang", 
                   "Xi'an", "Shanghai", "Chengdu", "Tianjin", "Hangzhou", "Jiaxing")

