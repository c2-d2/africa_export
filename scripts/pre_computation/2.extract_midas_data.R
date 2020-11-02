## 29th October 2020
## PRE-COMPUTATION SCRIPT B - extract MIDAS case count data and produce clean data file

setwd(main_wd)

## NOTE - NEED TO DOWNLOAD THE MIDAS GIT REPO FIRST. CHANGE THE FILE PATH HERE
setwd("~/Documents/GitHub/midas_covid/data/cases/china/daily_cases_chinacdc_EN/")


## List all files with data
files <- list.files()
files <- files[!(files %in% c("collection_metadata.csv","data_guide.csv"))]

all_dat <- NULL
all_dats <- NULL
## Only want to store province name and number of new diagnoses
subset_cols <- c("province","New diagnosis")

## Read in each file
for(file in files){
  tmp_dat <- read_csv(file)
  
  ## Case sensitive
  colnames(tmp_dat)[which(colnames(tmp_dat) == "Province")] <- "province"  
  tmp_dat <- tmp_dat[,subset_cols]
  
  ## Get date from filename
  tmp_dat$date <- str_split(file, pattern="_")[[1]][1]
  all_dats[[file]] <- tmp_dat
  all_dat <- bind_rows(all_dat, tmp_dat)
}
all_dat$date <- as.Date(all_dat$date)

## Need to match names given here to names that I'm using
name_keys <- read_csv(paste0(main_wd,"data/midas/province_names.csv"))

## Merge the data with name keys to get the name I need
colnames(all_dat) <- c("province_raw","n","date")
all_dat <- left_join(all_dat, name_keys)

## Only want provinces we're using
all_dat <- all_dat %>% filter(!(all_dat$province %in% c("Tibet", "Taiwan", "Hong Kong", "Macau")))

## Change factor levels and reorder by total number of cases
all_dat$province <- factor(all_dat$province, levels=c("Hubei", "Guangdong", "Henan", "Zhejiang", "Hunan", "Anhui", 
                                                         "Jiangxi", "Shandong", "Jiangsu", "Chongqing", "Sichuan", "Heilongjiang", 
                                                         "Beijing", "Shanghai", "Hebei", "Fujian", "Guangxi", "Shaanxi", 
                                                         "Yunnan", "Hainan", "Guizhou", "Tianjin", "Shanxi", "Liaoning", 
                                                         "Jilin", "Gansu", "Xinjiang", "Inner Mongolia", "Ningxia", "Qinghai"
))
all_dat <- all_dat %>% select(date, province, n)
colnames(all_dat)[2] <- "province_raw"

## Cutoff of 3rd march, as want before second increase
all_dat <- all_dat %>% filter(date <= "2020-03-03")

## Get data from 1st November 2019 to now, filling with zeros
## Also the 15th is missing, so add zeros for that day. Pretty much fine as way before majority of cases
## For most provinces
times_fill <- c(seq(as.Date("2019-11-01"),as.Date("2020-01-09"),by="1 day"),as.Date("2020-01-15"))
all_dat_fill <- as_tibble(expand.grid(date=times_fill,province_raw=unique(all_dat$province_raw),n=0))
all_dat_fill$province_raw <- factor(all_dat_fill$province_raw, levels=c("Hubei", "Guangdong", "Henan", "Zhejiang", "Hunan", "Anhui", 
                                                                        "Jiangxi", "Shandong", "Jiangsu", "Chongqing", "Sichuan", "Heilongjiang", 
                                                                        "Beijing", "Shanghai", "Hebei", "Fujian", "Guangxi", "Shaanxi", 
                                                                        "Yunnan", "Hainan", "Guizhou", "Tianjin", "Shanxi", "Liaoning", 
                                                                        "Jilin", "Gansu", "Xinjiang", "Inner Mongolia", "Ningxia", "Qinghai"
))


## Merge with dummy data, arrange and visualise
all_dat_final <- bind_rows(all_dat, all_dat_fill)
all_dat_final$province <- as.numeric(all_dat_final$province_raw)
all_dat_final <- all_dat_final  %>% arrange(province, date)
all_dat_final %>% ggplot() + geom_line(aes(x=date, y=n)) + facet_wrap(~province_raw, scales="free_y")
all_dat_final$date_full <- all_dat_final$date

times <- seq(as.Date("2019-11-01"),max(all_dat_final$date), by="1 day")

all_dat_final$date <- match(all_dat_final$date, times) - 1

## Save
write_csv(all_dat_final, paste0(main_wd,"data/midas/midas_data_final.csv"))



