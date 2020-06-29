# Source: https://github.com/timktsang/covid19_casedef/blob/master/2020_03_10_figure3.R (timktsang)
library(ggplot2)
library(tidyverse)
library(data.table)

data <- read.csv("./data/Epidemic_curve_China.csv")
data <- data[-82,]
data$dateid <- 1:nrow(data)

data1 <- data[,c("date","dateid","wuhan.confirmed","hubeiexclwuhan.confirmed","chinaexclhubei.confirmed")]
#1/15 1/18 1/22 1/27 2/4 2/18
# 45   48   52   57   65  79

test <- read.csv("./data/pred_f3.csv")

## Name predictions and plot
colnames(test) <- c("wuhan_v1","wuhan_v1b","wuhan_v2","wuhan_v4","wuhan_v5",
                    "hubei_v1","hubei_v1b","hubei_v2","hubei_v4","hubei_v5",
                    "china_v1","china_v1b","china_v2","china_v4","china_v5"
                    )
## Melt and relabel location and case definition for plotting
predicted_dat <- as_tibble(test) %>% 
  mutate(t = 1:n()) %>% 
  pivot_longer(-t) %>%
  mutate(loc="Wuhan",
         loc=ifelse(name %like% "hubei", "Hubei", loc),
         loc=ifelse(name %like% "china","China (non-Hubei)",loc)) %>%
  mutate(case_def = "V1",
         case_def=ifelse(name %like% "v2", "V2", case_def),
         case_def=ifelse(name %like% "v4", "V4", case_def),
         case_def=ifelse(name %like% "v5", "V5", case_def)
         )
predicted_dat %>% ggplot() + 
  geom_line(aes(x=t,y=value,col=case_def)) + 
  theme_bw() +
  facet_wrap(~loc, ncol=1)

## Get fold-change in case definition changes relative to previous def
testp <- test
## How many more cases on this day under case definition X relative to def X-1?
testp <- testp[,c(2:5,7:10,12:15)] - testp[,c(2:5,7:10,12:15)-1] 

total_additional_cases_wuhan <- rowSums(testp[,1:4])
total_additional_cases_hubei <- rowSums(testp[,1:4 + 4])
total_additional_cases_china <- rowSums(testp[,1:4 + 8])

## Find proportion of additional cases attributed to each case definition
testp[,1:4] <- testp[,1:4]/total_additional_cases_wuhan
testp[,1:4+4] <- testp[,1:4+4]/total_additional_cases_wuhan
testp[,1:4+8] <- testp[,1:4+8]/total_additional_cases_china
#pred<- as.matrix(test)
#test <- pred

#date.lab.day <- c(1:31,1:31,1:28)
#date.lab.day[1:length(date.lab.day)%%7!=1] <- NA
#date.lab.day[31+23] <- 23

#layout(matrix( 1:3, ncol=1,byrow=T))

#par(fig=c(0,1,0,1),mar=c(5,5,4,0),mfrow=c(3,1))

#max.y1=1
bar.hwid=0.45

#plot(NA, xlim=c(0,81), ylim=c(0,8000), axes=F, ann=F)
#axis(1, at=0:81-0.5, lab=NA, pos=-0.5, padj=-0.5,tick=F) 
#axis(1, at=c(c(-1,30,61,81)), lab=NA, pos=-0.5, padj=-0.5)
#axis(1, at=0:81-0.5, lab=date.lab.day[0:81+1], pos=0, padj=-0.5, tick=F)
#axis(1, at=55:70, lab=c(NA,"Feb",rep(NA,14)), pos=0, padj=1.3, tick=F)
#axis(2, at=0:8*1000, las=1, pos=-1.5)

#mtext('Dec, 2019', 1, line=2,at=29/2)
#mtext('Jan, 2020', 1, line=2,at=30+15.5)
#mtext('Feb, 2020', 1, line=2,at=61+10)

## extract Wuhan observed & estimated case counts (Figure 3A)
date_cases_wuhan=data.frame(matrix(nrow=81,ncol=4))
colnames(date_cases_wuhan)<-c("date_lb","date_ub","num_observed","num_estimated_5")

xvec <- 1:81-0.5

for (i in 1:length(xvec)){
  #polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
        #  c(0,rep(data1[i,3],2), 0), col='dodgerblue2', border=F)	 #dodgerblue2
  # ADD: indices for lower and upper x bounds for bar 
  date_cases_wuhan[i,1:2]=c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2))[c(1,3)]
  # ADD: height of bar, or # of observed cases
  date_cases_wuhan[i,3]=c(0,rep(data1[i,3],2), 0)[2]
  if (test[i,5]>test[i,4]&test[i,5]>data1[i,3]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    #polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
           # c(max(data1[i,3],test[i,4]),rep(test[i,5],2), max(data1[i,3],test[i,4])), col=rgb(0,0,0,0.2), border=F)	 #dodgerblue2
  }
  # ADD: indices for lower and upper height of bar, or # of cases estimated under case definition #5
  height1=c(max(data1[i,3],test[i,4]),rep(test[i,5],2), max(data1[i,3],test[i,4]))[1]
  height2=c(max(data1[i,3],test[i,4]),rep(test[i,5],2), max(data1[i,3],test[i,4]))[2]
  # ADD: if height1 > height2, # of observed cases is equal to # of estimated cases
  # else: use full height of bar
  height_final=ifelse(height1>height2,height1,height2)
  date_cases_wuhan[i,4]=height_final
}

# ADD: calculate ascertainment rate as ratio of observed and cases estimated under version 5 case definition
date_cases_wuhan$asct=date_cases_wuhan$num_observed/date_cases_wuhan$num_estimated_5
# ADD: if more observed cases than cases estimated under version 5 case definition, set ascertainment rate equal to 1 
#date_cases_wuhan$asct=ifelse(date_cases_wuhan$asct>1,1,date_cases_wuhan$asct)


## extract all provinces != Hubei observed & estimated case counts (Figure 3A)
date_cases_rest=data.frame(matrix(nrow=81,ncol=4))
colnames(date_cases_rest)<-c("date_lb","date_ub","num_observed","num_estimated_5")

for (i in 1:length(xvec)){
 # polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
         # c(0,rep(data1[i,3+2],2), 0), col='dodgerblue2', border=F)	 #dodgerblue2
  # ADD: indices for lower and upper x bounds for bar 
  date_cases_rest[i,1:2]=c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2))[c(1,3)]
  # ADD: height of bar, or # of observed cases
  date_cases_rest[i,3]=c(0,rep(data1[i,3+2],2), 0)[2]
  if (test[i,5+10]>test[i,4+10]&test[i,5+10]>data1[i,3+2]){
    # compute the proportion for version 2, 4 and 5 (red, orange, black)
    #polygon(c(rep(xvec[i]-bar.hwid,2),rep(xvec[i]+bar.hwid,2)), 
          #  c(max(data1[i,3+2],test[i,4+10]),rep(test[i,5+10],2), max(data1[i,3+2],test[i,4+10])), col=rgb(0,0,0,0.2), border=F)	 #dodgerblue2
  }
  # ADD: indices for lower and upper height of bar, or # of cases estimated under case definition #5
  height1=c(max(data1[i,3+2],test[i,4+10]),rep(test[i,5+10],2), max(data1[i,3+2],test[i,4+10]))[1]
  height2=c(max(data1[i,3+2],test[i,4+10]),rep(test[i,5+10],2), max(data1[i,3+2],test[i,4+10]))[2]
  # ADD: if height1 > height2, # of observed cases is equal to # of estimated cases
  # else: use full height of bar
  height_final=ifelse(height1>height2,height1,height2)
  date_cases_rest[i,4]=height_final
}

################### ALL BELOW IS ADD
# calculate ascertainment rate as ratio of observed and cases estimated under version 5 case definition
date_cases_rest$asct=date_cases_rest$num_observed/date_cases_rest$num_estimated_5
# if more observed cases than cases estimated under version 5 case definition, set ascertainment rate equal to 1 
#date_cases_rest$asct=ifelse(date_cases_rest$asct>1,1,date_cases_rest$asct)

plot(date_cases_wuhan$asct,type='l')
lines(date_cases_rest$asct,col="red")

## DAILY ASCERTAINMENT RATE RATIO
# combined ascertainment rate for wuhan and for all provinces != Hubei
asct_both=cbind.data.frame(date_cases_wuhan$date_lb,
                           date_cases_wuhan$date_ub, date_cases_wuhan$asct,date_cases_rest$asct)
# ascertainment rate ratio = ascertainment rate all other / ascertainment rate wuhan
# if ascertainment rate = 0 for both, ratio = 1
asct_both$asct_ratio=ifelse(asct_both$`date_cases_wuhan$asct`==0 & asct_both$`date_cases_rest$asct`==0, 1,
                       asct_both$`date_cases_rest$asct`/asct_both$`date_cases_wuhan$asct`)
colnames(asct_both)=c("date_lb","date_ub","asct_wuhan","asct_rest","asct_ratio")

# calculate mid date
asct_both$date_mid=((asct_both$date_ub-asct_both$date_lb)/2)+asct_both$date_lb

# plot ascertainment rate ratio
ggplot(asct_both,aes(x=date_mid,y=asct_ratio))+
  geom_line() +
  xlab("date")+
  ylab("ascertainment rate ratio")
ggsave("daily_ascertainment_rate_ratio_plot.pdf")

## ASCERTAINMENT RATE PRE-VERSION 2 CASE DEFINITION AND POST-VERSION 2 CASE DEFINITION

# calculate mid date 
# define an indicator for pre vs post- version 2 of the case definition
date_cases_wuhan$date_mid=((date_cases_wuhan$date_ub-date_cases_wuhan$date_lb)/2)+date_cases_wuhan$date_lb
date_cases_wuhan$indicator=ifelse(date_cases_wuhan$date_mid>51,1,0)
date_cases_rest$date_mid=((date_cases_rest$date_ub-date_cases_rest$date_lb)/2)+date_cases_rest$date_lb
date_cases_rest$indicator=ifelse(date_cases_rest$date_mid>51,1,0)

# summarize version 5 estimated & observed cases by period indicator (for Wuhan)
date_cases_wuhan_summary<-date_cases_wuhan%>%
  group_by(indicator)%>%
  summarise(sum_num_observed=sum(num_observed),sum_num_estimated_5=sum(num_estimated_5))%>%
  mutate(asct=sum_num_observed/sum_num_estimated_5)

# summarize version 5 estimated & observed cases by period indicator (for all provinces != Hubei)
date_cases_rest_summary<-date_cases_rest%>%
  group_by(indicator)%>%
  summarise(sum_num_observed=sum(num_observed),sum_num_estimated_5=sum(num_estimated_5))%>%
  mutate(asct=sum_num_observed/sum_num_estimated_5)

date_cases_both_summary<-cbind.data.frame(date_cases_rest_summary$indicator,
                                          date_cases_wuhan_summary$asct,
                                          date_cases_rest_summary$asct)
colnames(date_cases_both_summary)<-c("indicator","asct_wuhan","asct_rest")

# divide ascertainment rate for rest by ascertainment rate for wuhan
date_cases_both_summary$asct_ratio=date_cases_both_summary$asct_rest/date_cases_both_summary$asct_wuhan

# assign corresponding ascertainment rate ratio to each day of cases
date_cases_both_by_day=data.frame(matrix(nrow=81,ncol=2))
date_cases_both_by_day[,1]<-date_cases_wuhan$date_mid
date_cases_both_by_day[,2]<-ifelse(date_cases_wuhan$date_mid>51,
                date_cases_both_summary$asct_ratio[which(date_cases_both_summary$indicator==1)],
                date_cases_both_summary$asct_ratio[which(date_cases_both_summary$indicator==0)])
colnames(date_cases_both_by_day)<-c("date_mid","asct_ratio")
# plot
ggplot(date_cases_both_by_day,aes(x=date_mid,y=asct_ratio))+geom_line()+xlab("date")+ylab("ascertainment rate ratio")
ggsave("two_periods_ascertainment_rate_ratio_plot.pdf")

## ASCERTAINMENT RATE IN EACH PERIOD OF CASE DEFINITIONS (UP TO VERSION 5)

# define an indicator for each period of case definitions (Wuhan)
for (i in 1:nrow(date_cases_wuhan)){
  if(date_cases_wuhan$date_mid[i]<=47){
    date_cases_wuhan$indicator_2[i]=1
  }
  else if (date_cases_wuhan$date_mid[i]>47&date_cases_wuhan$date_mid[i]<=51) {
    date_cases_wuhan$indicator_2[i]=2
  }
  else if (date_cases_wuhan$date_mid[i]>51&date_cases_wuhan$date_mid[i]<=56) {
    date_cases_wuhan$indicator_2[i]=3
  }
  else if (date_cases_wuhan$date_mid[i]>56&date_cases_wuhan$date_mid[i]<=64) {
    date_cases_wuhan$indicator_2[i]=4
  }
  else{
    date_cases_wuhan$indicator_2[i]=5
  }
}

# define an indicator for each period of cases definitions (all provinces != Hubei)
for (i in 1:nrow(date_cases_rest)){
  if(date_cases_rest$date_mid[i]<=47){
    date_cases_rest$indicator_2[i]=1
  }
  else if (date_cases_rest$date_mid[i]>47&date_cases_rest$date_mid[i]<=51) {
    date_cases_rest$indicator_2[i]=2
  }
  else if (date_cases_rest$date_mid[i]>51&date_cases_rest$date_mid[i]<=56) {
    date_cases_rest$indicator_2[i]=3
  }
  else if (date_cases_rest$date_mid[i]>56&date_cases_rest$date_mid[i]<=64) {
    date_cases_rest$indicator_2[i]=4
  }
  else{
    date_cases_rest$indicator_2[i]=5
  }
}

# summarize version 5 estimated & observed cases by period indicator (for Wuhan)
date_cases_wuhan_2_summary<-date_cases_wuhan%>%
  group_by(indicator_2)%>%
  summarise(sum_num_observed=sum(num_observed),sum_num_estimated_5=sum(num_estimated_5))%>%
  mutate(asct=sum_num_observed/sum_num_estimated_5)

# summarize version 5 estimated & observed cases by period indicator (for all provinces != Hubei)
date_cases_rest_2_summary<-date_cases_rest%>%
  group_by(indicator_2)%>%
  summarise(sum_num_observed=sum(num_observed),sum_num_estimated_5=sum(num_estimated_5))%>%
  mutate(asct=sum_num_observed/sum_num_estimated_5)

date_cases_both_2_summary<-cbind.data.frame(date_cases_rest_2_summary$indicator_2,
                                          date_cases_wuhan_2_summary$asct,
                                          date_cases_rest_2_summary$asct)
colnames(date_cases_both_2_summary)<-c("indicator","asct_wuhan","asct_rest")

# divide ascertainment rate for rest by ascertainment rate for wuhan
date_cases_both_2_summary$asct_ratio=date_cases_both_2_summary$asct_rest/date_cases_both_2_summary$asct_wuhan

# assign corresponding ascertainment rate ratio to each day of cases
date_cases_both_2_by_day=data.frame(matrix(nrow=81,ncol=2))
date_cases_both_2_by_day[,1]<-date_cases_wuhan$date_mid
colnames(date_cases_both_2_by_day)<-c("date_mid","asct_ratio")

for (i in 1:nrow(date_cases_rest)){
  if(date_cases_rest$date_mid[i]<=47){
    date_cases_both_2_by_day$asct_ratio[i]=date_cases_both_2_summary$asct_ratio[which(date_cases_both_2_summary$indicator==1)]
  }
  else if (date_cases_rest$date_mid[i]>47&date_cases_rest$date_mid[i]<=51) {
    date_cases_both_2_by_day$asct_ratio[i]=date_cases_both_2_summary$asct_ratio[which(date_cases_both_2_summary$indicator==2)]
  }
  else if (date_cases_rest$date_mid[i]>51&date_cases_rest$date_mid[i]<=56) {
    date_cases_both_2_by_day$asct_ratio[i]=date_cases_both_2_summary$asct_ratio[which(date_cases_both_2_summary$indicator==3)]
  }
  else if (date_cases_rest$date_mid[i]>56&date_cases_rest$date_mid[i]<=64) {
    date_cases_both_2_by_day$asct_ratio[i]=date_cases_both_2_summary$asct_ratio[which(date_cases_both_2_summary$indicator==4)]
  }
  else{
    date_cases_both_2_by_day$asct_ratio[i]=date_cases_both_2_summary$asct_ratio[which(date_cases_both_2_summary$indicator==5)]
  }
}

# plot
ggplot(date_cases_both_2_by_day,aes(x=date_mid,y=asct_ratio))+geom_line()+xlab("date")+ylab("ascertainment rate ratio")
ggsave("five_periods_ascertainment_rate_ratio_plot.pdf")



## Save table of ascertainment rates
dates <- as.Date(as.character(data$date),origin="01/01/2019",format="%d/%m/%Y")
start_date <- c(min(dates), as.Date(c("2020-01-15","2020-01-18","2020-01-22","2020-02-14")))
end_date <- c(as.Date(c("2020-01-17","2020-01-21","2020-01-26","2020-02-03")),max(dates))
date_cases_both_2_summary %>% mutate(start_date=start_date,end_date=end_date)

enumerated_asc_rates <- tibble(date=seq(as.Date("2019-11-01"), as.Date("2020-03-04"),by="1 day")) %>%
  mutate(indicator=5,
         indicator=ifelse(date < start_date[5], 4,indicator),
         indicator=ifelse(date < start_date[4], 3,indicator),
         indicator=ifelse(date < start_date[3], 2,indicator),
         indicator=ifelse(date < start_date[2], 1,indicator)
         ) %>%
  left_join(date_cases_both_2_summary)

write_csv(enumerated_asc_rates, "data/tsang_ascertainment_rates.csv")
