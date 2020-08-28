# read in cumulative cases in Hubei and in all other provinces, as of Feb 29
data_feb29=read.csv("./data/table_getAreaStat_en_2020-02-29.csv")
confirmed_cases_nh<-data_feb29%>%
  group_by(provinceShortName)%>%
  filter(provinceShortName!="Hubei")%>%
  summarise(cumulative_cases_province=sum(confirmedCount))%>%
  summarise(cumulative_cases_total=sum(cumulative_cases_province))

confirmed_cases_h<-data_feb29%>%
  group_by(provinceShortName)%>%
  filter(provinceShortName=="Hubei")%>%
  summarise(cumulative_cases_Hubei=sum(confirmedCount))

# read in cumulative deaths in Hubei and in all other provinces, as of Mar 19 (to account for 19 day delay from symptom onset to death)
data_mar13=read.csv("./data/table_getAreaStat_en_2020-03-13.csv")
confirmed_deaths_nh<-data_mar13%>%
  group_by(provinceShortName)%>%
  filter(provinceShortName!="Hubei")%>%
  summarise(cumulative_deaths_province=sum(deadCount))%>%
  summarise(cumulative_deaths_total=sum(cumulative_deaths_province))

confirmed_deaths_h<-data_mar13%>%
  group_by(provinceShortName)%>%
  filter(provinceShortName=="Hubei")%>%
  summarise(cumulative_deaths_Hubei=sum(deadCount))

# assumpe of equal infection infection fatality rates in provinces outside of Hubei and provinces within Hubei
IFR_nh=IFR_h=0.66 # Verity et al. estimate

# Extract total infections in Hubei and in all other provinces
total_infections_h=confirmed_deaths_h$cumulative_deaths_Hubei/IFR_h
total_infections_nh=confirmed_deaths_nh$cumulative_deaths_total/IFR_nh

# Compute the ascertainment rate ratio using total_infections as defined above
## define proportion symptomatic - to yield number of cases who are symptomatic
prop_symp= 0.22 # REF: https://www.bmj.com/content/369/bmj.m1375 

ARR=(confirmed_cases_nh$cumulative_cases_total/(total_infections_nh*prop_symp))/(confirmed_cases_h$cumulative_cases_Hubei/(total_infections_h*prop_symp))

####### CHECK: Direct computation - b/c IFRs cancel out
ARR=(confirmed_cases_nh$cumulative_cases_total/confirmed_deaths_nh$cumulative_deaths_total)/
(confirmed_cases_h$cumulative_cases_Hubei/confirmed_deaths_h$cumulative_deaths_Hubei)




