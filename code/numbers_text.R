library(tidyverse)
#library(tidylog) # have it on when writing new pipes, otherwise overload with messages

# compute numbers for text
mt <- read_csv("./data/master_table.csv",guess_max = Inf)
mt %>% mutate( fvolume_od = ifelse( is.na(fvolume_od), 0 , fvolume_od ) ) ->mt
# look at variables
mt %>% glimpse()

# global Ratio Wuhan/non-Wuhan --------------------------------------------
mt %>% 
  filter(is_global_d==1) %>% 
  mutate( force_imp=prevalence_o*fvolume_od ) %>% 
  # by scenario
  group_by(is_wuhan,scenario) %>% 
  summarise( sum_force_imp=sum(force_imp) ) %>% ungroup() %>% 
  #
  pivot_wider(names_from = is_wuhan, values_from = sum_force_imp) %>% 
  set_names( "scenario" , "non_W" , "W"  ) %>% 
  mutate( R=non_W/W,
          frac_W=W/(W+non_W),
          frac_nW=non_W/(W+non_W)) %>% 
  select(-non_W,-W) # R: 0.5 - 5.33, frac_nW: 0.33 - 0.842

# Africa Ratio and fraction Wuhan/non-Wuhan -----------------------------------------
mt %>% 
  filter(is_africa_d==1) %>% 
  mutate( force_imp=prevalence_o*fvolume_od ) %>% 
  # by scenario
  group_by(is_wuhan,scenario) %>% 
  summarise( sum_force_imp=sum(force_imp) ) %>% ungroup() %>% 
  #
  pivot_wider(names_from = is_wuhan, values_from = sum_force_imp) %>% 
  set_names( "scenario" , "non_W" , "W"  ) %>% 
  mutate( R=non_W/W,
          frac_W=W/(W+non_W),
          frac_nW=non_W/(W+non_W)) %>% 
  select(-non_W,-W) # R: 0.9 - 10.2, frac_nW= 0.5 - 0.91

# total number of predicted cases for Africa ------------------------------
mt %>% 
  filter(is_africa_d==1) %>% 
  mutate( imp_number=prevalence_o*fvolume_od*alpha ) %>% 
  # by scenario
  group_by(scenario) %>% 
  summarise( sum=sum(imp_number) ) # 8.8 - 110
# for individual countries
mt %>% 
  filter(is_africa_d==1) %>% 
  mutate( imp_number=prevalence_o*fvolume_od*alpha ) %>% 
  # by scenario
  group_by(destination_country,scenario) %>% 
  summarise( sum=sum(imp_number) ) %>% ungroup() %>% 
  group_by(destination_country) %>% mutate( mean_pred= mean(sum) ) %>% 
  arrange( desc(mean_pred) ) %>% print(n=Inf) # Egypt, SA, Kenya, Ethiopia


# date range (for majority of case arrival) ---------------------------------
mt %>% 
  filter(is_africa_d==1) %>% 
  #filter(scenario=="Scenario 1") %>% 
  mutate( imp_number=prevalence_o*fvolume_od*alpha ) %>% 
  #
  group_by(date,scenario) %>% 
  summarise( sum_daily=sum(imp_number) ) %>% ungroup() %>% 
  arrange( scenario,desc(sum_daily) ) %>% 
  group_by(scenario) %>% 
  #normalise
  mutate( sum_daily=sum_daily/sum(sum_daily) ) %>% 
  mutate( cumsum_daily=cumsum(sum_daily) ) %>% 
  mutate( in_interval=as.numeric(cumsum_daily<=0.90) ) -> pf
# visual inspection
# pf %>% 
#                 #
#                 ggplot( aes(x=date,y=sum_daily, col=as.factor(in_interval)) ) +
#                 geom_point(size=0.6) +
#                 facet_wrap(~scenario,ncol=1)
pf                 %>% filter( in_interval==1 ) %>% 
  summarise( int_start=min(date),
             int_end=max(date)) 

# Prevalence estimates for the 5 scenarios --------------------------------
prevalence_dat <- mt %>% 
  select(origin_city, scenario, date, prevalence_o) %>% 
  distinct()

## Peak time overall
prevalence_dat %>%
  group_by(origin_city, scenario) %>%
  filter(prevalence_o == max(prevalence_o)) %>%
  group_by(scenario) %>%
  summarise(min_date=min(date),
            max_date=max(date)) 

## Peak prevalence in Jiaxing
prevalence_dat %>%
  filter(origin_city == "Jiaxing") %>%
  group_by(scenario) %>%
  filter(prevalence_o == max(prevalence_o)) %>%
  mutate(prevalence_percentage = prevalence_o * 100) %>%
  ungroup() %>%
  summarise(min_prev = min(prevalence_percentage),
            max_prev = max(prevalence_percentage))

## Peak prevalence in Shanghai
prevalence_dat %>%
  filter(origin_city == "Shanghai") %>%
  group_by(scenario) %>%
  filter(prevalence_o == max(prevalence_o)) %>%
  mutate(prevalence_percentage = prevalence_o * 100) %>%
  ungroup() %>%
  summarise(min_prev = min(prevalence_percentage),
            max_prev = max(prevalence_percentage))

## Peak prevalence in Tianjin
prevalence_dat %>%
  filter(origin_city == "Tianjin") %>%
  group_by(scenario) %>%
  filter(prevalence_o == max(prevalence_o)) %>%
  mutate(prevalence_percentage = prevalence_o * 100) %>%
  ungroup() %>%
  summarise(min_prev = min(prevalence_percentage),
            max_prev = max(prevalence_percentage))
