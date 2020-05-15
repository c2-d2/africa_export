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
                mutate( R=`0`/`1`) # 0 - 5.3

# total number of predicted cases for Africa ------------------------------
mt %>% 
                filter(is_africa_d==1) %>% 
                mutate( imp_number=prevalence_o*fvolume_od*alpha ) %>% 
                # by scenario
                group_by(scenario) %>% 
                summarise( sum=sum(imp_number) ) # 8.8 - 110


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
