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
