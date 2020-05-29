source("~/Documents/GitHub/africa_export/code/functions/prevalence_calculation.R")
source("code/testing_scripts/fit_delay_distributions.R")
Rcpp::sourceCpp("~/Documents/GitHub/africa_export/code/functions/prevalence_calculation_cpp.cpp")

library(tidyverse)
library(patchwork)
library(ggthemes)

## Read in fitted delay distributions. We have a gamma distribution for the confirmation delay
## going either forward or backward from each day of symptom onset or date of confirmation
forward_delay_dists <- read_csv("data/confirm_delay_dists_forward.csv")
backward_delay_dists <- read_csv("data/confirm_delay_dists_backward.csv")
forward_delay_dists <- forward_delay_dists %>% rename(date=date_onset_symptoms)
backward_delay_dists <- backward_delay_dists %>% rename(date=date_confirmation)

tmin <- as.Date("2020-01-01")
tmax <- as.Date("2020-03-03")
times <- seq(tmin, tmax, by="1 day")
n_times <- length(times)

## Some toy fake incidence
incu_par1 <- 1.621
incu_par2 <- 0.418
delay_sd <- 3
incu_mean <- exp(incu_par1 + (incu_par2)^2/2)

## Very toy incidence - something that looks like a poisson distribution scaled
incidence <- rpois(n_times,dpois(1:n_times,n_times/3) * 1000)
incidence_dat <- tibble(date=times, incidence=incidence)

########################
## Forward simulation
########################
## Add in delays
incidence_dat_long <- incidence_dat %>% 
  ## One line per person
  uncount(incidence) %>%
  ## Incubation periods
  mutate(incu_delay=floor(rlnorm(n(), incu_par1, incu_par2)),
         onset_date=date + incu_delay) %>%
  ## Cannot see people after last date
  filter(onset_date <= "2020-03-03") %>%
  rename(infection_date=date,
         date=onset_date) %>%
  select(infection_date, incu_delay, date) %>%
  ## Join with forward confirmation delay parameters
  left_join(forward_delay_dists %>% select(date, gamma_scale_forward, gamma_shape_forward)) %>%
  ## Confirmation delays
  mutate(confirm_delay = rdgamma(n(), gamma_shape_forward, scale=gamma_scale_forward),
         confirm_date = date + confirm_delay) %>%
  ## Cannot see people after last date
  filter(confirm_date <= "2020-03-03")

## Tally to give infection incidence counts
incidence_dat_mod <- incidence_dat_long %>%
  select(infection_date) %>%
  group_by(infection_date) %>%
  tally() %>%
  rename(date = infection_date) %>%
  mutate(var="Infections")

## Tally to give onset incidence counts
onset_dat_mod <- incidence_dat_long %>%
  select(date) %>%
  group_by(date) %>%
  tally() %>%
  rename(date = date) %>%
  mutate(var="Onsets")

## Tally to give confirmation incidence counts
confirm_dat_mod <- incidence_dat_long %>%
  select(confirm_date) %>%
  group_by(confirm_date) %>%
  tally() %>%
  rename(date = confirm_date) %>%
  mutate(var="Confirmations")

## Combine
all_dat <- bind_rows(incidence_dat_mod, onset_dat_mod, confirm_dat_mod)

## Plot mean confirmation delays looking forward or back over time
forward_delays <- incidence_dat_long %>% group_by(date) %>%
  summarise(delay=mean(confirm_delay))
backward_delays <- incidence_dat_long %>% group_by(confirm_date) %>%
  summarise(delay=mean(confirm_delay)) %>%
  rename(date=confirm_date)

## Whole forward simulation
p1 <- ggplot(all_dat) + 
  geom_line(aes(x=date,y=n,col=var)) +
  scale_y_continuous(limits=c(0,150)) +
  scale_color_colorblind() +
  scale_x_date(limits=range(times)) +
  ylab("Case counts") +
  xlab("Date") +
  theme_bw() +
  theme(legend.position=c(0.8,0.7)) +
  ggtitle("Full simulated dataset")

forward_delay <- ggplot(forward_delays) +
  geom_jitter(data=incidence_dat_long,aes(x=date,y=confirm_delay),size=0.1,width=0.1,height=0.1) +
  geom_line(aes(x=date, y=delay))+
  scale_x_date(limits=range(times)) +
  xlab("Date of symptom onset") +
  ylab("Delay until report") +
  theme_bw() +
  scale_y_continuous(limits=c(0,20)) +
  ggtitle("Forward delays (aggregated by date of symptom onset)")

backward_delay <- ggplot(backward_delays) +
  geom_jitter(data=incidence_dat_long,aes(x=confirm_date,y=confirm_delay),size=0.1,width=0.1,height=0.1) +
  geom_line(aes(x=date, y=delay)) +
  scale_x_date(limits=range(times)) +
  xlab("Date of confirmation") +
  ylab("Delay from report") +
  theme_bw() +
  scale_y_continuous(limits=c(0,20))+
  ggtitle("Backward delays (aggregated by date of report)")

main_p <- p1 / forward_delay / backward_delay

## Now do the backward simulation incorrectly - just subtract the means
onset_dat_back <- confirm_dat_mod %>% 
  left_join(forward_delays) %>%
  mutate(onset_date = date - floor(delay)) %>% 
  group_by(onset_date) %>%
  summarise(onsets=sum(n)) %>%
  mutate(ver="Wrong (sample from forward dist)")
incidence_dat_back <- onset_dat_back %>% 
  mutate(date = onset_date - floor(incu_mean)) %>% 
  group_by(date) %>%
  summarise(incidence=sum(onsets))  %>%
  mutate(ver="Wrong (sample from forward dist)")
onset_dat_back_right <- confirm_dat_mod %>% 
  left_join(backward_delays) %>%
  mutate(onset_date = date - floor(delay)) %>% 
  group_by(onset_date) %>%
  summarise(onsets=sum(n))  %>%
  mutate(ver="Right (sample from backward dist)")

incidence_dat_back_right <- onset_dat_back_right %>% 
  mutate(date = onset_date - floor(incu_mean)) %>% 
  group_by(date) %>%
  summarise(incidence=sum(onsets))%>%
  mutate(ver="Right (sample from backward dist)")
  
onset_dat_back_all <- bind_rows(onset_dat_back, onset_dat_back_right)
incidence_dat_back_all <- bind_rows(incidence_dat_back, incidence_dat_back_right)

p2 <- ggplot() +
  scale_y_continuous(limits=c(0,250)) +
  scale_x_date(limits=range(times)) +
  geom_line(data=onset_dat_mod,aes(x=date,y=n),col="#56B4E9") +
  geom_line(data=onset_dat_back_all, aes(x=onset_date,y=onsets,col=ver),linetype="dashed") +
  scale_color_manual(values=c("#009E73","#CC79A7")) +
  xlab("Date") +
  ylab("Case counts") +
  ggtitle("Augmented symptom onset curves") +
  theme_bw() +
  theme(legend.position=c(0.8,0.8))

p3 <- ggplot() +
  scale_y_continuous(limits=c(0,250)) +
  scale_x_date(limits=range(times)) +
  geom_line(data=incidence_dat_mod,aes(x=date,y=n),col="#E69F00") +
  geom_line(data=incidence_dat_back_all,aes(x=date,y=incidence,col=ver),linetype="dashed") +
  scale_color_manual(values=c("#009E73","#CC79A7")) + +
  xlab("Date") +
  ylab("Case counts") +
  ggtitle("Augmented infection onset curves") +
  theme_bw() +
  theme(legend.position=c(0.8,0.8))

main_p2 <- p1/p2/p3

png("figures/simulated_outbreak.png",height=8,width=8,res=300,units="in")
main_p
dev.off()

png("figures/reconstructed_outbreak.png",height=8,width=8,res=300,units="in")
main_p2
dev.off()


################################
## Bootstrap version
################################
confirm_dat_mod_tmp <- confirm_dat_mod %>% 
  left_join(backward_delay_dists)
confirm_dat_mod_tmp_wrong <-  confirm_dat_mod %>% 
  left_join(forward_delay_dists)

repeats <- 10000
tmp_all_onsets <- NULL
tmp_all_infections <- NULL
tmp_wrong_all_onsets <- NULL
tmp_wrong_all_infections <- NULL
for(i in 1:repeats) {
  tmp <- confirm_dat_mod_tmp %>%
    mutate(
      delay=rdgamma(n(), shape=gamma_shape_backward,scale=gamma_scale_backward),
      onset_date = date - delay,
      incu_period = floor(rlnorm(n(), incu_par1, incu_par2)),
      infection_date = onset_date - incu_period
      )
  tmp_onsets <- tmp %>%
    group_by(onset_date) %>%
    summarise(onsets=sum(n)) %>%
    mutate(i = i)
  
  tmp_infections <- tmp %>%
    group_by(infection_date) %>%
    summarise(infections=sum(n)) %>%
    mutate(i = i)
  
  tmp_all_onsets <- bind_rows(tmp_all_onsets, tmp_onsets)
  tmp_all_infections <- bind_rows(tmp_all_infections, tmp_infections)
  
  tmp_wrong <- confirm_dat_mod_tmp_wrong %>%
    mutate(
      delay=rdgamma(n(), shape=gamma_shape_forward,scale=gamma_scale_forward),
      onset_date = date - delay,
      incu_period = floor(rlnorm(n(), incu_par1, incu_par2)),
      infection_date = onset_date - incu_period) 
  
  tmp_wrong_onsets <- tmp_wrong %>%
    group_by(onset_date) %>%
    summarise(onsets=sum(n)) %>%
    mutate(i = i)
  
  tmp_wrong_infections <- tmp_wrong %>%
    group_by(infection_date) %>%
    summarise(infections=sum(n)) %>%
    mutate(i = i)
  
  tmp_wrong_all_onsets <- bind_rows(tmp_wrong_all_onsets, tmp_wrong_onsets)
  tmp_wrong_all_infections <- bind_rows(tmp_wrong_all_infections, tmp_wrong_infections)
}

combined_augmented <- tmp_all_onsets %>% 
  group_by(onset_date) %>%
  summarise(lower_quant=quantile(onsets, c(0.025)),
            mid_quant1=quantile(onsets, c(0.25)),
         median=median(onsets),
         mean=mean(onsets),
         mid_quant2=quantile(onsets, c(0.75)),
         upper_quant = quantile(onsets, c(0.975))) %>%
  mutate(ver="Right (sample from backward dist)")

combined_augmented_wrong <- tmp_wrong_all_onsets %>% 
  group_by(onset_date) %>%
  summarise(lower_quant=quantile(onsets, c(0.025)),
            mid_quant1=quantile(onsets, c(0.25)),
            median=median(onsets),
            mean=mean(onsets),
            mid_quant2=quantile(onsets, c(0.75)),
            upper_quant = quantile(onsets, c(0.975))) %>%
  mutate(ver="Wrong (sample from forward dist)")
all_augmented <- bind_rows(combined_augmented, combined_augmented_wrong)

n_onsets <- sum(onset_dat_mod$n)
n_onsets_median <- all_augmented %>% group_by(ver) %>% summarise(total_median=sum(median)) %>% ungroup() %>%
  mutate(scale_factor = n_onsets/total_median)
all_augmented <- all_augmented %>% left_join(n_onsets_median)

main_p3 <- ggplot(data=all_augmented) +
  geom_bar(data=confirm_dat_mod, aes(x=date,y=n),stat="identity") +
  geom_ribbon(aes(x=onset_date,ymin=lower_quant,ymax=upper_quant,fill=ver),alpha=0.25) +
  geom_ribbon(aes(x=onset_date,ymin=mid_quant1,ymax=mid_quant2,fill=ver),alpha=0.5) +
  geom_line(aes(x=onset_date, y=median,col=ver)) +
  #geom_line(aes(x=onset_date, y=median*scale_factor,col=ver),linetype="dotted",size=2) +
  geom_line(data=onset_dat_back_all, aes(x=onset_date,y=onsets,col=ver),linetype="dashed") +
  geom_line(data=onset_dat_mod,aes(x=date,y=n),size=1.5,col="#56B4E9") +
  facet_wrap(~ver,ncol=1)+
  scale_color_manual(values=c("#009E73","#CC79A7")) +
  scale_fill_manual(values=c("#009E73","#CC79A7")) +
  theme_bw() +
  scale_x_date(breaks="7 days") +
  ggtitle("Symptom onsets - comparison of shifting strategies. Dashed line shows subtracting mean") +
  theme(legend.position="bottom",
        axis.text.x=element_text(angle=45,hjust=1)) +
  ylab("Number of symptom onsets") +
  xlab("Date")

combined_augmented_inf <- tmp_all_infections %>% 
  group_by(infection_date) %>%
  summarise(lower_quant=quantile(infections, c(0.025)),
            mid_quant1=quantile(infections, c(0.25)),
            median=median(infections),
            mean=mean(infections),
            mid_quant2=quantile(infections, c(0.75)),
            upper_quant = quantile(infections, c(0.975))) %>%
  mutate(ver="Right (sample from backward dist)")

combined_augmented_inf_wrong <- tmp_wrong_all_infections %>% 
  group_by(infection_date) %>%
  summarise(lower_quant=quantile(infections, c(0.025)),
            mid_quant1=quantile(infections, c(0.25)),
            median=median(infections),
            mean=mean(infections),
            mid_quant2=quantile(infections, c(0.75)),
            upper_quant = quantile(infections, c(0.975))) %>%
  mutate(ver="Wrong (sample from forward dist)")
all_augmented_infections <- bind_rows(combined_augmented_inf, combined_augmented_inf_wrong)


n_infections <- sum(incidence_dat_mod$n)
n_infections_median <- all_augmented_infections %>% group_by(ver) %>% summarise(total_median=sum(median)) %>% ungroup() %>%
  mutate(scale_factor = n_infections/total_median)
all_augmented_infections <- all_augmented_infections %>% left_join(n_infections_median)

main_p4 <- ggplot(data=all_augmented_infections) +
  geom_line(data=onset_dat_mod,aes(x=date,y=n),size=1.5,col="#56B4E9") +
  geom_ribbon(aes(x=infection_date,ymin=lower_quant,ymax=upper_quant,fill=ver),alpha=0.25) +
  geom_ribbon(aes(x=infection_date,ymin=mid_quant1,ymax=mid_quant2,fill=ver),alpha=0.5) +
  geom_line(aes(x=infection_date, y=median,col=ver)) +
  #geom_line(aes(x=infection_date, y=median*scale_factor,col=ver)) +
  geom_line(data=incidence_dat_back_all, aes(x=date,y=incidence,col=ver),linetype="dashed") +
  geom_line(data=incidence_dat_mod,aes(x=date,y=n),size=2,col="#E69F00") +
  facet_wrap(~ver,ncol=1)+
  scale_color_manual(values=c("#009E73","#CC79A7")) +
  scale_fill_manual(values=c("#009E73","#CC79A7")) +
  theme_bw() +
  scale_x_date(breaks="7 days") +
  ggtitle("Infections - comparison of shifting strategies. Dashed line shows subtracting mean") +
  theme(legend.position="bottom",
        axis.text.x=element_text(angle=45,hjust=1)) +
  ylab("Number of new infections") +
  xlab("Date")


png("figures/augmented_fit_onsets.png",height=8,width=8,res=300,units="in")
main_p3
dev.off()
png("figures/augmented_fit_infections.png",height=8,width=8,res=300,units="in")
main_p4
dev.off()

