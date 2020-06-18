## TITLE: backcalculation_show
## Description: Demonstrating how different time-varying delay distributions may be used to back-cast latent events from confirmed case data
## Author: James Hay (Center for Communicable Disease Dynamics, Harvard T.H. Chan School of Public Health)
## Date: 04 June 2020

set.seed(1234)

library(tidyverse)
library(patchwork)
library(ggthemes)

setwd("~/Documents/GitHub/africa_export/")
source("code/functions/prevalence_calculation.R")
source("code/testing_scripts/fit_delay_distributions.R")
source("code/functions/backcalculation_funcs.R")
Rcpp::sourceCpp("~/Documents/GitHub/africa_export/code/functions/prevalence_calculation_cpp.cpp")

## Read in fitted delay distributions. We have a gamma distribution for the confirmation delay
## going either forward or backward from each day of symptom onset or date of confirmation
forward_delay_dists <- read_csv("data/confirm_delay_dists_forward.csv")
backward_delay_dists <- read_csv("data/confirm_delay_dists_backward.csv")
forward_delay_dists <- forward_delay_dists %>% rename(date=date_onset_symptoms)
backward_delay_dists <- backward_delay_dists %>% rename(date=date_confirmation)

## applying ascertainment rates
## What's the probability of someone being reported, given that they have developed symptoms?
ascertainment_date_seq_1<-seq(as.Date('2019-10-01'),as.Date('2020-01-23'),by="day")
ascertainment_date_seq_2<-seq(as.Date('2020-01-24'),as.Date('2020-02-03'),by="day")
ascertainment_date_seq_3<-seq(as.Date('2020-02-04'),as.Date('2020-03-03'),by="day") # previously this assigned to ascertainment_date_seq_2

ascertainment_rect1 <- data.frame(xmin=as.Date("2020-01-01"),xmax=as.Date("2020-01-23"),ymin=0,ymax=10000)
ascertainment_rect2 <- data.frame(xmin=as.Date("2020-01-24"),xmax=as.Date("2020-02-03"),ymin=0,ymax=10000)
ascertainment_rect3 <- data.frame(xmin=as.Date("2020-02-04"),xmax=as.Date("2020-03-03"),ymin=0,ymax=10000)


ascertainment_rate_1=0.1
ascertainment_rate_2=0.5
ascertainment_rate_3=0.5

symptomatic_proportion <- 1

ascertainment_rates <- tibble(onset_date=c(ascertainment_date_seq_1,ascertainment_date_seq_2,ascertainment_date_seq_3),
                              ascertainment_rate = c(rep(ascertainment_rate_1, length(ascertainment_date_seq_1)),
                                                     rep(ascertainment_rate_2, length(ascertainment_date_seq_2)),
                                                     rep(ascertainment_rate_2, length(ascertainment_date_seq_3))))

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
incidence <- rpois(n_times,dpois(1:n_times,n_times/3) * 10000)
incidence_dat <- tibble(date=times, incidence=incidence)


## First, just looking at the incubation period we use here:
symptom_prob <- dlnorm(seq(0,21,by=0.1),incu_par1, incu_par2)
incu_dat <- tibble(time=seq(0,21,by=0.1),prob=symptom_prob)

incu_p <- ggplot(incu_dat) +
  geom_line(aes(x=time,y=prob)) +
  ylab("Probability of developing symptoms") +
  xlab("Days since infection") +
  theme_bw()
png("figures/incubation_period.png",height=4,width=7,res=300,units="in")
incu_p
dev.off()

## Next, look at the empirical distribution of delays from the China line list data
png("figures/changing_delay_forward.png",width=7,height=4,res=300,units="in")
changing_delay_by_onset
dev.off()

########################
## Forward simulation
########################
## Add in delays
incidence_dat_long <- incidence_dat %>% 
  mutate(incidence = incidence*symptomatic_proportion) %>%
  ## One line per person
  uncount(incidence) %>%
  ## Incubation periods - how long to wait until symptom onset?
  mutate(incu_delay=floor(rlnorm(n(), incu_par1, incu_par2)),
         onset_date=date + incu_delay) %>%
  left_join(ascertainment_rates) %>%
  ## Cannot see people after last date
  filter(onset_date <= "2020-03-03") %>%
  rename(infection_date=date,
         date=onset_date) %>%
  select(infection_date, incu_delay, date, ascertainment_rate) %>%
  ## Join with forward confirmation delay parameters
  left_join(forward_delay_dists %>% select(date, gamma_scale_forward, gamma_shape_forward)) %>%
  ## Confirmation delays
  mutate(confirm_delay = rdgamma(n(), gamma_shape_forward, scale=gamma_scale_forward),
         confirm_date = date + confirm_delay) %>%
  mutate(observed=rbinom(n(), 1, ascertainment_rate)) %>%
  ## Cannot see people after last date
  filter(confirm_date <= "2020-03-03")

## Look at line list data
forward_linelist <- incidence_dat_long %>% ggplot() + geom_histogram(aes(x=confirm_delay,fill=as.factor(observed)),binwidth=1) + facet_wrap(~date,scales="free_y")
backward_linelist <- incidence_dat_long %>% ggplot() + geom_histogram(aes(x=confirm_delay,fill=as.factor(observed)),binwidth=1) + facet_wrap(~confirm_date,scales="free_y")

## Tally to give infection incidence counts
incidence_dat_mod <- incidence_dat_long %>%
  select(infection_date) %>%
  group_by(infection_date) %>%
  tally() %>%
  rename(date = infection_date) %>%
  mutate(var="Infections")

## Tally to give infection incidence counts
incidence_dat_mod_obs <- incidence_dat_long %>%
  filter(observed==1) %>%
  select(infection_date) %>%
  group_by(infection_date) %>%
  tally() %>%
  rename(date = infection_date) %>%
  mutate(var="Infections")

## Tally to give onset incidence counts
onset_dat_mod <- incidence_dat_long %>%
  filter(observed==1) %>%
  select(date) %>%
  group_by(date) %>%
  tally() %>%
  rename(date = date) %>%
  mutate(var="Onsets")

## Tally to give onset incidence counts
onset_dat_mod_all <- incidence_dat_long %>%
  select(date) %>%
  group_by(date) %>%
  tally() %>%
  rename(date = date) %>%
  mutate(var="Onsets")

## Tally to give confirmation incidence counts
confirm_dat_mod <- incidence_dat_long %>%
  filter(observed==1) %>%
  select(confirm_date) %>%
  group_by(confirm_date) %>%
  tally() %>%
  rename(date = confirm_date) %>%
  mutate(var="Confirmations")

## Combine
all_dat_obs <- bind_rows(incidence_dat_mod_obs, onset_dat_mod, confirm_dat_mod)
all_dat <- bind_rows(incidence_dat_mod, onset_dat_mod_all)

## Plot mean confirmation delays looking forward or back over time
forward_delays <- incidence_dat_long %>% group_by(date) %>%
  summarise(delay=mean(confirm_delay))
backward_delays <- incidence_dat_long %>% group_by(confirm_date) %>%
  select(-observed) %>%
  summarise(delay=mean(confirm_delay)) %>%
  mutate(observed="All") %>%
  rename(date=confirm_date)
backward_delays_observed <- incidence_dat_long %>% group_by(confirm_date) %>%
  filter(observed == 1) %>%
  select(-observed) %>%
  summarise(delay=mean(confirm_delay)) %>%
  mutate(observed="Observed") %>%
  rename(date=confirm_date)

backward_delays_all <- bind_rows(backward_delays, backward_delays_observed)

## Plot mean confirmation delays looking forward or back over time
forward_delays_inf <- incidence_dat_long %>% group_by(infection_date) %>%
  summarise(delay=mean(incu_delay))
backward_delays_inf <- incidence_dat_long %>% group_by(date) %>%
  summarise(delay=mean(incu_delay)) %>%
  mutate(observed="All") %>%
  rename(date=date)
backward_delays_inf_observed <- incidence_dat_long %>% group_by(date) %>%
  filter(observed == 1) %>%
  select(-observed) %>%
  summarise(delay=mean(incu_delay)) %>%
  mutate(observed="Observed") %>%
  rename(date=date)
backward_delays_inf_all <- bind_rows(backward_delays_inf, backward_delays_inf_observed)
backward_delays_inf2 <- incidence_dat_long %>% group_by(confirm_date) %>%
  summarise(delay=mean(incu_delay)) %>%
  rename(date=confirm_date)


## Whole forward simulation
p1 <- ggplot(all_dat_obs) + 
  #geom_vline(xintercept=c(as.Date(c("2020-01-23","2020-02-03"))),linetype="dashed")+
  #geom_rect(data=ascertainment_rect1, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="red",alpha=0.1) +
  #geom_rect(data=ascertainment_rect2, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="orange",alpha=0.1) +
  #geom_rect(data=ascertainment_rect3, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="green",alpha=0.1) +
  geom_line(aes(x=date,y=n,col=var)) +
  geom_line(data=all_dat,aes(x=date,y=n,col=var),linetype="dashed") +
  #geom_line(data=incidence_dat,aes(x=date,y=incidence),linetype="dashed",col="red") +
  coord_cartesian(ylim=c(0,1000)) +
  scale_color_colorblind() +
  scale_x_date(limits=range(times)) +
  ylab("Case counts") +
  xlab("Date") +
  theme_bw() +
  theme(legend.position=c(0.8,0.7)) +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        title=element_text(size=6)) +
  labs(tag="A")
p1

forward_delay <- ggplot(forward_delays) +
  geom_vline(xintercept=c(as.Date(c("2020-01-23","2020-02-03"))),linetype="dashed")+
  geom_jitter(data=incidence_dat_long,aes(x=date,y=confirm_delay),size=0.1,width=0.1,height=0.1) +
  geom_line(aes(x=date, y=delay))+
  scale_x_date(limits=range(times)) +
  xlab("Date of symptom onset") +
  ylab("Delay until report") +
  theme_bw() +
  #scale_y_continuous(limits=c(0,25)) +
  ggtitle("Forward confirmation delays (aggregated by date of symptom onset)")+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        title=element_text(size=6)) +
  labs(tag="D")

backward_delay <- ggplot(backward_delays_all) +
  geom_vline(xintercept=c(as.Date(c("2020-01-23","2020-02-03"))),linetype="dashed")+
  geom_jitter(data=incidence_dat_long,aes(x=confirm_date,y=confirm_delay),size=0.1,width=0.1,height=0.1) +
  geom_line(aes(x=date, y=delay,col=as.factor(observed))) +
  scale_x_date(limits=range(times)) +
  xlab("Date of confirmation") +
  ylab("Delay from report") +
  theme_bw() +
  #scale_y_continuous(limits=c(0,25))+
  ggtitle("Backward confirmation delays (aggregated by date of report)")+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        title=element_text(size=6),
        legend.position=c(0.2,0.8)) +
  labs(tag="E")
backward_delay

forward_delay_inf <- ggplot(forward_delays_inf) +
  geom_vline(xintercept=c(as.Date(c("2020-01-23","2020-02-03"))),linetype="dashed")+
  geom_jitter(data=incidence_dat_long,aes(x=infection_date,y=incu_delay),size=0.1,width=0.1,height=0.1) +
  geom_line(aes(x=infection_date, y=delay))+
  scale_x_date(limits=range(times)) +
  xlab("Date of infection") +
  ylab("Incubation period") +
  theme_bw() +
  #scale_y_continuous(limits=c(0,25)) +
  ggtitle("Forward incubation period (aggregated by date of infection)")+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        title=element_text(size=6)) +
  labs(tag="B")

backward_delay_inf <- ggplot(backward_delays_inf) +
  geom_vline(xintercept=c(as.Date(c("2020-01-23","2020-02-03"))),linetype="dashed")+
  geom_jitter(data=incidence_dat_long,aes(x=date,y=incu_delay),size=0.1,width=0.1,height=0.1) +
  geom_line(aes(x=date, y=delay)) +
  scale_x_date(limits=range(times)) +
  xlab("Date of onset") +
  ylab("Incubation period") +
  theme_bw() +
  #scale_y_continuous(limits=c(0,25))+
  ggtitle("Backward incubation period (aggregated by date of onset)")+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        title=element_text(size=6)) +
  labs(tag="C")

backward_delay_inf2 <- ggplot(backward_delays_inf2) +
  geom_vline(xintercept=c(as.Date(c("2020-01-23","2020-02-03"))),linetype="dashed")+
  geom_jitter(data=incidence_dat_long,aes(x=confirm_date,y=incu_delay),size=0.1,width=0.1,height=0.1) +
  geom_line(aes(x=date, y=delay)) +
  scale_x_date(limits=range(times)) +
  xlab("Date of onset") +
  ylab("Incubation period") +
  theme_bw() +
  #scale_y_continuous(limits=c(0,25))+
  ggtitle("Backward incubation period (aggregated by date of onset)")+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        title=element_text(size=6)) +
  labs(tag="C")

main_p <- (p1 | p1) / (forward_delay_inf | forward_delay) / (backward_delay_inf | backward_delay)
main_p <- p1 / backward_delay / backward_delay_inf
main_p
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

## Subtract the overall mean
overall_mean_confirm <- mean(incidence_dat_long$confirm_delay)

onset_dat_back <- confirm_dat_mod %>% 
  left_join(forward_delays) %>%
  mutate(onset_date = date - ceiling(overall_mean_confirm)) %>% 
  group_by(onset_date) %>%
  summarise(onsets=sum(n)) %>%
  mutate(ver="Shift by overall mean")

incidence_dat_back <- onset_dat_back %>% 
  mutate(date = onset_date - ceiling(incu_mean)) %>% 
  group_by(date) %>%
  summarise(incidence=sum(onsets))  %>%
  mutate(ver="Shift by overall mean")

onset_dat_back_right <- confirm_dat_mod %>% 
  left_join(backward_delays) %>%
  mutate(onset_date = date - ceiling(delay)) %>% 
  group_by(onset_date) %>%
  summarise(onsets=sum(n))  %>%
  mutate(ver="Shift by correct backward mean")

incidence_dat_back_right <- onset_dat_back_right %>% 
  left_join(backward_delays_inf %>% rename(onset_date=date)) %>%
  mutate(date = onset_date - ceiling(delay)) %>% 
  group_by(date) %>%
  summarise(incidence=sum(onsets))%>%
  mutate(ver="Shift by correct backward mean")
  
onset_dat_back_all <- bind_rows(onset_dat_back, onset_dat_back_right)
incidence_dat_back_all <- bind_rows(incidence_dat_back, incidence_dat_back_right)

p2 <- ggplot() +
  #scale_y_continuous(limits=c(0,1000)) +
  geom_bar(data=confirm_dat_mod,aes(x=date,y=n),stat="identity") +
  scale_x_date(limits=range(times)) +
  geom_line(data=onset_dat_mod,aes(x=date,y=n),col="#56B4E9") +
  geom_line(data=onset_dat_back_all, aes(x=onset_date,y=onsets,col=ver),linetype="dashed") +
  scale_color_manual(values=c("#009E73","#CC79A7")) +
  xlab("Date") +
  ylab("Case counts") +
  theme_bw() +
  theme(legend.position=c(0.8,0.8),
        axis.text=element_text(size=6),
        axis.title = element_text(size=8))

p3 <- ggplot() +
  #scale_y_continuous(limits=c(0,1000)) +
  scale_x_date(limits=range(times)) +
  geom_line(data=incidence_dat_mod,aes(x=date,y=n),col="#E69F00") +
  geom_line(data=incidence_dat_back_all,aes(x=date,y=incidence,col=ver),linetype="dashed") +
  scale_color_manual(values=c("#009E73","#CC79A7"))  +
  xlab("Date") +
  ylab("Case counts") +
  ggtitle("Augmented infection onset curves") +
  theme_bw() +
  theme(legend.position=c(0.8,0.8))

main_p2 <- p1/p2/p3

png("figures/simulated_outbreak.png",height=8,width=8,res=300,units="in")
main_p
dev.off()

png("figures/reconstructed_onsets.png",height=4,width=7,res=300,units="in")
p2
dev.off()
png("figures/reconstructed_infections.png",height=4,width=7,res=300,units="in")
p3
dev.off()

################################
## Bootstrap version
################################
## Get overall confirmation delay distribution
use_max_delay <- min(as.numeric(start_date -  min(plot_dat$onset_date)), max_delay)

## Get the data for this period
tmp <- incidence_dat_long %>% filter(confirm_delay >= 0 & confirm_date <= start_date &
                                       confirm_date > end_date ) %>% 
  count(confirm_delay) %>% 
  mutate(confirm_delay=as.numeric(confirm_delay))
tmp <- tmp %>% mutate(start=use_dates[i])
x <- incidence_dat_long %>%
  pull(confirm_delay) %>% 
  as.numeric
max_delay <- max(x)

## Fit discretised gamma
mean_start <- mean(x)
var_start <- var(x)
overall_gamma <- optim(par=c(mean_start, var_start), fn=fit_gamma_discrete_own_normalized,dat=x,tmax=max_delay)
overall_gamma_mean <- overall_gamma$par[1]
overall_gamma_var <- overall_gamma$par[2]

overall_gamma_scale <- overall_gamma_var/overall_gamma_mean
overall_gamma_shape <- overall_gamma_mean/overall_gamma_scale

## Having a play with inflating the backwards histograms
incidence_dat_long %>% ggplot() + 
  geom_histogram(aes(x=confirm_delay,fill=as.factor(observed),y=..density..)) + 
  facet_wrap(~confirm_date,scales="free_y")

incidence_dat_long %>% 
  group_by(confirm_date, observed) %>% 
  count(confirm_delay) %>% 
  pivot_wider(names_from=observed,values_from=n) %>%
  rename(observed=`1`,unobserved=`0`) %>%
  mutate(observed=ifelse(is.na(observed), 0, observed),
         unobserved=ifelse(is.na(unobserved),0,unobserved)) %>%
  mutate(ratio = observed/unobserved) %>%
  ggplot() + geom_line(aes(x=confirm_delay,y=ratio)) + facet_wrap(~confirm_date)

tmax <- 32
dat1 <- incidence_dat_long %>% #filter(observed == 1) %>%
  rename(confirmation_delay=confirm_delay,onset_date=date,report_date=confirm_date)

all_tmp <- NULL
for(i in 1:1000){
  tmp <- incidence_dat_long %>% filter(observed == 1) %>%
    filter(confirm_date == "2020-01-30") %>%
    mutate(upsample=1+rnbinom(n(), 1, ascertainment_rate)) %>%
    uncount(upsample) %>%
    select(confirm_date, confirm_delay) %>%
    count(confirm_delay) %>%
    mutate(n=n/sum(n)) %>%
    mutate(i=i)
  all_tmp <- bind_rows(tmp, all_tmp)
}
greb <- all_tmp %>% group_by(confirm_delay) %>% 
  summarise(total_n=sum(n),median=median(n),lower_quant=quantile(n, c(0.025)),upper_quant=quantile(n, c(0.975)))
omg1 <- greb %>% ggplot() + 
  geom_ribbon(aes(x=confirm_delay,ymin=lower_quant,ymax=upper_quant),fill="red",alpha=0.25) +
  scale_x_continuous(limits=c(0,15)) +
  geom_line(aes(x=confirm_delay,y=median)) + scale_x_continuous(limits=c(0,15)) +
  scale_y_continuous(limits=c(0,0.4))

tmp_dat1 <- incidence_dat_long %>% filter(observed == 1) %>%
  filter(confirm_date == "2020-01-30") %>%
  count(confirm_delay) %>%
  mutate(n=n/sum(n)) %>%
  mutate(obs="Observed")
tmp_dat2 <- incidence_dat_long %>% 
  filter(confirm_date == "2020-01-30") %>% 
  count(confirm_delay)%>%
  mutate(n=n/sum(n)) %>%
  mutate(obs="All")
tmp_dat <- bind_rows(tmp_dat1, tmp_dat2)
omg <- tmp_dat %>% 
  ggplot() + 
  geom_bar(data=tmp_dat,aes(x=confirm_delay,y=n,fill=obs),stat="identity",position="dodge") +
  geom_ribbon(data=greb,aes(x=confirm_delay,ymin=lower_quant,ymax=upper_quant),fill="red",alpha=0.25) +
  geom_line(data=greb,aes(x=confirm_delay,y=median)) + 
  scale_x_continuous(limits=c(0,15)) +
  scale_y_continuous(limits=c(0,0.4))
omg



tmax <- 32
dat1 <- incidence_dat_long %>% filter(observed == 1) %>%
  sample_frac(1) %>%
  rename(confirmation_delay=confirm_delay,onset_date=date,report_date=confirm_date)



backwards_delays_this_res <- generate_gamma_distributions_backward(25, dat1, tmax,min_date=min(incidence_dat_long$confirm_date))
backwards_delays_this <- backwards_delays_this_res[[1]]

probs_incu <- discretized_lnorm_probs(incu_par1, incu_par2, tmax)
probs_overall_confirm <- gamma_discrete_own_normalized(seq(0,max_delay-1,by=1),overall_gamma_shape, overall_gamma_scale, tmax=max_delay)

repeats <- 100
tmp_all_onsets <- NULL
tmp_all_infections <- NULL
tmp_all_onsets_naive <- NULL
tmp_all_infections_naive <- NULL

## Precompute the sampling probabilities for each day
all_probs <- NULL
all_probs_empirical <- NULL
unique_dates <- unique(backwards_delays_this_res[[3]]$date_confirmation)
for(i in 1:nrow(backwards_delays_this)){
  max_delay_dat <- backwards_delays_this$max_delay_dat[i]
  gamma_shape_backward <- backwards_delays_this$gamma_shape_backward[i]
  gamma_scale_backward <- backwards_delays_this$gamma_scale_backward[i]
  all_probs[[i]] <- gamma_discrete_own_normalized(seq(0, max_delay_dat-1,by=1),gamma_shape_backward,gamma_scale_backward,max_delay_dat)
  all_probs_empirical[[i]] <- backwards_delays_this_res[[3]] %>% filter(date_confirmation==unique_dates[i]) %>% pull(prob)
}
precomputed_probs <- tibble(probs=all_probs,date_confirmation=backwards_delays_this$date_confirmation) %>% left_join(backwards_delays_this)
precomputed_probs_empirical <- tibble(probs=all_probs_empirical,date_confirmation=backwards_delays_this$date_confirmation) %>% 
  left_join(backwards_delays_this_res[[3]] %>% select(date_confirmation,max_delay) %>% unique())


confirm_dat_mod_tmp <- confirm_dat_mod %>% 
  left_join(precomputed_probs %>% rename(date=date_confirmation))
confirm_dat_mod_tmp <- confirm_dat_mod %>% 
  left_join(precomputed_probs_empirical %>% rename(date=date_confirmation))

for(i in 1:repeats) {
  if(i %% 10 == 0) print(i)
  dat1_subset <- dat1 %>% sample_frac(1,replace=TRUE) 
  dat1_subset <- dat1
  #backwards_delays_this_res <- generate_gamma_distributions_backward(25, dat1, tmax,min_date=min(incidence_dat_long$confirm_date))
  backwards_delays_empirical <- generate_empirical_distributions_backward(50, dat1, tmax,min_date=min(incidence_dat_long$confirm_date))
  
  ## Precompute the sampling probabilities for each day
  all_probs_empirical <- NULL
  unique_dates <- unique(backwards_delays_empirical$date_confirmation)
  for(i in 1:nrow(backwards_delays_this)){
    all_probs_empirical[[i]] <- backwards_delays_empirical %>% filter(date_confirmation==unique_dates[i]) %>% pull(prob)
  }
  precomputed_probs_empirical <- tibble(probs=all_probs_empirical,date_confirmation=backwards_delays_this$date_confirmation) %>% 
    left_join(backwards_delays_empirical %>% select(date_confirmation,max_delay) %>% unique(), by="date_confirmation")
  
  
  confirm_dat_mod_tmp <- confirm_dat_mod %>% 
    left_join(precomputed_probs_empirical %>% rename(date=date_confirmation),by="date")
  
  tmp <- confirm_dat_mod_tmp %>%
    uncount(n) %>%
    mutate(id=1:n()) %>%
    group_by(id) %>%
    mutate(
      delay = sample(seq(0, max_delay,by=1),size=n(),
                     prob=unlist(probs)),
      onset_date = date - delay)
  
  ## What's the probability of someone being reported, given that they have developed symptoms?
  date1 <- sample(seq(as.Date("2020-01-20"),as.Date("2020-01-25"),by="1 day"),1)
  date2 <- sample(seq(as.Date("2020-02-01"),as.Date("2020-02-06"),by="1 day"),1)
  date1 <- as.Date("2020-01-23")
  date2 <- as.Date("2020-02-03")
  ascertainment_date_seq_1<-seq(as.Date('2019-10-01'),date1,by="day")
  ascertainment_date_seq_2<-seq(date1+1,date2,by="day")
  ascertainment_date_seq_3<-seq(date2+1,as.Date('2020-03-03'),by="day") # previously this assigned to ascertainment_date_seq_2
  
  ascertainment_rate_1 <- rnorm(1,0.1,0.02)
  ascertainment_rate_2 <- rnorm(1, 0.5, 0.03)
  ascertainment_rate_3 <- rnorm(1, 0.5, 0.03)
  
  ascertainment_rate_1 <- 0.1
  ascertainment_rate_2 <- 0.5
  ascertainment_rate_3 <- 0.5
  
  symptomatic_proportion <- 1
  
  ascertainment_rates <- tibble(onset_date=c(ascertainment_date_seq_1,ascertainment_date_seq_2,ascertainment_date_seq_3),
                                ascertainment_rate = c(rep(ascertainment_rate_1, length(ascertainment_date_seq_1)),
                                                       rep(ascertainment_rate_2, length(ascertainment_date_seq_2)),
                                                       rep(ascertainment_rate_2, length(ascertainment_date_seq_3))))
  
  tmp_onsets <- tmp %>%
    group_by(onset_date) %>%
    tally() %>%
    left_join(ascertainment_rates %>% rename(onset_date=onset_date),by="onset_date") %>%
    rename(onsets=n) %>%
    mutate(onsets_inflate = onsets + rnbinom(n(), onsets,ascertainment_rate)) %>%
    mutate(i=i)
  
 
  tmp_infections <- tmp_onsets %>%
    uncount(onsets_inflate) %>%
    mutate(id=1:n()) %>%
    group_by(id) %>%
    mutate( incu_period = floor(incu_mean),
            infection_date = onset_date - incu_period) %>%
    group_by(infection_date) %>%
    tally() %>%
    rename(infections=n) %>%
    mutate(i = i)
  
  tmp_all_onsets <- bind_rows(tmp_all_onsets, tmp_onsets)
  tmp_all_infections <- bind_rows(tmp_all_infections, tmp_infections)
}

combined_augmented <- tmp_all_onsets %>% 
  group_by(onset_date) %>%
  rename(old=onsets,onsets=onsets_inflate) %>%
  summarise(lower_quant=quantile(onsets, c(0.025)),
            mid_quant1=quantile(onsets, c(0.25)),
         median=median(onsets),
         mean=mean(onsets),
         mid_quant2=quantile(onsets, c(0.75)),
         upper_quant = quantile(onsets, c(0.975))) %>%
  mutate(ver="Augment from daily delay distributions")

all_augmented <- bind_rows(combined_augmented)

n_onsets <- sum(onset_dat_mod$n)
n_onsets_median <- all_augmented %>% group_by(ver) %>% summarise(total_median=sum(median)) %>% ungroup() %>%
  mutate(scale_factor = n_onsets/total_median)
all_augmented <- all_augmented %>% left_join(n_onsets_median)

main_p3 <- ggplot(data=all_augmented) +
  geom_bar(data=confirm_dat_mod, aes(x=date,y=n),stat="identity") +
  geom_ribbon(aes(x=onset_date,ymin=lower_quant,ymax=upper_quant,fill=ver),alpha=0.25) +
  geom_ribbon(aes(x=onset_date,ymin=mid_quant1,ymax=mid_quant2,fill=ver),alpha=0.5) +
  geom_line(aes(x=onset_date, y=median,col=ver)) +
  #geom_line(data=onset_dat_mod,aes(x=date,y=n),size=1.5,col="#56B4E9") +
  geom_line(data=onset_dat_mod_all,aes(x=date,y=n),size=1.5,col="#56B4E9") +
  #coord_cartesian(ylim=c(0,2000)) +
  theme_bw() +
  facet_wrap(~ver,ncol=1) +
  scale_x_date(breaks="7 days") +
  theme(legend.position="bottom",
        axis.text.x=element_text(angle=45,hjust=1)) +
  ylab("Number of symptom onsets") +
  xlab("Date")
main_p3


combined_augmented_inf <- tmp_all_infections %>% 
  group_by(infection_date) %>%
  summarise(lower_quant=quantile(infections, c(0.025)),
            mid_quant1=quantile(infections, c(0.25)),
            median=median(infections),
            mean=mean(infections),
            mid_quant2=quantile(infections, c(0.75)),
            upper_quant = quantile(infections, c(0.975))) %>%
  mutate(ver="Augment from daily delay distributions")


all_augmented_infections <- bind_rows(combined_augmented_inf)#,combined_augmented_inf_naive)


n_infections <- sum(incidence_dat_mod$n)
n_infections_median <- all_augmented_infections %>% group_by(ver) %>% summarise(total_median=sum(median)) %>% ungroup() %>%
  mutate(scale_factor = n_infections/total_median)
all_augmented_infections <- all_augmented_infections %>% left_join(n_infections_median)

main_p4 <- ggplot(data=all_augmented_infections) +
  geom_bar(data=confirm_dat_mod, aes(x=date,y=n),stat="identity") +
  geom_line(data=incidence_dat,aes(x=date,y=incidence),size=2,col="#E69F00") +
  geom_ribbon(aes(x=infection_date,ymin=lower_quant,ymax=upper_quant,fill=ver),alpha=0.25) +
  geom_ribbon(aes(x=infection_date,ymin=mid_quant1,ymax=mid_quant2,fill=ver),alpha=0.5) +
  geom_line(aes(x=infection_date, y=median,col=ver)) +
  facet_wrap(~ver,ncol=1)+
  theme_bw() +
  scale_x_date(breaks="7 days") +
  theme(legend.position="bottom",
        axis.text.x=element_text(angle=45,hjust=1)) +
  ylab("Number of new infections") +
  xlab("Date")
main_p4


png("figures/augmented_fit_onsets.png",height=8,width=8,res=300,units="in")
main_p3
dev.off()
png("figures/augmented_fit_infections.png",height=8,width=8,res=300,units="in")
main_p4
dev.off()

