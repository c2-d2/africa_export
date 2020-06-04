library(tidyverse)
library(extraDistr)
library(patchwork)

setwd("~/Documents/GitHub/africa_export/")
source("code/functions/backcalculation_funcs.R")

tmin <- as.Date("2019-11-01")
tmax <- as.Date("2020-03-03")
times <- seq(tmin, tmax, by="1 day")
times_plot <- seq(as.Date("2020-01-01"),as.Date("2020-02-20"),by="1 day")

## Have a look at Zhang et al. data
dat <- read_csv("~/Documents/GitHub/covback/data/real/zhang_appendix_lancet.csv")
plot_dat <- dat %>% 
  group_by(Location) %>%
  mutate(report_date = as.Date(report_date, origin="2019/01/11"),
         onset_date= as.Date(onset_date, oprigin="2019/01/11"),
         report_delay = report_date - onset_date,
         stage=onset_date < as.Date("2020/01/27")) %>% 
  select(ID, onset_date, report_date,report_delay, stage) %>% drop_na()

## Need the maximum reporting delay to re-normalize the delay distributions later
max_delay <- as.numeric(max(plot_dat$report_delay))

changing_delay_by_report <-  ggplot(plot_dat,aes(x=report_date,y=report_delay)) + 
  geom_jitter(size=0.25,width = 0.1,height=0.1) +
  geom_smooth() +
  coord_cartesian(ylim=c(0,30)) +
  scale_x_date(breaks="7 days",limits=range(times_plot)) +
  geom_vline(xintercept=as.Date("2020-01-23",origin="2019-11-01"),col="green",size=0.5,linetype="dashed") +
  xlab("Date of report onset") +
  ylab("Delay from symptom onset to report") +
  ggtitle("Delay going backward. For a case reported on day t, how long ago did they report symptom onset?") +
  theme_bw()+
  theme(title=element_text(size=8))

## Fit a spline to observed reporting rate data, used for simulation later
spline_fit <- smooth.spline(as.numeric(plot_dat$report_delay) ~ as.numeric(plot_dat$report_date))
predict_x <- as.numeric(times)
predict_y <- predict(spline_fit, newdata=predict_x)
spline_dat <- data.frame(dates=as.Date(predict_y$x,origin="1970-01-01"),mean_delay=predict_y$y)
write_csv(spline_dat,"data/spline_dat.csv")

changing_delay_by_onset <- 
  ggplot(plot_dat,aes(x=onset_date,y=report_delay)) + 
  geom_jitter(size=0.25,width = 0.1,height=0.1) +
  geom_smooth() +
  coord_cartesian(ylim=c(0,30)) +
  scale_x_date(breaks="7 days",limits=range(times_plot)) +
  geom_vline(xintercept=as.Date("2020-01-23",origin="2019-11-01"),col="green",size=0.5,linetype="dashed") +
  xlab("Date of symptom onset") +
  ylab("Delay from symptom onset to report") +
  ggtitle("Delay going forward. For a symptom onset on day t, how long did it take to get confirmed?") +
  theme_bw() +
  theme(title=element_text(size=8))

########################
## SIMPLE ILLUSTRATION WITH FIXED DELAY DISTRIBUTIONS
ts <- 0:10
toy_inc <- 100*exp(ts * 0.25)

toy_mean <- 3
toy_var <- 2
toy_scale <- toy_var/toy_mean
toy_shape <- toy_mean/toy_scale

toy_inc_period <- tibble(y=ddgamma(0:25,shape=toy_shape,scale=toy_scale)/pdgamma(25,shape=toy_shape,scale=toy_scale),t=0:25)

toy_report <- tibble(date_infection=ts,n=floor(toy_inc)) %>%
  uncount(n) %>%
  mutate(report_delay=rdgamma(n(), shape=toy_shape,scale=toy_scale),
         report_date=date_infection+report_delay)

toy_report_dat <- toy_report %>% select(date_infection) %>% group_by(date_infection) %>% 
  tally() %>%
  rename(date=date_infection, n=n) %>%
  mutate(var="onsets") %>%
  bind_rows(
   toy_report %>% select(report_date) %>% group_by(report_date) %>% 
     tally() %>%
     rename(date=report_date, n=n) %>%
     mutate(var="reports")
  )

incu_plot <- ggplot(toy_inc_period)+
  geom_line(aes(x=t,y=y)) +
  theme_bw() +
  xlab("Report delay") +
  ylab("PMF")

simulated_p <- ggplot(data=toy_report_dat) +
  geom_line(aes(x=date,y=n,col=var)) +
  theme_bw() +
  xlab("Date") +
  ylab("Incidence") +
  theme(legend.position=c(0.8,0.8))

png("figures/simple_simulation.png",height=7,width=8,units="in",res=300)
incu_plot/simulated_p
dev.off()

toy_backward_scatter <- ggplot(toy_report,aes(x=report_date,y=report_delay)) + 
  geom_jitter(size=0.25) + 
  scale_x_continuous(limits=c(0,20)) +
  scale_y_continuous(breaks=seq(0,10,by=2)) +
  coord_cartesian(ylim=c(0,10)) +
  geom_smooth() +
  theme_bw() + xlab("Date of report") + ylab("Reporting delay")

toy_forward_scatter <- ggplot(toy_report,aes(x=date_infection,y=report_delay)) + 
  geom_jitter(size=0.25) + 
  scale_x_continuous(limits=c(0,20)) +
  scale_y_continuous(breaks=seq(0,10,by=2)) +
  coord_cartesian(ylim=c(0,10)) +
  geom_smooth() +
  theme_bw() + xlab("Date of onset") + ylab("Reporting delay")

png("figures/simple_simulation_scatters.png",height=7,width=8,units="in",res=300)
toy_forward_scatter/toy_backward_scatter
dev.off()

toy_forward <- ggplot(toy_report) + geom_histogram(aes(x=report_delay,y=..density..),binwidth=1) + 
  geom_line(data=toy_inc_period,aes(x=t,y=y),col="red") +
  facet_wrap(~date_infection,scales="free_y") +
  theme_bw()
toy_backward <- ggplot(toy_report) + geom_histogram(aes(x=report_delay,y=..density..),binwidth=1) + 
  geom_line(data=toy_inc_period,aes(x=t,y=y),col="red") +
  facet_wrap(~report_date,scales="free_y") +
  theme_bw()

toy_forward/toy_backward
###############################

##############################################################
## NOW ONTO FITTING THE DELAY DISTRIBUTIONS
##############################################################

## Remove NAs and negative confirmation delays
dat <- dat %>% select(onset_date, report_date)
dat <- dat %>% mutate(onset_date = as.Date(onset_date,origin="2019-11-01"),
              report_date = as.Date(report_date, origin="2019-11-01"),
              confirmation_delay = as.numeric(report_date - onset_date)) %>%
  drop_na() %>%
  filter(confirmation_delay >= 0)


## Sliding confirmation date windows
## Generate a range of dates as a sliding window
range(dat$confirmation_delay,na.rm=TRUE)

## Minimum number of observations for fitting
threshold <- 10

################################
## 1) PROBABILITIES GOING BACKWARD
## -- What's the probability that you developed symptoms on day t-x given confirmation on day t?
## Get number of new confirmations on each day
number_confirmed <- dat %>% filter(confirmation_delay > 0) %>% group_by(report_date) %>% tally()

## Store generated counts and geometric probabilities for each day
all_dat_backward <- NULL

## For each day, go back in time until the number of new confirmations in that period
## is greater than threshold
first_date <- number_confirmed$report_date[which(cumsum(number_confirmed$n) > threshold)[1]]
use_dates <- times[times > first_date]
probs <- numeric(length(use_dates))
gamma_pars_backward <- NULL

model_probs_gamma_backward <- matrix(0,nrow=length(use_dates),ncol=max_delay+1)

## For each day of potential confirmation
for(i in seq_along(use_dates)) {
  counted <- 0
  start_date <- end_date <- use_dates[i]
  ## Go back in time and accumlate cases until >threshold cases
  while(counted < threshold & end_date > min(times)){
    tmp_count <- number_confirmed %>% filter(report_date == end_date) %>% pull(n)
    counted <- counted + max(tmp_count,0)
    end_date <- end_date - 1
  }
  
  use_max_delay <- min(as.numeric(start_date -  min(plot_dat$onset_date)), max_delay)
  
  ## Get the data for this period
  tmp <- dat %>% filter(confirmation_delay >= 0 & report_date <= start_date &
                          report_date > end_date ) %>% 
    count(confirmation_delay) %>% 
    mutate(confirmation_delay=as.numeric(confirmation_delay))
  tmp <- tmp %>% mutate(start=use_dates[i])
  x <- dat %>% filter(confirmation_delay >= 0 & report_date <= start_date &
                                 report_date > end_date) %>% 
    pull(confirmation_delay) %>% 
    as.numeric

  ## Fit discretised gamma
  mean_start <- mean(x)
  var_start <- var(x)
  #fit1 <- optim(par=c(5, 25), fn=fit_gamma_discrete,dat=x)
  fit1 <- optim(par=c(5, 25), fn=fit_gamma_discrete_own_normalized,dat=x,tmax=use_max_delay)
  gamma_pars_backward[[i]] <- data.frame("gamma_mean_backward"=fit1$par[1],"gamma_var_backward"=fit1$par[2],"max_delay"=use_max_delay,
                                         "date_confirmation"=use_dates[i], "n_used"=counted,
                                         "direction"="backward")
  
  all_dat_backward <- bind_rows(tmp, all_dat_backward)
  
  scale <- fit1$par[2]/fit1$par[1]
  shape <- fit1$par[1]/scale
  
  model_probs_gamma_backward[i,1:(use_max_delay+1)] <- ddgamma(0:use_max_delay, scale=scale, shape=shape,log=FALSE)/pdgamma(use_max_delay, scale=scale,shape=shape)
}
all_dat_backward <- all_dat_backward %>% complete(confirmation_delay, start, fill=list(n=0))

model_probs_gamma_backward <- reshape2::melt(model_probs_gamma_backward)
colnames(model_probs_gamma_backward) <- c("label", "confirmation_delay","prob")
waits <- seq(0,max_delay,by=1)
model_probs_gamma_backward$confirmation_delay <- waits[model_probs_gamma_backward$confirmation_delay]

model_probs_gamma_backward$confirmation_delay <- model_probs_gamma_backward$confirmation_delay
model_probs_gamma_backward$label <- use_dates[model_probs_gamma_backward$label]
model_probs_gamma_backward$label <- paste0("<=", model_probs_gamma_backward$label)
model_probs_gamma_backward$fit <- "Gamma"
all_dat_backward$label <- paste0("<=", all_dat_backward$start)
all_model_probs_backward <- model_probs_gamma_backward

## For each day, go back in time day-by-day until at least `threshold` new confirmed cases have happened.
## Use the case confirmations in this window to generate a confirmation delay distribution
## for that window.
all_dat_backward <- all_dat_backward %>% group_by(label) %>% mutate(rel_n = n/sum(n))
p_sliding_delays_backward <- ggplot(all_dat_backward) + 
  geom_bar(aes(x=confirmation_delay,y=rel_n),stat="identity") + 
  geom_line(data=all_model_probs_backward,aes(x=confirmation_delay,y=prob),col="red",size=1) +
  facet_wrap(~label,ncol=6) +
  coord_cartesian(xlim=c(0,max_delay)) +
  theme_bw() +
  xlab("Delay from confirmation to symptom onset (days)") +
  ylab("Count") +
  ggtitle("Confirmation delay distribution from day of confirmation for each window (backward)") +
  theme(legend.position=c(0.9,0.1))

## What discretised gamma parameters should be used for each date of confirmation?
gamma_pars_dat_backward <- do.call("rbind", gamma_pars_backward)

################################
## 2) PROBABILITIES GOING FORWARD
## -- What's the probability that you have been confirmed by day t given symptom onset on day t-x?

## Get number of new onsets on each day
number_onsets <- dat %>% filter(confirmation_delay > 0) %>% group_by(onset_date) %>% tally()

## Store generated counts and geometric probabilities for each day
all_dat_forward <- NULL

## Minimum number of observations for fitting
threshold <- 20

## For each day, go back in time until the number of new symptom onsets in that period
## is greater than threshold
first_date <- number_onsets$onset_date[which(cumsum(number_onsets$n) > threshold)[1]]
use_dates <- times[times > first_date]
probs <- numeric(length(use_dates))
gamma_pars_forward <- NULL

model_probs_gamma_forward <- matrix(nrow=length(use_dates),ncol=max_delay+1)

## For each day of potential confirmation
for(i in seq_along(use_dates)) {
  counted <- 0
  start_date <- end_date <- use_dates[i]
  ## Go back in time and accumlate cases until >threshold cases
  while(counted < threshold & end_date > min(times)){
    tmp_count <- number_onsets %>% filter(onset_date == end_date) %>% pull(n)
    counted <- counted + max(tmp_count,0)
    end_date <- end_date - 1
  }
  
  ## Get the data for this period
  tmp <- dat %>% filter(confirmation_delay > 0 & onset_date <= start_date &
                          onset_date > end_date ) %>% count(confirmation_delay) %>% mutate(confirmation_delay=as.numeric(confirmation_delay))
  tmp <- tmp %>% mutate(start=use_dates[i])
  x <- dat %>% filter(confirmation_delay > 0 & onset_date <= start_date &
                        onset_date > end_date) %>% pull(confirmation_delay) %>% as.numeric

  ## Fit discretised gamma
  mean_start <- mean(x)
  var_start <- var(x)
  fit1 <- optim(par=c(5, 25), fn=fit_gamma_discrete_own_normalized,dat=x,tmax=max_delay)
  gamma_pars_forward[[i]] <- data.frame("gamma_mean_forward"=fit1$par[1],"gamma_var_forward"=fit1$par[2],
                                        "max_delay"=max_delay,
                                        "date_onset_symptoms"=use_dates[i], "n_used"=counted,
                                        "direction"="forward")
  
  all_dat_forward <- bind_rows(tmp, all_dat_forward)
  
  scale <- fit1$par[2]/fit1$par[1]
  shape <- fit1$par[1]/scale
  
  model_probs_gamma_forward[i,] <- ddgamma(0:max_delay, scale=scale, shape=shape,log=FALSE)/pdgamma(max_delay, scale=scale,shape=shape)
}
all_dat_forward <- all_dat_forward %>% complete(confirmation_delay, start, fill=list(n=0))
model_probs_gamma_forward <- reshape2::melt(model_probs_gamma_forward)
colnames(model_probs_gamma_forward) <- c("label", "confirmation_delay","prob")
waits <- seq(0,max_delay,by=1)
model_probs_gamma_forward$confirmation_delay <- waits[model_probs_gamma_forward$confirmation_delay]
model_probs_gamma_forward$confirmation_delay <- model_probs_gamma_forward$confirmation_delay
model_probs_gamma_forward$label <- use_dates[model_probs_gamma_forward$label]
model_probs_gamma_forward$label <- paste0("<=", model_probs_gamma_forward$label)
model_probs_gamma_forward$fit <- "Gamma"
all_dat_forward$label <- paste0("<=", all_dat_forward$start)

all_model_probs_forward <- model_probs_gamma_forward

## For each day, go back in time day-by-day until at least 20 new confirmed cases have happened.
## Use the case confirmations in this window to generate a confirmation delay distribution
## for that window.

all_dat_forward <- all_dat_forward %>% group_by(label) %>% mutate(rel_n = n/sum(n))
p_sliding_delays_forward <- ggplot(all_dat_forward) + 
  geom_bar(aes(x=confirmation_delay,y=rel_n),stat="identity") + 
  geom_line(data=all_model_probs_forward,aes(x=confirmation_delay,y=prob),size=1,col="red") +
  geom_vline(xintercept=1,linetype="dashed") +
  coord_cartesian(xlim=c(0,40)) +
  facet_wrap(~label,ncol=6,scales="free_y") +
  theme_bw() +
  xlab("Delay from symptom onset to confirmation (days)") +
  ylab("Density") +
  ggtitle("Confirmation delay distribution from day of symptom onset for each window (forward)") +
  theme(legend.position=c(0.9,0.1))
p_sliding_delays_forward

## What discretised gamma parameters should be used for each date of confirmation?
gamma_pars_dat_forward <- do.call("rbind", gamma_pars_forward)


#################################################
## Getting data for forward and backward delays
#################################################
## Smooth means and variances to capture smooth changes in processing
smooth_p_backward_mean <- gamma_pars_dat_backward %>% 
  ggplot(aes(x=date_confirmation,y=gamma_mean_backward)) + 
  geom_line() + 
  geom_smooth(span=0.3) +
  coord_cartesian(ylim=c(0,15)) +
  ylab("Gamma mean") +
  xlab("Date of confirmation") +
  theme_bw()

smooth_p_backward_var <- gamma_pars_dat_backward %>% 
  ggplot(aes(x=date_confirmation,y=gamma_var_backward)) + 
  geom_line() + 
  geom_smooth(span=0.3) +
  ylab("Gamma variance") +
  coord_cartesian(ylim=c(0,50)) +
  xlab("Date of confirmation") +
  theme_bw()

png("figures/smoothed_delays_backward.png",width=8,height=7,res=300,units="in")
smooth_p_backward_mean/smooth_p_backward_var
dev.off()


## Smooth means backward
smoothed_means_backward <- smooth.spline(gamma_pars_dat_backward$gamma_mean_backward ~ gamma_pars_dat_backward$date_confirmation,spar=0.5)
predict_x <- as.numeric(gamma_pars_dat_backward$date_confirmation)
predict_y <- predict(smoothed_means_backward, newdata=predict_x)
smoothed_means_backward_dat <- data.frame(date_confirmation=as.Date(predict_y$x,origin="1970-01-01"),gamma_mean_backward=predict_y$y)

## Smooth var backward
smoothed_vars_backward <- smooth.spline(gamma_pars_dat_backward$gamma_var_backward ~ gamma_pars_dat_backward$date_confirmation,spar=0.5)
predict_x <- as.numeric(gamma_pars_dat_backward$date_confirmation)
predict_y <- predict(smoothed_vars_backward, newdata=predict_x)
smoothed_vars_backward_dat <- data.frame(date_confirmation=as.Date(predict_y$x,origin="1970-01-01"),gamma_var_backward=predict_y$y)

gamma_pars_use_backward <- left_join(smoothed_vars_backward_dat, smoothed_means_backward_dat)

## Fill up backward confirmation delay distribution to first confirmation date
dates <- as.Date(tmin:(min(gamma_pars_use_backward$date_confirmation)-1),origin="1970-01-01")
gamma_mean_use_backward <- gamma_pars_use_backward %>% filter(date_confirmation == min(date_confirmation)) %>% pull(gamma_mean_backward)
gamma_var_use_backward <- gamma_pars_use_backward %>% filter(date_confirmation == min(date_confirmation)) %>% pull(gamma_var_backward)
gamma_pars_use_backward <- gamma_pars_use_backward %>% 
  bind_rows(tibble(date_confirmation=dates, gamma_mean_backward=gamma_mean_use_backward, 
                   gamma_var_backward=gamma_var_use_backward, direction="backward")) %>% 
  arrange(date_confirmation) %>% 
  as_tibble
gamma_pars_use_backward <- gamma_pars_use_backward  %>% 
  mutate(gamma_scale_backward=gamma_var_backward/gamma_mean_backward,
         gamma_shape_backward=gamma_mean_backward/gamma_scale_backward)

gamma_pars_use_backward %>% ggplot() + 
  geom_point(data=gamma_pars_dat_backward,aes(x=date_confirmation,y=gamma_mean_backward)) +
  geom_line(aes(y=gamma_mean_backward,x=date_confirmation)) +
coord_cartesian(ylim=c(0,15))


## Smooth means and variances to capture smooth changes in processing
smooth_p_forward_mean <- gamma_pars_dat_forward %>% 
  ggplot(aes(x=date_onset_symptoms,y=gamma_mean_forward)) + 
  geom_line() + 
  geom_smooth(span=0.3) +
  coord_cartesian(ylim=c(0,25)) +
  ylab("Gamma mean") +
  xlab("Date of symptom onset") +
  theme_bw()

smooth_p_forward_var <- gamma_pars_dat_forward %>% 
  ggplot(aes(x=date_onset_symptoms,y=gamma_var_forward)) + 
  geom_line() + 
  geom_smooth(span=0.3) +
  ylab("Gamma variance") +
  coord_cartesian(ylim=c(0,50)) +
  xlab("Date of symptom onset") +
  theme_bw()

png("figures/smoothed_delays_forward.png",width=8,height=7,res=300,units="in")
smooth_p_forward_mean/smooth_p_forward_var
dev.off()

## Smooth means
smoothed_means_forward <- smooth.spline(gamma_pars_dat_forward$gamma_mean_forward ~ gamma_pars_dat_forward$date_onset_symptoms,spar=0.5)
predict_x <- as.numeric(gamma_pars_dat_forward$date_onset_symptoms)
predict_y <- predict(smoothed_means_forward, newdata=predict_x)
smoothed_means_forward_dat <- data.frame(date_onset_symptoms=as.Date(predict_y$x,origin="1970-01-01"),gamma_mean_forward=predict_y$y)

## Smooth var
smoothed_vars_forward <- smooth.spline(gamma_pars_dat_forward$gamma_var_forward ~ gamma_pars_dat_forward$date_onset_symptoms,spar=0.5)
predict_x <- as.numeric(gamma_pars_dat_forward$date_onset_symptoms)
predict_y <- predict(smoothed_vars_forward, newdata=predict_x)
smoothed_vars_forward_dat <- data.frame(date_onset_symptoms=as.Date(predict_y$x,origin="1970-01-01"),gamma_var_forward=predict_y$y)

gamma_pars_use_forward <- left_join(smoothed_vars_forward_dat, smoothed_means_forward_dat)

## Fill up forward confirmation delay distribution to first confirmation date
dates <- as.Date(tmin:(min(gamma_pars_use_forward$date_onset_symptoms)-1),origin="1970-01-01")
gamma_mean_use_forward <- gamma_pars_use_forward %>% filter(date_onset_symptoms == min(date_onset_symptoms)) %>% pull(gamma_mean_forward)
gamma_var_use_forward <- gamma_pars_use_forward %>% filter(date_onset_symptoms == min(date_onset_symptoms)) %>% pull(gamma_var_forward)
gamma_pars_use_forward <- gamma_pars_use_forward %>% 
  bind_rows(tibble(date_onset_symptoms=dates, gamma_mean_forward=gamma_mean_use_forward, 
                   gamma_var_forward=gamma_var_use_forward, direction="forward")) %>% 
  arrange(date_onset_symptoms) %>% 
  as_tibble
gamma_pars_use_forward <- gamma_pars_use_forward  %>% 
  mutate(gamma_scale_forward=gamma_var_forward/gamma_mean_forward,
         gamma_shape_forward=gamma_mean_forward/gamma_scale_forward)

gamma_pars_use_forward %>% ggplot() + geom_line(aes(y=gamma_var_forward,x=date_onset_symptoms))


png("figures/changing_delay.png",width=8,height=8,res=300,units="in")
changing_delay_by_report/changing_delay_by_onset
dev.off()


png("figures/fitted_forward_delays.png",width=10,height=10,res=300,units="in")
p_sliding_delays_forward
dev.off()


png("figures/fitted_backward_delays.png",width=10,height=10,res=300,units="in")
p_sliding_delays_backward
dev.off()

write_csv(gamma_pars_use_forward, "data/confirm_delay_dists_forward.csv")
write_csv(gamma_pars_use_backward, "data/confirm_delay_dists_backward.csv")
