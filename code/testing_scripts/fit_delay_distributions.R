library(tidyverse)
library(extraDistr)

setwd("~/Documents/GitHub/africa_export/")

#' Function for optim to fit a gamma distribution
#' 
#' @param pars vector, 1: gamma mean; 2: gamma variance
#' @param dat is a vector of event observation times
#' @return negative sum log likelihood from dgamma_mean
fit_gamma <- function(pars, dat){
  mean <- pars[1]
  var <- pars[2]
  -sum(dgamma_mean(dat, mean, var, TRUE))
}
fit_gamma_discrete <- function(pars,dat){
  mean <- pars[1]
  var <- pars[2]
  scale <- var/mean
  shape <- mean/scale
  -sum(ddgamma(dat, shape=shape, scale=scale, log=TRUE))
}

fit_gamma_discrete_own <- function(pars,dat){
  mean <- pars[1]
  var <- pars[2]
  scale <- var/mean
  shape <- mean/scale
  lik <- log(pgamma(x+1, shape=shape,scale=scale,lower.tail = TRUE,log=FALSE) - pgamma(x, shape=shape,scale=scale,lower.tail = TRUE,log=FALSE))
  -sum(lik)
}

tmin <- as.Date("2020-01-01")
tmax <- as.Date("2020-03-03")
times <- seq(tmin, tmax, by="1 day")

## Have a look at Zhang et al. data
dat <- read_csv("~/Documents/GitHub/covback/data/real/zhang_appendix_lancet.csv")
plot_dat <- dat %>% 
  group_by(Location) %>%
  mutate(report_date = as.Date(report_date, origin="2019/01/11"),
         onset_date= as.Date(onset_date, oprigin="2019/01/11"),
         report_delay = report_date - onset_date,
         stage=onset_date < as.Date("2020/01/27")) %>% 
  select(ID, onset_date, report_date,report_delay, stage) %>% drop_na()

changing_delay_by_report <-  ggplot(plot_dat,aes(x=report_date,y=report_delay)) + 
  geom_jitter(size=0.25,width = 0.1,height=0.1) +
  geom_smooth() +
  coord_cartesian(ylim=c(0,30)) +
  scale_x_date(breaks="7 days",limits=range(times)) +
  geom_vline(xintercept=as.Date("2020-01-23",origin="2019-11-01"),col="green",size=0.5,linetype="dashed") +
  xlab("Date of report onset") +
  ylab("Delay from symptom onset to report") +
  ggtitle("Delay going backward. For a case reported on day t, how long ago did they report symptom onset?") +
  theme_bw()+
  theme(title=element_text(size=8))

changing_delay_by_onset <- 
  ggplot(plot_dat,aes(x=onset_date,y=report_delay)) + 
  geom_jitter(size=0.25,width = 0.1,height=0.1) +
  geom_smooth() +
  coord_cartesian(ylim=c(0,30)) +
  scale_x_date(breaks="7 days",limits=range(times)) +
  geom_vline(xintercept=as.Date("2020-01-23",origin="2019-11-01"),col="green",size=0.5,linetype="dashed") +
  xlab("Date of symptom onset") +
  ylab("Delay from symptom onset to report") +
  ggtitle("Delay going forward. For a symptom onset on day t, how long did it take to get confirmed?") +
  theme_bw() +
  theme(title=element_text(size=8))

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

model_probs_gamma_backward <- matrix(nrow=length(use_dates),ncol=101)

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
  fit1 <- optim(par=c(5, 25), fn=fit_gamma_discrete,dat=x)
  #fit1 <- optim(par=c(5, 25), fn=fit_gamma_discrete_own,dat=x)
  gamma_pars_backward[[i]] <- data.frame("gamma_mean_backward"=fit1$par[1],"gamma_var_backward"=fit1$par[2],
                                         "date_confirmation"=use_dates[i], "n_used"=counted,
                                         "direction"="backward")
  
  all_dat_backward <- bind_rows(tmp, all_dat_backward)
  
  scale <- fit1$par[2]/fit1$par[1]
  shape <- fit1$par[1]/scale
  
  model_probs_gamma_backward[i,] <- ddgamma(0:100, scale=scale, shape=shape,log=FALSE)
}
all_dat_backward <- all_dat_backward %>% complete(confirmation_delay, start, fill=list(n=0))

model_probs_gamma_backward <- reshape2::melt(model_probs_gamma_backward)
colnames(model_probs_gamma_backward) <- c("label", "confirmation_delay","prob")
model_probs_gamma_backward$confirmation_delay <- model_probs_gamma_backward$confirmation_delay
model_probs_gamma_backward$label <- use_dates[model_probs_gamma_backward$label]
model_probs_gamma_backward$label <- paste0("before ", model_probs_gamma_backward$label)
model_probs_gamma_backward$fit <- "Gamma"
all_dat_backward$label <- paste0("before ", all_dat_backward$start)
all_model_probs_backward <- model_probs_gamma_backward

## For each day, go back in time day-by-day until at least `threshold` new confirmed cases have happened.
## Use the case confirmations in this window to generate a confirmation delay distribution
## for that window.
all_dat_backward <- all_dat_backward %>% group_by(label) %>% mutate(rel_n = n/sum(n))
p_sliding_delays_backward <- ggplot(all_dat_backward) + 
  geom_bar(aes(x=confirmation_delay,y=rel_n),stat="identity") + 
  geom_line(data=all_model_probs_backward,aes(x=confirmation_delay,y=prob),col="red",size=1) +
  geom_vline(xintercept=1,linetype="dashed") +
  facet_wrap(~label) +
  coord_cartesian(xlim=c(0,40)) +
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

## For each day, go back in time until the number of new symptom onsets in that period
## is greater than threshold
first_date <- number_onsets$onset_date[which(cumsum(number_onsets$n) > threshold)[1]]
use_dates <- times[times > first_date]
probs <- numeric(length(use_dates))
gamma_pars_forward <- NULL

model_probs_gamma_forward <- matrix(nrow=length(use_dates),ncol=101)

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
  mean_start <- mean(x_fit)
  var_start <- var(x_fit)
  fit1 <- optim(par=c(5, 25), fn=fit_gamma_discrete,dat=x)
  gamma_pars_forward[[i]] <- data.frame("gamma_mean_forward"=fit1$par[1],"gamma_var_forward"=fit1$par[2],
                                        "date_onset_symptoms"=use_dates[i], "n_used"=counted,
                                        "direction"="forward")
  
  all_dat_forward <- bind_rows(tmp, all_dat_forward)
  
  scale <- fit1$par[2]/fit1$par[1]
  shape <- fit1$par[1]/scale
  
  model_probs_gamma_forward[i,] <- ddgamma(0:100, scale=scale, shape=shape,log=FALSE)
}
all_dat_forward <- all_dat_forward %>% complete(confirmation_delay, start, fill=list(n=0))
model_probs_gamma_forward <- reshape2::melt(model_probs_gamma_forward)
colnames(model_probs_gamma_forward) <- c("label", "confirmation_delay","prob")
model_probs_gamma_forward$confirmation_delay <- model_probs_gamma_forward$confirmation_delay
model_probs_gamma_forward$label <- use_dates[model_probs_gamma_forward$label]
model_probs_gamma_forward$label <- paste0("before ", model_probs_gamma_forward$label)
model_probs_gamma_forward$fit <- "Gamma"
all_dat_forward$label <- paste0("before ", all_dat_forward$start)

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
  facet_wrap(~label) +
  theme_bw() +
  xlab("Delay from symptom onset to confirmation (days)") +
  ylab("Count") +
  ggtitle("Confirmation delay distribution from day of symptom onset for each window (forward)") +
  theme(legend.position=c(0.9,0.1))


## What discretised gamma parameters should be used for each date of confirmation?
gamma_pars_dat_forward <- do.call("rbind", gamma_pars_forward)


## Fill up backward confirmation delay distribution to first confirmation date
dates <- as.Date(tmin:(min(gamma_pars_dat_backward$date_confirmation)-1),origin="1970-01-01")
gamma_mean_use <- gamma_pars_dat_backward %>% filter(date_confirmation == min(date_confirmation)) %>% pull(gamma_mean_backward)
gamma_var_use <- gamma_pars_dat_backward %>% filter(date_confirmation == min(date_confirmation)) %>% pull(gamma_var_backward)
gamma_pars_dat_backward <- gamma_pars_dat_backward %>% 
  bind_rows(tibble(date_confirmation=dates, gamma_mean_backward=gamma_mean_use, 
            gamma_var_backward=gamma_var_use, n_used=threshold, direction="backward")) %>% 
  arrange(date_confirmation) %>% 
  as_tibble
gamma_pars_dat_backward <- gamma_pars_dat_backward  %>% 
  mutate(gamma_scale_backward=gamma_var_backward/gamma_mean_backward,
  gamma_shape_backward=gamma_mean_backward/gamma_scale_backward)



## Fill up forward confirmation delay distribution to first confirmation date
dates <- as.Date(tmin:(min(gamma_pars_dat_forward$date_onset_symptoms)-1),origin="1970-01-01")
gamma_mean_use <- gamma_pars_dat_forward %>% filter(date_onset_symptoms == min(date_onset_symptoms)) %>% pull(gamma_mean_forward)
gamma_var_use <- gamma_pars_dat_forward %>% filter(date_onset_symptoms == min(date_onset_symptoms)) %>% pull(gamma_var_forward)
gamma_pars_dat_forward <- gamma_pars_dat_forward %>% 
  bind_rows(tibble(date_onset_symptoms=dates, gamma_mean_forward=gamma_mean_use, 
                   gamma_var_forward=gamma_var_use, n_used=threshold, direction="backward")) %>% 
  arrange(date_onset_symptoms) %>% 
  as_tibble
gamma_pars_dat_forward <- gamma_pars_dat_forward  %>% 
  mutate(gamma_scale_forward=gamma_var_forward/gamma_mean_forward,
         gamma_shape_forward=gamma_mean_forward/gamma_scale_forward)



png("figures/changing_delay.png",width=8,height=8,res=300,units="in")
changing_delay_by_report/changing_delay_by_onset
dev.off()


png("figures/fitted_forward_delays.png",width=10,height=10,res=300,units="in")
p_sliding_delays_forward
dev.off()


png("figures/fitted_backward_delays.png",width=10,height=10,res=300,units="in")
p_sliding_delays_backward
dev.off()

write_csv(gamma_pars_dat_forward, "data/confirm_delay_dists_forward.csv")
write_csv(gamma_pars_dat_backward, "data/confirm_delay_dists_backward.csv")
