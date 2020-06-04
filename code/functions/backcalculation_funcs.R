discretized_lnorm <- function(x,incu_par1, incu_par2, tmax){
  if(!is.null(tmax)){
    denominator <- plnorm(tmax, incu_par1, incu_par2, TRUE)
  } else {
    denominator <- 1
  }
  prob <- (plnorm(x+1, incu_par1, incu_par2) - plnorm(x, incu_par1, incu_par2))/denominator
  prob
}

discretized_lnorm_probs <- function(incu_par1, incu_par2, tmax){
  x <- seq(0,tmax-1,by=1)
  denominator <- plnorm(tmax, incu_par1, incu_par2, TRUE)
  probs <- (plnorm(x+1, incu_par1, incu_par2) - plnorm(x, incu_par1, incu_par2))/denominator
  probs
}

gamma_discrete_own_normalized <- function(x, gamma_shape, gamma_scale,tmax=NULL){
  if(!is.null(tmax)){
    denominator <- pgamma(tmax, shape=gamma_shape,scale=gamma_scale,lower.tail=TRUE,log=FALSE)
  } else {
    denominator <-1
  }
  lik <- (pgamma(x+1, shape=gamma_shape,scale=gamma_scale,lower.tail = TRUE,log=FALSE) - pgamma(x, shape=gamma_shape,scale=gamma_scale,lower.tail = TRUE,log=FALSE))/denominator
}

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

fit_gamma_discrete_own_normalized <- function(pars,dat, tmax=NULL){
  mean <- pars[1]
  var <- pars[2]
  scale <- var/mean
  shape <- mean/scale
  if(!is.null(tmax)){
    denominator <- pgamma(tmax, shape=shape,scale=scale,lower.tail=TRUE,log=FALSE)
  } else {
    denominator <-1
  }
  lik <- log((pgamma(dat+1, shape=shape,scale=scale,lower.tail = TRUE,log=FALSE) - pgamma(dat, shape=shape,scale=scale,lower.tail = TRUE,log=FALSE))/denominator)
  -sum(lik)
}

generate_gamma_distributions_backward <- function(threshold, 
                                                  dat,
                                                  max_delay## Need report_date and n
                                                  ){
  ## Store generated counts and geometric probabilities for each day
  all_dat_backward <- NULL
  
  ## Get number of new onsets on each day
  number_onsets <- dat %>% filter(confirmation_delay > 0) %>% group_by(onset_date) %>% tally()
  
  ## For each day, go back in time until the number of new confirmations in that period
  ## is greater than threshold
  first_date <- number_confirmed$report_date[which(cumsum(number_confirmed$n) > threshold)[1]]
  use_dates <- times[times > first_date]
  probs <- numeric(length(use_dates))
  gamma_pars_backward <- NULL
  tmin <- min(dat$report_date)
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
    fit1 <- optim(par=c(mean_start, var_start), fn=fit_gamma_discrete_own_normalized,dat=x,tmax=use_max_delay)
    gamma_pars_backward[[i]] <- data.frame("gamma_mean_backward"=fit1$par[1],"gamma_var_backward"=fit1$par[2],
                                           "max_delay"=use_max_delay,
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
  gamma_pars_use_backward <- gamma_pars_use_backward %>% group_by(date_confirmation) %>% mutate(max_delay_dat = min(as.numeric(date_confirmation - min(dat$onset_date)), max_delay))
  gamma_pars_use_backward
  
}
