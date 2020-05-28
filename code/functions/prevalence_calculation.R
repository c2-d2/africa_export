## Takes an infection incidence curve and converts it to prevalence over the same time period
## Can either use fixed, mean delays for incubation period and confirmation delay,
## or perform the proper convolution of the distributions
##  - infection_incidence: a vector giving number of new infections per unit time
##  - inc_period_mean: the mean incubation period duration
##  - confirm_delay_means: a vector given the mean confirmation delay duration given symptom onset on each day
##  - inc_period_par1: optional, parameter 1 of the log normal incubation period distribution
##  - inc_period_par2: optional, parameter 2 of the log normal incubation period distribution
##  - confirm_pars1: optional, a vector of (gamma) confirmation delay distribution shape parameters, one for each day
##  - confirm_pars2: optional, a vector of (gamma) confirmation delay distribution scale parameters, one for each day
convert_prevalence <- function(infection_incidence,
                               inc_period_mean=5,
                               confirm_delay_means=6,
                               inc_period_par1=NULL,
                               inc_period_par2=NULL,
                               confirm_pars1=NULL,
                               confirm_pars2=NULL){
  ## Data structures to store results in
  times <- seq_along(infection_incidence)
  n_times <- length(times)
  prev <- numeric(n_times)
  prev_tmp <- matrix(0, nrow=n_times, ncol=n_times)
  
  ## If doing the full convolutions
  if(!is.null(inc_period_par1)) {
    ## If only passed a single set of confirmation delay parameters, use these for all times
    if(length(confirm_pars1) < length(infection_incidence)){
      confirm_pars1 <- rep(confirm_pars1[1], length(infection_incidence))
      confirm_pars2 <- rep(confirm_pars2[1], length(infection_incidence))
    }
    ## Calculate daily prevalence
    prevalence <- calculate_preconfirmation_prevalence_from_infections(infection_incidence, incu_par1, incu_par2,
                                                                       confirm_pars1, confirm_pars2)
  } else {
    ## If only passed a single set of confirmation delay means, use for all times
    if(length(confirm_delay_means) < length(infection_incidence)){
      confirm_delay_means <- rep(confirm_delay_means[1], length(infection_incidence))
    }
    ## For each day of infection incidence, enumerate out for all days these infections contribute to prevalence for
    for(i in 1:n_times) {
      prob_prevalent <- c(rep(1, ceiling(inc_period_mean) + ceiling(confirm_delay_means[i])), rep(0, n_times - ceiling(inc_period_mean) - ceiling(confirm_delay_means[i])))
      prev_tmp[i,i:n_times] <- infection_incidence[i] * prob_prevalent[1:(length(prob_prevalent)-i+1)]
    }
    prevalence <- colSums(prev_tmp)
  }
  return(prevalence)
}
