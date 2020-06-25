#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
NumericVector calculate_preconfirmation_prevalence_from_infections(NumericVector infections, 
                                                                   double incu_par1, double incu_par2,
                                                                   NumericVector alphas, NumericVector scales){
  int tmax = infections.size() - 1;
  double prob_onset;
  NumericVector prevalence(tmax+1);
  for(int t = 0; t <= tmax; ++t){
    prevalence[t] = 0;
    for(int i = 0; i <= t; ++i) {
      // Proportion that are pre-symptomatic from each day of infection in the past
      prevalence[t] += infections[i] * (1.0 - R::plnorm(t-i, incu_par1, incu_par2, true, false));
      
      // Plus proportion that became symptomatic but not confirmed
      for(int j = i; j <= t; ++j){
        prob_onset = R::plnorm(j-i + 1, incu_par1, incu_par2, true, false) - R::plnorm(j-i, incu_par1, incu_par2, true, false);
        prevalence[t] += infections[i] * prob_onset * (1.0 - R::pgamma(t-j, alphas[j], scales[j], true, false));
      }
    }
  }
  return(prevalence);
}

//[[Rcpp::export]]
NumericVector calculate_presytmptomatic_prevalence(NumericVector infections, double incu_par1, double incu_par2){
  int tmax = infections.size() - 1;
  NumericVector prevalence(tmax+1);
  for(int t = 0; t <= tmax; ++t){
    prevalence[t] = 0;
    for(int i = 0; i <= t; ++i) {
      // Proportion that are pre-symptomatic from each day of infection in the past
      prevalence[t] += infections[i] * (1.0 - R::plnorm(t-i, incu_par1, incu_par2, true, false));
    }
  }
  return(prevalence);
}
