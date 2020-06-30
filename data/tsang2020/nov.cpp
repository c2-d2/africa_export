 #include <RcppArmadilloExtensions/sample.h>
#include <Rcpp.h>
#include <RcppParallel.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppParallel)]]
using namespace Rcpp ;
using namespace RcppParallel;

//########################################################################################################################################
// function to generate normal random variable
// [[Rcpp::export]]
double rnorm(double a, double b) { // a: mean, b: s.d.
	double c=a+b*sum(rnorm(1));
    return c;
}

//########################################################################################################################################
// function to compute the nb distribution
// [[Rcpp::export]]
double dnb(int n, int m, double c) { 
  double d=R::dpois(n,(m+1)/c,1);
  return d;
}

//########################################################################################################################################
// function to for inverse of logit
// [[Rcpp::export]]
double logit(double a) { 
    return log(a/(1-a));
}

//########################################################################################################################################
// function to for inverse of logit
// [[Rcpp::export]]
double invlogit(double a) { 
    return exp(a)/(1+exp(a));
}

//########################################################################################################################################
// function to generate binomial random number
// [[Rcpp::export]]
int gen_binom(double p){
  double cut=(double)rand()/(RAND_MAX);
  int out=0;
  if (cut<p){
    out=1;
  }
return out;
}

//########################################################################################################################################
// function to general multiviariable normal given sigma
// [[Rcpp::export]]
NumericVector rmnorm(arma::mat sigma) {
  int ncols=sigma.n_cols;
  arma::rowvec c=arma::randn(1,ncols);
  arma::rowvec a=c*arma::chol(sigma);   
  NumericVector b=NumericVector(a.begin(),a.end());   
  return b;
}

//########################################################################################################################################
//function to compute the prior likelihood 
// [[Rcpp::export]]
double prior_loglik(NumericVector para){
// check if the para are within their possible range
double out=1;
  int b1;
  for (b1=7;b1>=0;--b1){
    out*=sum(dunif(NumericVector::create(para(b1)),0.0000000000000000000000001,10000.0));
  }
  for (b1=19;b1>=8;--b1){
    out*=sum(dunif(NumericVector::create(para(b1)),-9.99,9.99));
  }
  // if the prior is outside the parameter space
  if (out==0){
    out=-9999999;
  }
  else{
    out=log(out);	
  }
  return out;
}




//########################################################################################################################################
//function to do simulation
// [[Rcpp::export]]
List sim_data(NumericMatrix data,NumericVector para){ 
  int b1;
  int b2;
  int b3;
  
  NumericMatrix data1(clone(data));   
  
  NumericVector onset2hosp(30);
  NumericVector onset2death(30);
  for (b1=29;b1>=0;--b1){
    onset2hosp(b1)=(R::pgamma(b1+1,para[1],para[2],1,0)-R::pgamma(b1,para[1],para[2],1,0))/(R::pgamma(30.0,para[1],para[2],1,0));
    onset2death(b1)=(R::pgamma(b1+1,para[3],para[4],1,0)-R::pgamma(b1,para[3],para[4],1,0))/(R::pgamma(30.0,para[3],para[4],1,0));	
  }
  
  for (b1=data1.nrow()-1;b1>=0;--b1){
    for (b2=2;b2>=0;--b2){
      data1(b1,1+4*b2)=0;
      data1(b1,2+4*b2)=0;
    }
  }
  
  for (b3=2;b3>=0;--b3){
    for (b1=data1.nrow()-1;b1>=40;--b1){
      for (b2=29;b2>=0;--b2){
        data1(b1-b2,1+4*b3)+=data1(b1,0+4*b3)*onset2hosp(b2)/(para[5]+(1-para[5])*para[6+b3]);
      }
    }
  }
  
  for (b3=2;b3>=0;--b3){
    for (b1=data1.nrow()-1;b1>=0;--b1){
      for (b2=29;b2>=0;--b2){
        if (b1+b2<data1.nrow()){
          data1(b1+b2,2+4*b3)+=data1(b1,1+4*b3)*onset2death(b2)*para[0]*para[5];	
        }
      }	
    }
  }
  
  for (b3=2;b3>=0;--b3){
    for (b1=data1.nrow()-1;b1>=0;--b1){
      data1(b1,3+4*b3)=R::rpois(data1(b1,2+4*b3));	
    }
  }
  
  return List::create(_[""]=data1,
  	_[""]=onset2death,
  	_[""]=onset2hosp);
} 



//########################################################################################################################################
//function to compute likelihood for infection and symptom
// [[Rcpp::export]]
NumericMatrix loglik(NumericMatrix data1,NumericVector para){
  
  int b1;
  int b2;
  int b3;
  
  // for record likelihood
  NumericMatrix out(data1.nrow(),4);
  
  // first compute the interval
  NumericVector inf2onset(200);
  NumericVector onset2report(200);
  NumericVector Finf2onset(200);
  NumericVector Fonset2report(200);
  for (b1=199;b1>=0;--b1){
    onset2report(b1)=(R::pgamma(b1+1,para[2],para[3],1,0)-R::pgamma(b1,para[2],para[3],1,0))/(R::pgamma(200.0,para[2],para[3],1,0));
    inf2onset(b1)=(R::plnorm(b1+1,para[0],para[1],1,0)-R::plnorm(b1,para[0],para[1],1,0))/(R::plnorm(200.0,para[0],para[1],1,0));	
    Fonset2report(b1)=(R::pgamma(b1+1,para[2],para[3],1,0))/(R::pgamma(200.0,para[2],para[3],1,0));
    Finf2onset(b1)=(R::plnorm(b1+1,para[0],para[1],1,0))/(R::plnorm(200.0,para[0],para[1],1,0));	
  }

  // 1/15 1/18 1/22 1/27 2/4 2/18
  // 45   48   52   57   65  79
  
  // generate changepoint matrix
  NumericMatrix changepoint(data1.nrow(),4);
  for (b1=data1.nrow()-1;b1>=0;--b1){
    changepoint(b1,0)=1;
    if (b1<=46){
      changepoint(b1,0)=1-Fonset2report(47-b1);	
    }
    if (b1<=31){
      changepoint(b1,0)=0;
    }
    
    changepoint(b1,1)=1;
    if (b1<=56){
      changepoint(b1,1)=1-Fonset2report(57-b1);	
    }	
    if (b1<=41){
      changepoint(b1,1)=0;	
    }
    
    changepoint(b1,2)=1;
    if (b1<=64){
      changepoint(b1,2)=1-Fonset2report(65-b1);	
    }	
    if (b1<=49){
      changepoint(b1,2)=0;	
    }
    
    changepoint(b1,3)=1;
    if (b1>=52){
      changepoint(b1,3)=Finf2onset(b1-52);	
    }
  
  }
  
  // generate onset
  NumericMatrix data1onset(data1.nrow(),3);   
  NumericMatrix data1pred(data1.nrow(),3); 
  
  for (b1=data1.nrow()-1;b1>=0;--b1){
    for (b2=2;b2>=0;--b2){
      data1onset(b1,b2)=exp(
        para[11+b2]+
        para[14+b2]*(b1+1)+
        para[17+b2]*(b1>=52)*(b1-52)*changepoint(b1,3) +
        para[8]*changepoint(b1,0)+
        para[9]*changepoint(b1,1)+
        para[10]*changepoint(b1,2)
      );
    }
  }
  
  // based on onset2report, generate reporting
  NumericMatrix data1report(data1.nrow(),3); 
    for (b1=data1.nrow()-1;b1>=0;--b1){
      for (b2=2;b2>=0;--b2){
        for (b3=69;b3>=0;--b3){
          if (b1-b3>=0){
            data1report(b1,b2)+=data1(b1-b3,b2)*onset2report[b3];
          }
      }
    }
  }
  
  NumericMatrix probmat(data1.nrow(),4);
  for (b1=data1.nrow()-1;b1>=0;--b1){
    for (b3=2;b3>=0;--b3){	
      probmat(b1,b3)=data1onset(b1,b3); ///sum(data1onsetobs(_,b3));
    }
    probmat(b1,3)=sum(data1report(b1,_)); ///sum(data1report);
  }
  
  for (b1=data1.nrow()-1;b1>=1;--b1){
    for (b3=3;b3>=0;--b3){	
      if (data1(b1,b3)!=0){
        out(b1,b3)=R::dnorm(log(data1(b1,b3)),log(probmat(b1,b3)),para[4+(b3==3)],1); 
        //out(b1,b3)=data1(b1,b3)*log(probmat(b1,b3));  
        //out(b1,b3)=R::dpois(data1(b1,b3),data1onsetobs(b1,b3),1);
      }
    }
  //out(b1,3)=R::dpois(data1(b1,3),sum(data1report(b1,_)),1);
  }
  
  
  return out;
  //return List::create(_[""]=out,
  //	_[""]=data1onset,
  //	_[""]=data1report,
  //  _[""]=data1pred,	
  //	_[""]=inf2onset,
  //	_[""]=onset2report,
  //	_[""]=Finf2onset,
  //	_[""]=Fonset2report,
  //	_[""]=changepoint);
}





//########################################################################################################################################
//function to compute likelihood for infection and symptom
// [[Rcpp::export]]
List pred(NumericMatrix data1,NumericVector para){
  
  int b1;
  int b2;
  int b3;
  
  // for record likelihood
  NumericMatrix out(data1.nrow(),4);
  
  // first compute the interval
  NumericVector inf2onset(200);
  NumericVector onset2report(200);
  NumericVector Finf2onset(200);
  NumericVector Fonset2report(200);
  for (b1=199;b1>=0;--b1){
    onset2report(b1)=(R::pgamma(b1+1,para[2],para[3],1,0)-R::pgamma(b1,para[2],para[3],1,0))/(R::pgamma(200.0,para[2],para[3],1,0));
    inf2onset(b1)=(R::plnorm(b1+1,para[0],para[1],1,0)-R::plnorm(b1,para[0],para[1],1,0))/(R::plnorm(200.0,para[0],para[1],1,0));	
    Fonset2report(b1)=(R::pgamma(b1+1,para[2],para[3],1,0))/(R::pgamma(200.0,para[2],para[3],1,0));
    Finf2onset(b1)=(R::plnorm(b1+1,para[0],para[1],1,0))/(R::plnorm(200.0,para[0],para[1],1,0));	
  }
  
  //    1/15 1/18 1/22 1/27 2/4 2/18
  //    45   48   52   57   65  79
  //JH: 37   47        57   65
  // generate changepoint matrix
  NumericMatrix changepoint(data1.nrow(),4);
  for (b1=data1.nrow()-1;b1>=0;--b1){
    changepoint(b1,0)=1;
    // Probability not reported by change definition 1
    if (b1<=46){
      changepoint(b1,0)=1-Fonset2report(47-b1);	
    }
    if (b1<=36){
      changepoint(b1,0)=0;
    }
    
    changepoint(b1,1)=1;
    if (b1<=56){
      changepoint(b1,1)=1-Fonset2report(57-b1);	
    }	
    if (b1<=46){
      changepoint(b1,1)=0;	
    }
    
    changepoint(b1,2)=1;
    if (b1<=64){
      changepoint(b1,2)=1-Fonset2report(65-b1);	
    }	
    if (b1<=54){
      changepoint(b1,2)=0;	
    }
    
    // Contribution of infections this many days after lockdown to 
    // onsets on this day
    changepoint(b1,3)=1;
    if (b1>=52){
      changepoint(b1,3)=Finf2onset(b1-52);	
    }
  
  }
  
  // generate onset
  NumericMatrix data1onset(data1.nrow(),3);   
  NumericMatrix data1pred(data1.nrow(),15); 
  // For each time point
  for (b1=data1.nrow()-1;b1>=0;--b1){
    // For Wuhan, non-Wuhan Hubei and non-Hubei
    for (b2=2;b2>=0;--b2){
      // Generate predicted onsets for each of the 3 locations
      data1onset(b1,b2)=exp(
        para[11+b2]+ // Intercept
        para[14+b2]*(b1+1)+ // Growth rate as per version 1, r * time
        para[17+b2]*(b1>=52)*(b1-52)*changepoint(b1,3)+ // Plus reduction in growth due to lockdown x days post lockdown
        para[8]*changepoint(b1,0)+ // Plus flat increase due to case definition 2
        para[9]*changepoint(b1,1)+ // Plus flat increase due to case definition 4
        para[10]*changepoint(b1,2) // Plus flat increase due to case definition 5
      );
      
      
      // For predictions, we have 5 columns per location
      // First, let exponential growth continue without changing case definition
      data1pred(b1,5*b2)=exp(
        para[11+b2]+
        para[14+b2]*(b1+1)+
        para[17+b2]*(b1>=52)*(b1-52)*changepoint(b1,3)
      );
      //data1pred(b1,5*b2+1)=exp(para[11+b2]+para[14+b2]*(b1+1)+para[17+b2]*(b1>=52)*(b1-52)*changepoint(b1,3)+para[8]*changepoint(b1,0));
      //data1pred(b1,5*b2+2)=exp(para[11+b2]+para[14+b2]*(b1+1)+para[17+b2]*(b1>=52)*(b1-52)*changepoint(b1,3)+para[8]*changepoint(b1,0)+para[9]*changepoint(b1,1));
      //data1pred(b1,5*b2+3)=exp(para[11+b2]+para[14+b2]*(b1+1)+para[17+b2]*(b1>=52)*(b1-52)*changepoint(b1,3)+para[8]*changepoint(b1,0)+para[9]*changepoint(b1,1)+para[10]*changepoint(b1,2));
      
      // Identical to above
      data1pred(b1,5*b2+1)=exp(
        para[11+b2]+
        para[14+b2]*(b1+1)+
        para[17+b2]*(b1>=52)*(b1-52)*changepoint(b1,3)
      );
      
      // Under case definition 2
      data1pred(b1,5*b2+2)=exp(
        para[11+b2]+
        para[14+b2]*(b1+1)+
        para[17+b2]*(b1>=52)*(b1-52)*changepoint(b1,3)+para[8]
      );
      
      // Under case definition 4
      data1pred(b1,5*b2+3)=exp(
        para[11+b2]+
          para[14+b2]*(b1+1)+
          para[17+b2]*(b1>=52)*(b1-52)*changepoint(b1,3)+
          para[8]+
          para[9]
      );
      
      // Under case definition 5
      data1pred(b1,5*b2+4)=exp(
        para[11+b2]+
          para[14+b2]*(b1+1)+
          para[17+b2]*(b1>=52)*(b1-52)*changepoint(b1,3)+
          para[8]+
          para[9]+
          para[10]
      );
    
    }
  }
  
  // based on onset2report, generate reporting
  NumericMatrix data1report(data1.nrow(),3); 
  for (b1=data1.nrow()-1;b1>=0;--b1){
    for (b2=2;b2>=0;--b2){
      // Go over days in the past and find probability that an onset b3 days in the past is reported today
      for (b3=69;b3>=0;--b3){
        if (b1-b3>=0){
          data1report(b1,b2)+=data1(b1-b3,b2)*onset2report[b3];
        }
      }
    }
  }
  
  NumericMatrix probmat(data1.nrow(),4);
  for (b1=data1.nrow()-1;b1>=0;--b1){
    for (b3=2;b3>=0;--b3){	
      probmat(b1,b3)=data1onset(b1,b3); ///sum(data1onsetobs(_,b3));
    }
    probmat(b1,3)=sum(data1report(b1,_)); ///sum(data1report);
  }
  
  // Obs likelihood, assume normally distributed log(cases)
  for (b1=data1.nrow()-1;b1>=1;--b1){
    for (b3=3;b3>=0;--b3){	
      if (data1(b1,b3)!=0){
        out(b1,b3)=R::dnorm(log(data1(b1,b3)),log(probmat(b1,b3)),para[4+(b3==3)],1); 
        //out(b1,b3)=data1(b1,b3)*log(probmat(b1,b3));  
        //out(b1,b3)=R::dpois(data1(b1,b3),data1onsetobs(b1,b3),1);
      }
    }
  //out(b1,3)=R::dpois(data1(b1,3),sum(data1report(b1,_)),1);
  }
  
  
  //return out;
  return List::create(_[""]=out,
  	_[""]=data1onset,
  	_[""]=data1report,
      _[""]=data1pred,	
  	_[""]=inf2onset,
  	_[""]=onset2report,
  	_[""]=Finf2onset,
  	_[""]=Fonset2report,
  	_[""]=changepoint);
}


//##############################################################################################################################################
//##############################################################################################################################################
// function for mcmc
// [[Rcpp::export]]
List mcmc(NumericMatrix data1, 
          NumericVector int_para, // initial parameter
          int mcmc_n,             // length of mcmc stain
          NumericVector move,     // which one should move in the model
          NumericVector sigma){            
  
  
  // create the vector for use
  int b0;
  int b1;
  int b2;
  int moveindex;
  
  // need to set number of parameter here
  NumericMatrix p_para(mcmc_n,int_para.length());
  NumericMatrix p_para_r(mcmc_n,sum(move));
  p_para(0,_)=int_para;
  moveindex=sum(move)-1;
  for (b1=int_para.length()-1;b1>=0;--b1){
    if (move(b1)){
      p_para_r(0,moveindex)=p_para(0,b1);
      --moveindex;
    }	
  }
  
  // matrix to record LL
  NumericMatrix LL1(mcmc_n,2);
  
  
  //####################################################################################################################################
  // compute likelihood
  
  NumericMatrix loglik1pro;
  NumericMatrix loglik1=loglik(data1,p_para(0,_));
  
  LL1(0,0)=sum(loglik1);
  
  NumericVector temploglik(1);
  NumericVector newloglik(1);
  temploglik(0)=LL1(0,0)+prior_loglik(p_para(0,_));
  
  double loglikeratio;
  double accept_pro;
  NumericVector pro_para(int_para.length());
  
  //####################################################################################################################################
  // main mcmc step
  
  // here to record and tune the accept rate for parameter
  NumericVector acceptrate(int_para.length());
  NumericMatrix record1(mcmc_n,2);
  
  //####################################################################################################################################
  for (b0=1;b0<mcmc_n;++b0){
    
    // after 500 step, then set the sigma to be the empirical sigma
    if ((b0>500)&(b0%100==0)){
      for (b1=int_para.length()-1;b1>=0;--b1){
        if (move(b1)){	
          NumericVector temp1(b0-1);
          for (b2=b0-2;b2>=0;--b2){
            temp1(b2)=p_para(b2,b1);	
          }
          sigma(b1)=sd(temp1);	
          // tuning
          if (acceptrate(b1)<=0.1){
            sigma(b1)*=0.5;
          }	
          if ((acceptrate(b1)<=0.15)&(acceptrate(b1)>0.1)){
            sigma(b1)*=0.8;
          }
          if ((acceptrate(b1)<=0.2)&(acceptrate(b1)>0.15)){
            sigma(b1)*=0.95;
          }
          if ((acceptrate(b1)<=0.4)&(acceptrate(b1)>0.3)){
            sigma(b1)*=1.05;
          }
          if ((acceptrate(b1)<=0.9)&(acceptrate(b1)>0.4)){
            sigma(b1)*=1.2;
          }
          if (acceptrate(b1)>0.9){
            sigma(b1)*=2;
          }
        }
      }
    }
  
  
    // metorpolis-hasing update on parameter
    
    for (b1=0;b1<int_para.length();++b1){
      if (move(b1)){
        pro_para=p_para(b0-1,_);
        for (b2=b1-1;b2>=0;--b2){
          pro_para(b2)=p_para(b0,b2);	
        } 
        pro_para(b1)+=rnorm(0.0,sigma(b1));
        newloglik(0)=prior_loglik(pro_para);
        if (newloglik(0)> -9999999){
          loglik1pro=loglik(data1,pro_para);
          newloglik(0)+=sum(loglik1pro);
          loglikeratio=newloglik(0)-temploglik(0);
          accept_pro=pow(exp(1),loglikeratio);
        } else {
          accept_pro=0;	
        }
        if(gen_binom(accept_pro)){
          p_para(b0,b1)=pro_para(b1);
          loglik1=clone(loglik1pro);
          temploglik(0)=newloglik(0);
          acceptrate(b1)*=(b0-1);
          acceptrate(b1)+=1;
          acceptrate(b1)/=b0;
        } else{
          p_para(b0,b1)=p_para(b0-1,b1);
          acceptrate(b1)*=(b0-1);
          acceptrate(b1)/=b0;
        }
      } else {
        p_para(b0,b1)=p_para(b0-1,b1);
      }
    }
    
    LL1(b0,0)=temploglik(0)-prior_loglik(p_para(b0,_));
    
    
    if (b0%1000==1){
      Rcout << "b0: " << b0 << std::endl;
    }
  }
  return List::create(_[""]=p_para,
  _[""]=LL1);//,//,_[""]=data21_pro_tracking,//_[""]=pro_value_tracking,//_[""]=data21_lik_tracking,//_[""]=data21_pro_lik_tracking,//_[""]=lik_ratio_tracking,//_[""]=acc_rej_tracking,//_[""]=stepsize_tracking);
}


