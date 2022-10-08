data {
  int<lower=1> N;
  vector[N] x1; //the latest change in price
  vector[N] x2; //the recent distribution of price-changes
  vector[N] x3; //trend
  int<lower=0,upper=1> z[N]; //output
  //test
  int<lower=1,upper=1> N_test; //predict one price
  int<lower=-1,upper=1> x1_test; //the latest change in price
  real<lower=0,upper=1> x2_test; //the recent distribution of price-changes
  int<lower=1> x3_test; //trend
}
parameters {
  real alpha;
  real beta1;
  real beta2;
  real beta3;
}
model {
  z ~ bernoulli_logit(alpha + beta1*x1 + beta2*x2 + beta3*x3);
  beta1 ~ normal(0,10); 
  beta2 ~ normal(0,10);
  beta3 ~ normal(0,10);
}
generated quantities {
  int<lower=0,upper=1> z_test;
  z_test = bernoulli_rng(inv_logit(alpha + 
  beta1*x1_test + 
  beta2*x2_test + 
  beta3*x3_test));
}
