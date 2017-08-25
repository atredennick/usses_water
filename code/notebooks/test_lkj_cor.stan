data {
  int<lower=0> N; 
  int<lower=0> K;
  row_vector[K] X[N]; 
  real Y[N]; 
  int<lower=0> J;
  int<lower=0, upper=J> uu[N];
}
parameters {
	cholesky_factor_corr[K] L;
	vector<lower=0>[K] tau;
	real<lower=0> sig_e;
	vector[K] beta; 
	vector[K] Z[J];
}
transformed parameters {
  vector[N] mu; 
  vector[K] U[J];
  matrix[K,K] SIGMA; 
  
  SIGMA = diag_pre_multiply(tau, L); # generate cholesky of Covariance matrix
  
  for(j in 1:J)
    U[j] = SIGMA*Z[j];               # multiply by uncorrelated deviates to get correlated random effects 
    
  for(i in 1:N)  
    mu[i] = X[i]*(beta + U[uu[i]]); 
}
model {
  ####  PRIORS
  L ~ lkj_corr_cholesky(2.0);
  
  tau ~ cauchy(0,2.5);
  sig_e ~ cauchy(0,2.5);
  beta ~ normal(0,1);
  
  for(j in 1:J)
    Z[j] ~ normal(0,1);
  
  ####  LIKELIHOOD
  Y ~ normal(mu, sig_e); # observations vary normally around expected values
}
