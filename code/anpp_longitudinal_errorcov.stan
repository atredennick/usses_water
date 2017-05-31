data {
  int<lower=0> Npreds;
  int<lower=0> Nplots;
  int<lower=0> Ntimes;
  vector[Ntimes] y[Nplots];
  matrix[Ntimes,Npreds] x[Nplots];
}

parameters {
  vector[Nplots] plot_offset;        # a unique vector matrix of intercepts for each plot
	vector[Npreds] betas;	          # overall coefficients
  real<lower=0> sigma[Nplots];        # residual error (standard deviation) for each plot
  real<lower=0,upper=1> rho[Nplots]; # error correlation for timepoints in each plot
  real<lower=0> sd_plots;
}

transformed parameters {
  cov_matrix[Ntimes] Sigma[Nplots];
  cov_matrix[Ntimes] tmp_Sigma;
  real lag;
  real<lower=0> sigma_square[Nplots];
  vector[Ntimes] yhat[Nplots];

  ####  Create AR(1) Error Covariance Matrix For Each Plot
  for(j in 1:Nplots){
    sigma_square[j] = square(sigma[j]);
    for(t1 in 1:Ntimes){
      for(t2 in 1:Ntimes){
        lag = abs(t2-t1);
        tmp_Sigma[t1,t2] = sigma_square[j]*(rho[j]^lag);
      }
    }
    Sigma[j] = tmp_Sigma;
  }

  for (j in 1:Nplots)
    yhat[j] = x[j]*betas + plot_offset[j];
}

model {
  ####  PRIORS
  for(j in 1:Nplots)
		plot_offset[j] ~ normal(0, sd_plots);
	betas ~ normal(0,1);	          # priors on overall effects
	sigma ~ weibull(2,1);
	sd_plots ~ weibull(2,1);

	####  LIKELIHOOD
  for(j in 1:Nplots)
    y[j] ~ multi_normal(yhat[j], Sigma[j]);
}
