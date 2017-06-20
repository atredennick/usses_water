data {
  int<lower=0> Npreds;        # number of covariates, including intercept
  int<lower=0> Nplots;        # number of plots
  int<lower=0> Nyears;       # number of treatments
  int<lower=0> Nobs;          # number of observations
  vector[Nobs] y;             # vector of observations
  row_vector[Npreds] x[Nobs];	# design matrix
  int plot_id[Nobs];          # vector of plot ids
  int year_id[Nobs];         # vector of treatment ids
  matrix[Npreds,Npreds] R;	  # priors for covariance matrix
}

parameters {
  vector[Nplots] gamma_plot;		    # a unique scalar for each plot
  vector[Npreds] beta[Nyears];	    # a unique vector matrix for each treatment
	vector[Npreds] beta_mu;	                # overall coefficients
	real<lower=0> sd_y;          # treatment-level observation std. dev.
	real<lower=0> sigma_plot;          # treatment-level observation std. dev.
	cov_matrix[Npreds] Sigma_year;	# unique covariance matrix for plot-level coefficients for each treatment
}

transformed parameters {
  vector[Nobs] yhat;                      # vector of expected values (predictions)
  for (i in 1:Nobs)
    yhat[i] = x[i]*beta[year_id[i]] + gamma_plot[plot_id[i]]; # GLM for expected values (one for each plot-year)
}

model {
  ####  PRIORS
  for(i in 1:Nplots)
		gamma_plot[i] ~ normal(0, sigma_plot); # plot-level offsets vary normally around 0
	for(j in 1:Nyears)
	  beta[j] ~ multi_normal(beta_mu, Sigma_year);
	beta_mu ~ normal(0,1);	                     # priors on overall effects
	sigma_plot ~ weibull(2,1);	             # priors on plot std. dev.
	Sigma_year ~ inv_wishart(Npreds+1, R);	# priors on covariance of effects at treatment-level
	sd_y ~ weibull(2,1);                     # priors on observation std. dev.

	####  LIKELIHOOD
  y ~ normal(yhat, sd_y); # observations vary normally around expected values
}

generated quantities {
  // vector[Nppts] ypreds[Ntreats];
  // vector[Nppts] ydiff_control_drought;
  // vector[Nppts] ydiff_control_irrigate;
  // vector[2] inter_diffs;
  // for(i in 1:Ntreats)
  //   ypreds[i] = newx*beta_treat[i]; # mean predictions for each treatment
  // ydiff_control_drought = ypreds[1] - ypreds[2]; # difference between mean predictions
  // ydiff_control_irrigate = ypreds[1] - ypreds[3]; # difference between mean predictions
}
