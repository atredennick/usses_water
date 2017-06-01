data {
  int<lower=0> Npreds;        # number of covariates, including intercept
  int<lower=0> Nplots;        # number of plots
  int<lower=0> Ntreats;       # number of treatments
  int<lower=0> Nobs;          # number of observations
  vector[Nobs] y;             # vector of observations
  row_vector[Npreds] x[Nobs];	# design matrix
  matrix[Npreds,Npreds] R;	  # priors for covariance matrix
  int[Nobs] plot_id;          # vector of plot ids
  int[Nobs] treat_id;         # vector of treatment ids
}

parameters {
  vector[Npreds] beta_plot[Nplots];		    # a unique vector matrix for each plot
  vector[Npreds] beta_treat[Ntreats];	    # a unique vector matrix for each treatment
	vector[Npreds] beta_mu;	                # overall coefficients
	cov_matrix[Npreds] Sigma;				        # covariance matrix for treatment-level coefficients
	cov_matrix[Npreds] Sigma_plot[Ntreats];	# unique covariance matrix for plot-level coefficients for each treatment
	vector<lower=0>[Ntreats] sd_y;          # treatment-level observation std. dev.
}

transformed parameters {
  vector[Nobs] yhat;                      # vector of expected values (predictions)
  for (i in 1:Nobs)
    yhat[i] = x[i]*beta_plot[plot_id[i]]; # regression model for expected values (one for each plot-year)
}

model {
  ####  PRIORS
  for(i in 1:Nplots)
		beta_plot[i] ~ multi_normal(beta_treat[treat_id[i]], Sigma_plot[treat_id[i]]);	# plot-level coefficients vary normally around treatment coefs
	
	for(i in 1:Ntreats){
	  beta_treat[i] ~ multi_normal(beta_mu, Sigma); # treatment-level coefficients vary normally around overall coefficients
	  Sigma_plot[i] ~ inv_wishart(Npreds+1, R);	    # priors on covariance of effects at plot-level
	}
	
	beta_mu ~ normal(0,1);	                # priors on overall effects
	Sigma_treat ~ inv_wishart(Npreds+1, R);	# priors on covariance of effects at treatment-level
	sd_y ~ weibull(2,1);                    # priors on observation std. dev. for each treatment

	####  LIKELIHOOD
  for(i in 1:Nobs)
    y[i] ~ normal(yhat[i], sd_y[treat_id[i]]); # observations vary normally around expected values
}

generated quantities {
  # add mean regressions by treatment and calc differences
}
