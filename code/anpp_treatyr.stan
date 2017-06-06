data {
  int<lower=0> Npreds;        # number of covariates, including intercept
  int<lower=0> Nplots;        # number of plots
  int<lower=0> Ntreats;       # number of treatments
  int<lower=0> Nobs;          # number of observations
  vector[Nobs] y;             # vector of observations
  row_vector[Npreds] x[Nobs];	# design matrix
  int plot_id[Nobs];          # vector of plot ids
  int treat_id[Nobs];         # vector of treatment ids
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
    yhat[i] = x[i]*beta + gamma_plot[plot_id[i]]; # GLM for expected values (one for each plot-year)
}

model {
  ####  PRIORS
  for(i in 1:Nplots)
		gamma_plot[i] ~ normal(0, sigma_plot); # plot-level offsets vary normally around 0
	beta ~ normal(0,1);	                     # priors on overall effects
	sigma_plot ~ weibull(2,1);	             # priors on plot std. dev.
	sigma_treat ~ weibull(2,1);	             # priors on treatment std. dev.
	sd_y ~ weibull(2,1);                     # priors on observation std. dev.

	####  LIKELIHOOD
  y ~ normal(yhat, sd_y); # observations vary normally around expected values
}

generated quantities {
  vector[Nppts] ypreds[Ntreats];
  vector[Nppts] ydiff_control_drought;
  vector[Nppts] ydiff_control_irrigate;
  vector[2] inter_diffs;
  for(i in 1:Ntreats)
    ypreds[i] = newx*beta_treat[i]; # mean predictions for each treatment
  ydiff_control_drought = ypreds[1] - ypreds[2]; # difference between mean predictions
  ydiff_control_irrigate = ypreds[1] - ypreds[3]; # difference between mean predictions
  inter_diffs[1] = beta_treat[1][1] - beta_treat[2][1]; # difference between control and drought in avg ppt year
  inter_diffs[2] = beta_treat[1][1] - beta_treat[3][1]; # difference between control and drought in avg ppt year
}
