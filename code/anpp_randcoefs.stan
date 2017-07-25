data {
  int<lower=0> Npreds;        # number of covariates, including intercept
  int<lower=0> Nplots;        # number of plots
  int<lower=0> Ntreats;       # number of treatments
  int<lower=0> Nobs;          # number of observations
  int<lower=0> Nppts;         # Number of precip levels to predict
  int<lower=0> Nyears;        # Number of years
  vector[Nobs] y;             # vector of observations
  row_vector[Npreds] x[Nobs];	# design matrix
  matrix[Nppts,Npreds] newx;  # design matrix for predictions 
  matrix[Npreds,Npreds] R;	  # priors for covariance matrix
  int plot_id[Nobs];          # vector of plot ids
  int treat_id[Nobs];         # vector of treatment ids
  int year_id[Nobs];          # vector of year ids
}

parameters {
  vector[Npreds] beta_plot[Nplots];		    # a unique vector matrix for each plot
  vector[Npreds] beta_treat[Ntreats];	    # a unique vector matrix for each treatment
	vector[Npreds] beta_mu;	                # overall coefficients
	vector[Nyears] year_off;                # vector of year effects
	cov_matrix[Npreds] Sigma;				        # covariance matrix for treatment-level coefficients
	cov_matrix[Npreds] Sigma_plot[Ntreats];	# unique covariance matrix for plot-level coefficients for each treatment
	vector<lower=0>[Ntreats] sd_y;          # treatment-level observation std. dev.
	vector<lower=0>[Nyears] sigma_year;     # year std. dev. hyperprior
}

transformed parameters {
  vector[Nobs] yhat; # vector of expected values
  for (i in 1:Nobs)
    yhat[i] = x[i]*beta_plot[plot_id[i]] + year_off[year_id[i]]; # regression model for expected values (one for each plot-year)
}

model {
  ####  PRIORS
  for(i in 1:Nplots)
		beta_plot[i] ~ multi_normal(beta_treat[treat_id[i]], Sigma_plot[treat_id[i]]);	# plot-level coefficients vary normally around treatment coefs
	
	for(i in 1:Ntreats){
	  beta_treat[i] ~ multi_normal(beta_mu, Sigma); # treatment-level coefficients vary normally around overall coefficients
	  Sigma_plot[i] ~ inv_wishart(Npreds+1, R);	    # priors on covariance of effects at plot-level
	}
	
	beta_mu ~ normal(0,1);	          # priors on overall effects
	year_off ~ normal(0,sigma_year);  # priors on year effects, shared variance
	Sigma ~ inv_wishart(Npreds+1, R);	# priors on covariance of effects at treatment-level
	sd_y ~ weibull(2,1);              # priors on observation std. dev. for each treatment
	sigma_year ~ weibull(2,1);        # hyperprior on year std. dev.

	####  LIKELIHOOD
  for(i in 1:Nobs)
    y[i] ~ normal(yhat[i], sd_y[treat_id[i]]); # observations vary normally around expected values
}

generated quantities {
  vector[Nppts] ypreds[Ntreats];
  vector[Nppts] ypreds_mu;
  vector[Nppts] ydiff_control_drought;
  vector[Nppts] ydiff_control_irrigate;
  vector[Nobs] resid;
  vector[2] inter_diffs;
  resid = y - yhat;
  for(i in 1:Ntreats)
    ypreds[i] = newx*beta_treat[i]; # mean predictions for each treatment-year
  ypreds_mu = newx*beta_mu;
  ydiff_control_drought = ypreds[1] - ypreds[2]; # difference between mean predictions
  ydiff_control_irrigate = ypreds[1] - ypreds[3]; # difference between mean predictions
  inter_diffs[1] = beta_treat[1][1] - beta_treat[2][1]; # difference between control and drought in avg ppt year
  inter_diffs[2] = beta_treat[1][1] - beta_treat[3][1]; # difference between control and drought in avg ppt year
}
