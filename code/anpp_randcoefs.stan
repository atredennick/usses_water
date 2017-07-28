data {
  int<lower=0> Npreds;        # number of covariates, including intercept
  int<lower=0> Npreds2;       # number of random effect covariates
  int<lower=0> Nplots;        # number of plots
  int<lower=0> Ntreats;       # number of treatments
  int<lower=0> Nobs;          # number of observations
  int<lower=0> Nppts;         # Number of precip levels to predict
  int<lower=0> Nyears;        # Number of years
  vector[Nobs] y;             # vector of observations
  row_vector[Npreds] x[Nobs];	# design matrix for fixed effects
  row_vector[Npreds2] z[Nobs];# simple design matrix for random effects 
  matrix[Nppts,Npreds] newx;  # design matrix for predictions 
  matrix[Npreds2,Npreds2] R;	# priors for covariance matrix
  int plot_id[Nobs];          # vector of plot ids
  int treat_id[Nobs];         # vector of treatment ids
  int year_id[Nobs];          # vector of year ids
}

parameters {
	vector[Npreds] beta;	                  # overall coefficients
	vector[Nyears] year_off;                # vector of year effects

	cholesky_factor_corr[Npreds2] L_u;       # cholesky factor of plot ranef corr matrix
  vector[Npreds2] beta_plot[Nplots];		   # plot level random effects
	vector<lower=0>[Npreds2] sigma_u;        # subj ranef std
	
	real<lower=0> sd_y;          # treatment-level observation std. dev.
	real<lower=0> sigma_year;    # year std. dev. hyperprior
}

transformed parameters {
  vector[Nobs] yhat; # vector of expected values
  vector[Npreds2] u[Nplots]; # Transformed plot ranefs
  matrix[Npreds2,Npreds2] Sigma_u; # plot ranef cov matrix
  
  Sigma_u = diag_pre_multiply(sigma_u, L_u); # this is the plot-level covariance matrix 
  
  for(j in 1:Nplots)
    u[j] = Sigma_u * beta_plot[j]; 
  
  for (i in 1:Nobs)
    yhat[i] = x[i]*beta + z[i]*u[plot_id[i]] + year_off[year_id[i]]; # regression model for expected values (one for each plot-year)
}

model {

  ####  PRIORS
  beta ~ normal(0,2);	          # priors on treatment coefficients
  L_u ~ lkj_corr_cholesky(1); # prior on the cholesky factor which controls the correlation between plot level treatment effects (I think)
  
  for(i in 1:Nplots)
		beta_plot[i] ~ normal(0, 1);	# plot-level coefficients for intercept and slope
	
	year_off ~ normal(0,sigma_year);  # priors on year effects, shared variance
  
	####  LIKELIHOOD
  for(i in 1:Nobs)
    y[i] ~ normal(yhat[i], sd_y); # observations vary normally around expected values
}

generated quantities {
  vector[Nppts] ypreds[Ntreats];
  vector[Nppts] ypreds_mu;
  vector[Nppts] ydiff_control_drought;
  vector[Nppts] ydiff_control_irrigate;
  vector[Nobs] resid;
  vector[3] inter_diffs;
  vector[3] vwc_diffs;
  
  resid = y - yhat; # residuals
  for(i in 1:Ntreats)
    ypreds[i] = newx*beta_treat[i]; # treatment-level mean predictions
  ypreds_mu = newx*beta_mu; # mean predictions
  
  # Difference among intercepts
  inter_diffs[1] = beta_treat[1][1] - beta_treat[2][1]; # control - drought
  inter_diffs[2] = beta_treat[1][1] - beta_treat[3][1]; # control - irrigation
  inter_diffs[3] = beta_treat[2][1] - beta_treat[3][1]; # drought - irrigation
  
  # Difference among slopes
  vwc_diffs[1] = beta_treat[1][2] - beta_treat[2][2]; # control - drought
  vwc_diffs[2] = beta_treat[1][2] - beta_treat[3][2]; # control - irrigation
  vwc_diffs[3] = beta_treat[1][2] - beta_treat[3][2]; # drought - irrigation
}
