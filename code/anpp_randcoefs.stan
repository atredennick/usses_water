data {
  int<lower=0> Npreds;         # number of covariates, including intercept
  int<lower=0> Npreds2;        # number of random effect covariates
  int<lower=0> Nplots;         # number of plots
  int<lower=0> Ntreats;        # number of treatments
  int<lower=0> Nobs;           # number of observations
  int<lower=0> Nyears;         # number of years
  vector[Nobs] y;              # vector of observations
  row_vector[Npreds] x[Nobs];	 # design matrix for fixed effects
  row_vector[Npreds2] z[Nobs]; # simple design matrix for random effects 
  int plot_id[Nobs];           # vector of plot ids
  int treat_id[Nobs];          # vector of treatment ids
  int year_id[Nobs];           # vector of year ids
}

parameters {
	vector[Npreds] beta;	             # overall coefficients
	vector[Nyears] year_off;           # vector of year effects
	cholesky_factor_corr[Npreds2] L_u; # cholesky factor of plot random effect corr matrix
  vector[Npreds2] beta_plot[Nplots]; # plot level random effects
	vector<lower=0>[Npreds2] sigma_u;  # plot random effect std. dev.
	real<lower=0> sd_y;                # treatment-level observation std. dev.
	real<lower=0> sigma_year;          # year std. dev. hyperprior
}

transformed parameters {
  vector[Nobs] yhat;               # vector of expected values
  vector[Npreds2] u[Nplots];       # transformed plot random effects
  matrix[Npreds2,Npreds2] Sigma_u; # plot ranef cov matrix
  
  Sigma_u = diag_pre_multiply(sigma_u, L_u); # cholesky factor for plot-level covariance matrix 
  for(j in 1:Nplots)
    u[j] = Sigma_u * beta_plot[j]; # plot random intercepts and slopes
  
  # regression model for expected values (one for each plot-year)
  for (i in 1:Nobs)
    yhat[i] = x[i]*beta + z[i]*u[plot_id[i]]; #+ year_off[year_id[i]]; 
}

model {
  ####  PRIORS
  sigma_year ~ cauchy(0,2.5);
  sd_y ~ cauchy(0,2.5);
  # year_off ~ normal(0,sigma_year); # priors on year effects, shared variance
  beta ~ normal(0,5);	             # priors on treatment coefficients
  L_u ~ lkj_corr_cholesky(2.0);      # prior on the cholesky factor which controls the 
                                    # correlation between plot level treatment effects
  sigma_u ~ cauchy(0,2.5);
  
  for(i in 1:Nplots)
		beta_plot[i] ~ normal(0,1); # plot-level coefficients for intercept and slope
	
	####  LIKELIHOOD
  for(i in 1:Nobs)
    y[i] ~ normal(yhat[i], sd_y); # observations vary normally around expected values
}

generated quantities{
  corr_matrix[Npreds2] R = multiply_lower_tri_self_transpose(L_u);
  cov_matrix[Npreds2] V = quad_form_diag(R,sigma_u);
}
