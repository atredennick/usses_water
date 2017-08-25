##  anpp_randcoefs_model.R: Script to run GLMM analysis to test for treatment 
##  effects on the relationship between precipitation and ANPP.
##
##  NOTE: Stan may issue a couple warnings after running the MCMC that, as
##  the messages state, can be safely ignored. Just rejects a couple proposals
##  that result in ill-formed covariance matrices.
##
##  Author: Andrew Tredennick
##  Date created: June 2, 2017

##  Clear everything
rm(list = ls(all.names = TRUE))

##  Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # only for RStudio

####
####  LOAD LIBRARIES ----
####
library(tidyverse)    # Data munging
library(dplyr)        # Data summarizing
library(ggthemes)     # Pleasing ggplot themes
library(stringr)      # Working with strings
library(rstan)        # For MCMC
library(lme4)         # Mixed-effects modeling

####
####  READ IN AND EXTRACT EXPERIMENT ANPP DATA ----
####
source("read_format_data.R")

my_df <- anpp_data
my_df$y <- as.numeric(scale( log( anpp_data$anpp ) ))
m1 <- lmer( y ~ Treatment*vwc_scaled + (1|year_id) + (vwc_scaled|quadname), my_df)
summary(m1)

#### simulate fake data with extra observations and years 
Nyears <- 10
Ntreats <- 3 
Nplots <- c(10, 8, 8) # control, drought, irrigation

foo_df <- list()
for( i in 1:Ntreats){
  foo_df[[i]] <- expand.grid( rep = 1:Nplots[i], year_id = factor(1:Nyears), Treatment = i)
}

foo_df <- do.call( rbind, foo_df )
foo_df$quadname <- factor(as.numeric( factor(paste0(foo_df$Treatment, foo_df$rep))))
foo_df$year_id <- factor(foo_df$year_id)
foo_df$Treatment <- factor(foo_df$Treatment)

vwc <- exp(rnorm(Nyears, 0, 0.5))
foo_df$vwc <- vwc[foo_df$year_id]*c(0.5, 1, 1.5)[as.numeric(foo_df$Treatment)]

ggplot(foo_df, aes(x = year_id, y = vwc, color = Treatment)) + geom_point() # structure of covariates 

foo_df$vwc_scaled <- scale(foo_df$vwc)
X <- model.matrix(~ Treatment*vwc_scaled, foo_df) # fixed effects 
Z <- model.matrix(~ vwc_scaled, foo_df)           # frandom effects 

# make parameters 
library(mvtnorm)
R <- matrix(c(1, -0.4, -0.4, 1), 2,2)
tau  <- c(0.1, 0.1)
sigma <- 0.1
sigma_year <- 0.1
my_beta <- c(1, 0, 0, 1, 0, -0.6)

vcov <- diag(tau)%*%R%*%diag(tau) # covariance matrix
zz <- rmvnorm(sum(Nplots), c(0,0), vcov)
yy <- rnorm(Nyears, sigma_year)

foo_y <- NA
for( i in 1:nrow(foo_df)){ 
  foo_y[i] <- X[i,]%*%my_beta + Z[i,]%*%zz[foo_df$quadname[i],] + yy[foo_df$year_id[i]] + rnorm(1, 0, sigma)
}

foo_df$y <- scale(foo_y)
ggplot( foo_df, aes(x = vwc_scaled, y = y, color = Treatment)) + geom_point()
ggplot( foo_df, aes(x = vwc_scaled, y = y, color = Treatment, group = quadname)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = F) 



####
####  SET UP AND FIT MODEL IN STAN ----
####
##  Set up fixed and random design matrices
lmod <- lm(y ~ Treatment*vwc_scaled, foo_df)
x    <- model.matrix(lmod)
newx <- unique(x)
head(x)
lmod <- lm(y ~ vwc_scaled, foo_df)
z    <- model.matrix(lmod)

##  Make data list for Stan
anppdat <- list(
  Nobs     = nrow(foo_df),
  Npreds   = ncol(x),
  Npreds2  = ncol(z),
  Nplots   = length(unique(foo_df$quadname)),
  Ntreats  = length(unique(foo_df$Treatment)),
  Nyears   = length(unique(foo_df$year)),
  y        = as.numeric(foo_df$y),
  x        = x,
  z        = z,
  plot_id  = as.numeric(as.factor(foo_df$quadname)),
  treat_id = as.numeric(as.factor(as.character(foo_df$Treatment))),
  year_id  = as.numeric(as.factor(foo_df$year_id))
  ) # close list

##  Set Stan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(123)

##  Fit the model in Stan
rt  <- stanc("anpp_randcoefs.stan")
sm  <- stan_model(stanc_ret = rt, verbose = FALSE)
foo_fit <- sampling(sm, data = anppdat, iter = 10000, chains = 4, thin = 10)
###

fit_betas <- as.numeric( summary(foo_fit, 'beta')$summary[,1] )

df <- data.frame( x = anppdat$x, predicted = anppdat$x %*% fit_betas)

df <- df %>% 
  mutate( Treatment = ifelse( x.Treatment2 == 1, 'Drought', 'Control')) %>% 
  mutate( Treatment = ifelse( x.Treatment3 == 1, 'Irrigation', Treatment)) %>% 
  mutate( VWC = as.numeric(foo_df$vwc_scaled)) %>% 
  mutate( observed = as.numeric(foo_df$y)) %>% 
  mutate( year = anppdat$year_id)

ggplot(df, aes( x = VWC, y = observed, color = Treatment)) + 
  geom_point() + 
  geom_line( aes( y = predicted)) + 
  scale_color_manual(values = c('black', 'red', 'blue')) + 
  ylab( 'Annual Net Primary Productivity') + 
  geom_smooth( aes( group = Treatment), method = 'lm', se = F, color = 'gray')

L_u <- matrix( summary(foo_fit, 'L_u')$summary[,1], 2,2, byrow = T)
L_u #lower cholesky
L_u%*%t(L_u) # this should be the correlation matrix but it's odd that the lower diagonal is not one


####
####  OPTIONAL DIAGNOSTICS ----
####
library(bayesplot)
stan_diag(foo_fit)
draws <- as.array(foo_fit, pars = 'L_u')
mcmc_trace(draws)
