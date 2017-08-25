library(mvtnorm)
library(rstan)
library(ggplot2)
library(bayesplot)
library(lme4)
N <- 1000
J <- 20
my_tau <- c(0.6, 0.6) # 
my_R <- matrix( c(1,-0.5,-0.5,1), 2,2)  # correlation matrix  
my_Sig <- diag(my_tau)%*%my_R%*%diag(my_tau) # covariance matrix 
U <- rmvnorm(J, c(0,0), my_Sig)
uu <- as.numeric(cut(1:N, J))
my_sig_e <- 0.1
my_Sig
cov(my_dat)

X <- as.numeric(scale(rnorm(1000)))
beta <- c(1, 2)

mm <- model.matrix( ~ X , data.frame( X = X))
Y <- NA
for( i in 1:N){ 
  Y[i] <- rnorm(1, mm[i,]%*%(beta + U[uu[i],]), my_sig_e)
}
Y <- as.numeric(scale(Y))
plot(X,Y)
boxplot(Y ~ uu)
df <- data.frame(x = X, y = Y, group = uu)
ggplot( df , aes( x , y , group = factor(group))) + geom_point() + geom_smooth(method = 'lm', se = F)

dat_list <- list( N = N, K = ncol(mm), J = J, uu = uu, X = mm, Y = Y)
m1 <- lmer(y ~ x + (x|group) , data = df)
summary(m1)

#  Stan fit 
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(123)

fit <- stan(file = '~/Dropbox/usses_water/code/test_lkj_cor.stan', data = dat_list)

stan_beta <- summary(fit, 'beta')$summary[,1]
stan_beta
fixef(m1)

summary(fit, 'sig_e')$summary[,1]
VarCorr(m1)

L <- matrix( summary(fit, 'L')$summary[,1], 2,2 , byrow = T)
L%*%t(L) ## This should be the correlation matrix 
my_R # This is the correlation matrix used for simulation 
attr(VarCorr(m1)$group, 'correlation') # this is the LMER correlation matrix

draws <- as.array(fit, pars = 'beta')
mcmc_trace(draws)

draws <- as.array(fit, pars = 'L')
mcmc_trace(draws)

y_hat <- model.matrix( ~ x , df) %*% stan_beta
df$y_hat <- y_hat
ggplot(df, aes(x , y )) + geom_point( alpha = 0.2) + geom_line(aes(y = y_hat))
