library(rstan)
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

stan_data <- make_stan_data(data_cluster)
fit_H1 <- stan(file = "Stan/ZIP-model-H1.stan", data = stan_data)

round(summary(fit_H1)$summary[, "mean"],2)

round(brms::posterior_summary(brm_int_additive)[, "Estimate"],2)

post_H1 <- as.matrix(fit_H1, pars = c("db_mother[1]", "db_mother[2]", "db_mother[3]"))

colnames(post_H1)

#---- BF ----

post_mean <- apply(post_H1, 2, mean)
post_cov <- cov(post_H1)

prior_mean <- c(0, 0, 0)
prior_cov <- diag(3) * 25

# prior density equality
prior_den_eq <- mvtnorm::dmvnorm(x = 0,
                                 mean = prior_mean[2],
                                 sigma = as.matrix(prior_cov[2,2]))
dnorm(0,0,5)

# posterior density equality
post_den_eq <- mvtnorm::dmvnorm(x = 0,
                                mean = post_mean[2],
                                sigma = as.matrix(post_cov[2,2]))
dnorm(0, post_mean[2], sqrt(post_cov[2,2]))

# prior conditional probability
prior_cond_prob <- condMVNorm::pcmvnorm(
  lower = rep(0,2), upper = rep(Inf,2),
  mean = prior_mean, sigma = prior_cov,
  dependent.ind=c(1, 3), given.ind=c(2), X.given = c(0))

# posterior conditional probability
post_cond_prob <- condMVNorm::pcmvnorm(
  lower = rep(0,2), upper = rep(Inf,2),
  mean = post_mean, sigma = post_cov,
  dependent.ind=c(1, 3), given.ind=c(2), X.given = c(0))

# Resulting BF

BF_H1 <- (post_den_eq * post_cond_prob) / (prior_den_eq * prior_cond_prob)

#----
