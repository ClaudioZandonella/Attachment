library(rstan)
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

stan_data <- make_stan_data(data_cluster, formula = list("gender + mother", "gender"))
fit_H3 <- stan(file = "Stan/ZIP-model-H2.stan", data = stan_data)

round(summary(fit_H1)$summary[, "mean"],2)

round(brms::posterior_summary(brm_int_mother)[, "Estimate"],2)

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
stan_data <- make_stan_data(data_cluster, formula = list("cluster_mother * cluster_father", "gender"))
fit_int <- stan(file = "Stan/ZIP-model.stan", data = stan_data)

plot(fit_int)
traceplot(fit_int, pars = "b_zi")
#----

fit_int_zip = pscl::zeroinfl(externalizing_sum ~ gender + cluster_mother * cluster_father,
                             dist = "poisson", data = data_cluster)
car::Anova(fit_int_zip)
summary(fit_int_zip)
rcompanion::nagelkerke(fit_int_zip)

#----- non supported hypothesis -----

# prior density equality
prior_den_eq <- mvtnorm::dmvnorm(x = c(0),
                                 mean = prior_mean[c(1)],
                                 sigma = as.matrix(prior_cov[c(1),c(1)]))
dnorm(0, 0, 5)

# posterior density equality
post_den_eq <- mvtnorm::dmvnorm(x = c(0),
                                mean = post_mean[c(1)],
                                sigma = as.matrix(post_cov[c(1),c(1)]))

ggplot(data.frame(x = 0:1)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd =5)) +
  stat_function(fun = dnorm, args = list(mean = post_mean[c(1)], sd = sqrt(post_cov[c(1),c(1)]))) +
  xlim(-.1,.1) +
  ylim(0, .25)

# prior conditional probability
prior_cond_prob <- condMVNorm::pcmvnorm(
  lower = rep(-Inf, 2), upper = rep(0,2),
  mean = prior_mean, sigma = prior_cov,
  dependent.ind=c(2, 3), given.ind=c(1), X.given = c(0))

# posterior conditional probability
post_cond_prob <- condMVNorm::pcmvnorm(
  lower = rep(-Inf, 2), upper = rep(0,2),
  mean = post_mean, sigma = post_cov,
  dependent.ind=c(2, 3), given.ind=c(1), X.given = c(0))


data_plot <- data.frame(db_mother_2 = post_H1[,2],
                        db_mother_3 = post_H1[,3])
data_grid <- expand.grid(s_1 = seq(-.5, .5, length.out=100), s_2 = seq(-.5, .5, length.out=100))
q_samp <- cbind(data_grid, prob = mvtnorm::dmvnorm(data_grid, mean = c(0,0), sigma = diag(2)*25))

ggplot(data_plot) +
  geom_density_2d(aes(x=db_mother_2, y=db_mother_3)) +
  geom_contour(data = q_samp, aes(x=s_1, y=s_2, z=prob), col = "black")+
  theme_bw()

# Resulting BF

BF_H2 <- (post_den_eq * post_cond_prob) / (prior_den_eq * prior_cond_prob)

BF_H2^-1


#-----


