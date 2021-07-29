########################
####    Analysis    ####
########################

#----    load env    ----
env <- devtools::load_all()$env

#----    Plan   -----

# load plan
plan <- get_analysis_plan()

# Configure the analysis plan
config <- drake::drake_config(plan,
                              prework = "devtools::load_all()",
                              envir = env)

# Plot the analysis plan
drake::vis_drake_graph(config, font_size = 16, targets_only = FALSE)

#----    Make    ----

# Delate the analysis results
# drake::clean('brm_ext_mother', destroy = TRUE)

# Run the analysis
drake::make(prework = "devtools::load_all()",
            envir = env,
            plan = plan)

# Plot the analysis plan
drake::vis_drake_graph(config, font_size = 16, targets_only = FALSE)

#----    load    ----

# data
drake::loadd(data_munged)

#---- cluster ----

# cluster tat
drake::loadd(cluster_mother_fit)
drake::loadd(cluster_father_fit)

plot(cluster_mother_fit, main = "Dendrogramma")
rect.hclust(cluster_mother_fit, k=4, border="red")
plot(cluster_father_fit, main = "Dendrogramma")
rect.hclust(cluster_father_fit, k=4, border="red")

drake::loadd(data_cluster)
table(data_cluster$mother)
table(data_cluster$father)

plot_scores_mother(data = data_cluster)
plot_scores_father(data = data_cluster)

# mclust
drake::loadd(mclust_mother)
drake::loadd(mclust_father)

plot(mclust_mother)
summary(mclust_mother)

plot(mclust_father)
summary(mclust_father)

#=============================#
#====    Internalizing    ====#
#=============================#
#---- ZINB analysisi ----
drake::loadd(fit_int_nb)
car::Anova(fit_int_nb)
summary(fit_int_nb)

drake::loadd(test_zero_inflated_int)
test_zero_inflated_int

#---- anova approach ----
drake::loadd(fit_int_zinb)
pscl::vuong(fit_int_nb, fit_int_zinb) # Model zero-inflated is slightly better

car::Anova(fit_int_zinb) # https://rcompanion.org/handbook/J_01.html see "Zero-inflated regression example"
rcompanion::nagelkerke(fit_int_zinb)

summary(fit_int_zinb)
drake::loadd(plot_zinb_int)
plot(plot_zinb_int)

#---- brms models ----

# internalizing
drake::loadd(waic_weights_int)
drake::loadd(loo_weights_int)

drake::loadd(brm_int_mother)

summary(brm_int_mother)
plot(brm_int_mother)
brms::conditional_effects(brm_int_mother)
brms::conditional_effects(brm_int_mother, dpar = "zi")

brms::pp_check(brm_int_mother, nsamples = 100)
brms::bayes_R2(brm_int_mother)

#---- BF encompassing priors ----

drake::loadd(encompassing_model_int)

drake::loadd(BF_null_int)
drake::loadd(BF_monotropy_int)
drake::loadd(BF_hierarchical_int)
drake::loadd(BF_independent_int)
drake::loadd(BF_interaction_int)

drake::loadd(table_BF_int)
drake::loadd(BF_weights_int)

# sensitivity
drake::loadd(summary_sensitivity_int)

#=============================#
#====    Externalizing    ====#
#=============================#
#---- ZINB analysisi ----
drake::loadd(fit_ext_nb)
car::Anova(fit_ext_nb)
summary(fit_ext_nb)

drake::loadd(test_zero_inflated_ext)
test_zero_inflated_ext

#---- anova approach ----
drake::loadd(fit_ext_zinb)
pscl::vuong(fit_ext_nb, fit_ext_zinb) # Model zero-inflated is slightly better

car::Anova(fit_ext_zinb) # https://rcompanion.org/handbook/J_01.html see "Zero-inflated regression example"
rcompanion::nagelkerke(fit_ext_zinb)

summary(fit_ext_zinb)
drake::loadd(plot_zinb_ext)
plot(plot_zinb_ext)

#---- brms models ----

# externalizing
drake::loadd(waic_weights_ext)
drake::loadd(loo_weights_ext)

drake::loadd(brm_ext_mother)

summary(brm_ext_mother)
plot(brm_ext_mother)
brms::conditional_effects(brm_ext_mother)
brms::conditional_effects(brm_ext_mother, dpar = "zi")

brms::pp_check(brm_ext_mother, nsamples = 100)
brms::bayes_R2(brm_ext_mother)

#---- BF encompassing priors ----

drake::loadd(encompassing_model_ext)

drake::loadd(BF_null_ext)
drake::loadd(BF_monotropy_ext)
drake::loadd(BF_hierarchical_ext)
drake::loadd(BF_independent_ext)
drake::loadd(BF_interaction_ext)

drake::loadd(table_BF_ext)
drake::loadd(BF_weights_ext)

#===========================#


#----

drake::loadd(stan_data)

str(brm_int_additive$fit)

#-----
drake::loadd(brm_int_zero)
drake::loadd(brm_int_mother)
drake::loadd(brm_int_additive)

brms::conditional_effects(brm_int_additive)
#----

ggplot(data_cluster) +
  geom_histogram(aes(x = internalizing_sum, fill = cluster_mother),alpha = .7, bins = 20) +
  facet_grid(cluster_mother ~ .)

#----

data_plot <- brms::posterior_samples(brm_int_additive, pars = c("b_cluster_motheranxious", "b_cluster_motheravoidant"))

mu <- apply(data_plot,2, mean)[c(1,3)]
cov <- cov(data_plot[, c(1,3)])

data_grid <- expand.grid(s_1 = seq(0, .3, length.out=100), s_2 = seq(-.1, .25, length.out=100))
q_samp <- cbind(data_grid, prob = mvtnorm::dmvnorm(data_grid, mean = mu, sigma = cov))

ggplot(data_plot) +
  geom_density_2d(aes(x=b_cluster_motheranxious, y=b_cluster_motheravoidant)) +
  geom_contour(data = q_samp, aes(x=s_1, y=s_2, z=prob), col = "black")

#----

condMVNorm

mvtnorm::dmvnorm(x = c(0,0), mean = c(0,0), sigma = diag(2)*10)

condMVNorm::dcmvnorm(c(0), mean = c(0,0), sigma = diag(2)*10,
                     dependent.ind=c(1), given.ind=c(2), X.given = c(0))

obs <- condMVNorm::rcmvnorm(n = 1e4, mean = c(0,0,0), sigma = diag(3),
                            dependent.ind=c(1,2), given.ind=c(3), X.given = c(0))

res <- apply(obs, 1, function(x){
  condMVNorm::dcmvnorm(x, mean = c(0,0,0), sigma = diag(3),
                       dependent.ind=c(1,2), given.ind=c(3), X.given = c(0), log = TRUE)
})
mean(res)

condMVNorm::pcmvnorm(lower = rep(-Inf,1), upper = rep(0,1), mean = c(0,0), sigma = diag(2),
                     dependent.ind=c(1), given.ind=c(2), X.given = c(0))

#----
