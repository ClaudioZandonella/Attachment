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
# drake::clean(destroy = TRUE)

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
table(data_cluster$cluster_mother)
table(data_cluster$cluster_father)

plot_scores_mother(data = data_cluster)
plot_scores_father(data = data_cluster)

# mclust
drake::loadd(mclust_mother)
drake::loadd(mclust_father)

plot(mclust_mother)
summary(mclust_mother)

plot(mclust_father)
summary(mclust_father)

#---- brms models ----

drake::loadd(fit_int_additive)

summary(fit_int_additive)
plot(brms::conditional_effects(fit_int_additive), ask = FALSE)

plot(fit_int_additive)

drake::loadd(stan_data)

str(fit_int_additive$fit)
#-----




#----
