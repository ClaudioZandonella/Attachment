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
drake::loadd(cluster_mother_tat)
drake::loadd(cluster_father_tat)

plot(cluster_mother_tat, main = "Dendrogramma")
rect.hclust(cluster_mother_tat, k=4, border="red")
plot(cluster_father_tat, main = "Dendrogramma")
rect.hclust(cluster_father_tat, k=4, border="red")

drake::loadd(data_cluster_tat)
table(data_cluster_tat$cluster_mother_tat)
table(data_cluster_tat$cluster_father_tat)

plot_scores_mother(data = data_cluster_tat)
plot_scores_father(data = data_cluster_tat)

# mclust
drake::loadd(mclust_mother)
drake::loadd(mclust_father)

plot(mclust_mother)
summary(mclust_mother)

plot(mclust_father)
summary(mclust_father)

#---- brms models ----

drake::loadd(fit_int)

summary(fit_int)
plot(brms::conditional_effects(fit_int), ask = FALSE)

plot(fit_int)

drake::loadd(stan_data)

str(fit_int$fit)
#-----




#----
