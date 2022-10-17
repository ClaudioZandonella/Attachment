########################
####    Analysis    ####
########################

#----    Analysis    ----

# Targets plan
targets::tar_config_set(script = "Analysis/Targets-workflow.R",
                        store = "Analysis/_targets/")

# Check plan
targets::tar_manifest(fields = "command")
targets::tar_visnetwork()

# Run analysis
targets::tar_make()
tar_load_all()

# End
targets::tar_visnetwork()

#----    Results    ----
tar_load_all()

# data
data_munged

#=======================
#====    cluster    ====
#=======================

#---- cluster results ----
cluster_mother_fit
cluster_father_fit

plot(cluster_mother_fit, main = "Dendrogramma")
rect.hclust(cluster_mother_fit, k=4, border="red")
plot(cluster_father_fit, main = "Dendrogramma")
rect.hclust(cluster_father_fit, k=4, border="red")

data_cluster
table(data_cluster$mother)
table(data_cluster$father)

#---- scores ----
plot_scores_cluster(parent = "mother")
plot_scores_cluster(parent = "father")

#---- mclust ----
plot(mclust_mother)
summary(mclust_mother)

plot(mclust_father)
summary(mclust_father)

#=============================
#====    Internalizing    ====
#=============================

#---- ZINB analysisi ----
# negative binomial model
car::Anova(fit_int_nb)
summary(fit_int_nb)

# test zero inflation
test_zero_inflated_int
anova(fit_int_nb, fit_int_zinb) # Model zero-inflated is slightly better

#---- anova approach ----
# ZINB model
car::Anova(fit_int_zinb) # https://rcompanion.org/handbook/J_01.html see "Zero-inflated regression example"
summary(fit_int_zinb)

# effects
get_plot_zinb(model = fit_int_zinb, attachment = "mother", gender = FALSE)
emmeans::contrast(emmeans::emmeans(fit_int_zinb, specs = ~ mother ),
                  "pairwise", adjust = "mvt")
emmeans::contrast(emmeans::emmeans(fit_int_zinb, specs = ~ mother ),
                  "pairwise", adjust = NULL)

# fit
performance::r2(fit_int_zinb)

#---- model comparison ----
AIC_weights_int
BIC_weights_int

# selected model
car::Anova(fit_int_mother)
summary(fit_int_mother)
get_plot_zinb(model = fit_int_mother, attachment = "mother", gender = FALSE)
emmeans::contrast(emmeans::emmeans(fit_int_mother, specs = ~ mother ),
                  "pairwise", adjust = "mvt")

performance::r2(fit_int_mother)

#---- BF encompassing priors ----

table_BF_int
BF_weights_int

# sensitivity
summary_sensitivity_int

# selected
summary(brm_selected_int)
plot_post_pred(post_pred_int, problem = "Internalizing")
plot_post_diff(post_pred_int, problem = "Internalizing")

r2_int
my_pp_check(brm_selected_int, problem = "Internalizing")

#=============================
#====    Externalizing    ====
#=============================

#---- ZINB analysisi ----
# negative binomial model
car::Anova(fit_ext_nb)
summary(fit_ext_nb)

# test zero inflation
test_zero_inflated_ext
anova(fit_ext_nb, fit_ext_zinb) # Model zero-inflated is slightly better

#---- anova approach ----
# ZINB model
car::Anova(fit_ext_zinb) # https://rcompanion.org/handbook/J_01.html see "Zero-inflated regression example"
summary(fit_ext_zinb)

# effects
get_plot_zinb(model = fit_ext_zinb, attachment = "mother")
emmeans::contrast(emmeans::emmeans(fit_ext_zinb, specs = ~ mother ),
                  "pairwise", adjust = "mvt")
emmeans::contrast(emmeans::emmeans(fit_ext_zinb, specs = ~ mother ),
                  "pairwise", adjust = NULL)

# fit
performance::r2(fit_ext_zinb)

#---- model comparison ----
AIC_weights_ext
BIC_weights_ext

# selected model
car::Anova(fit_ext_mother)
summary(fit_ext_mother)
get_plot_zinb(model = fit_ext_mother, attachment = "mother")
emmeans::contrast(emmeans::emmeans(fit_ext_mother, specs = ~ mother ),
                  "pairwise", adjust = "mvt")

performance::r2(fit_ext_mother)

#---- BF encompassing priors ----

table_BF_ext
BF_weights_ext

# sensitivity
summary_sensitivity_ext

# selected
summary(brm_selected_ext)
plot_post_pred(post_pred_ext, problem = "Externalizing")
plot_post_diff(post_pred_ext, problem = "Externalizing")

r2_ext
my_pp_check(brm_selected_ext, problem = "Externalizing")




#----
