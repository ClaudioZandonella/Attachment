#############################
####    Analysis Plan    ####
#############################

#----    settings    ----

library(targets)

# List all scripts in R/
script_list <- list.files("R", full.names = TRUE)
# Source scripts
invisible(sapply(script_list, source))

# Options
options(tidyverse.quiet = TRUE)

# Targets options
targets::tar_option_set(
  packages = c("tidyverse")
    )


#========================
#====    Workflow    ====
#========================

#----    Munge Data    ----

tar_data <- list(
  tar_target(data_raw, readRDS("Data/data_ECR.rds")),
  tar_target(data_munged, munge_data(data = data_raw)))

#----    Cluster Analysis    ----

tar_cluster <- list(
  # Get cluster fit
  tar_target(cluster_mother_fit,
             get_cluster_fit(data = data_munged, parent = "mother")),
  tar_target(cluster_father_fit,
             get_cluster_fit(data = data_munged, parent = "father")),

  # Get cluster data
  tar_target(data_cluster,
             get_data_cluster(data = data_munged,
                              cluster_mother_fit = cluster_mother_fit,
                              cluster_father_fit = cluster_father_fit)),

  # mclust check
  tar_target(mclust_mother, mclust_BIC(data = data_munged, parent = "mother")),
  tar_target(mclust_father, mclust_BIC(data = data_munged, parent = "father")))

#=============================
#====    Internalizing    ====
#=============================

#----    main analysis    ----
tar_int <- list(

  #----    ZINB analysis    ----
  tar_target(fit_int_nb,
             glmmTMB::glmmTMB(internalizing_sum ~ gender + mother * father + (1|ID_class),
                              data = data_cluster, family = glmmTMB::nbinom2())),
  tar_target(test_zero_inflated_int, my_check_zeroinflation(fit_int_nb)),

  #----    anova approach ----
  tar_target(fit_int_zinb,
             glmmTMB::glmmTMB(internalizing_sum ~ gender + mother * father + (1|ID_class),
                              ziformula = ~ gender + (1|ID_class),
                              data = data_cluster, family = glmmTMB::nbinom2())),

  tar_target(plot_zinb_int,
             get_plot_zinb(model = fit_int_zinb, attachment = "mother")),

  #----    Model Comparison int    ----
  tar_target(fit_int_zero,
             zinb_fit(data = data_cluster, y = "internalizing_sum",
                      formula = "gender")),
  tar_target(fit_int_mother,
             zinb_fit(data = data_cluster, y = "internalizing_sum",
                      formula = "gender + mother")),
  tar_target(fit_int_additive,
             zinb_fit(data = data_cluster, y = "internalizing_sum",
                      formula = "gender + mother + father")),
  tar_target(fit_int_inter,
             zinb_fit(data = data_cluster, y = "internalizing_sum",
                      formula = "gender + mother * father")),

  # AIC BIC Weights
  tar_target(AIC_weights_int,
             get_rel_weights(fit_int_zero,
                             fit_int_mother,
                             fit_int_additive,
                             fit_int_inter, ic = "AIC")),
  tar_target(BIC_weights_int,
             get_rel_weights(fit_int_zero,
                             fit_int_mother,
                             fit_int_additive,
                             fit_int_inter, ic = "BIC")),

  #----    BF encompassing priors    ----

  # Encompassing model
  tar_target(encompassing_model_int,
             get_encompassing_model(data = data_cluster,
                                    y = "internalizing_sum",
                                    prior_par = "normal(0, 3)")),
  # BF hypothesis
  tar_target(BF_null_int,
             get_BF(hypothesis = "null", encompassing_model_int)),
  tar_target(BF_monotropy_int,
             get_BF(hypothesis = "monotropy", encompassing_model_int)),
  tar_target(BF_hierarchy_int,
             get_BF(hypothesis = "hierarchy", encompassing_model_int)),
  tar_target(BF_independence_int,
             get_BF(hypothesis = "independence", encompassing_model_int)),
  tar_target(BF_integration_int,
             get_BF(hypothesis = "integration", encompassing_model_int)),

  tar_target(table_BF_int,
             get_table_BF(BF_null_int, BF_monotropy_int, BF_hierarchy_int,
                          BF_independence_int, BF_integration_int)),

  tar_target(BF_weights_int,
             get_BF_weights(BF_null_int, BF_monotropy_int, BF_hierarchy_int,
                            BF_independence_int, BF_integration_int)))

#----    Prior Sensitivity    ----
tar_int_sens <- tarchetypes::tar_map(
  # Prior Sensitivity
  values = tibble::tibble(prior_par_values = c("normal(0,.5)",
                                               "normal(0,1)",
                                               "normal(0,5)",
                                               "normal(0,10)"),
                          names = c(".5", "01", "05", "10")),
    names = names,
    tar_target(encompassing_model_ps_int,
               get_encompassing_model(data = data_cluster,
                                      y = "internalizing_sum",
                                      prior_par = prior_par_values)))

tar_int_sems_res <- list(
  tar_target(prior_sensitivity_int_.5, get_prior_sensitivity(encompassing_model_ps_int_.5)),
  tar_target(prior_sensitivity_int_01, get_prior_sensitivity(encompassing_model_ps_int_01)),
  tar_target(prior_sensitivity_int_05, get_prior_sensitivity(encompassing_model_ps_int_05)),
  tar_target(prior_sensitivity_int_10, get_prior_sensitivity(encompassing_model_ps_int_10)),

  tar_target(summary_sensitivity_int,
             get_summary_sensitivity(reference = BF_weights_int,
                                     prior_sensitivity_int_.5,
                                     prior_sensitivity_int_01,
                                     prior_sensitivity_int_05,
                                     prior_sensitivity_int_10)),

  # selected model
  tar_target(brm_selected_int,
             zinb_brms_selected(data = data_cluster,
                                y = "internalizing_sum",
                                prior_par = "normal(0, 3)")),
  tar_target(post_pred_int, get_post_pred(brm_selected_int)),
  tar_target(r2_int, brms::bayes_R2(brm_selected_int))
)

#=============================
#====    Externalizing    ====
#=============================

tar_ext <- list(

  #----    ZINB analysis    ----
  tar_target(fit_ext_nb,
             glmmTMB::glmmTMB(externalizing_sum ~ gender + mother * father + (1|ID_class),
                              data = data_cluster, family = glmmTMB::nbinom2())),
  tar_target(test_zero_inflated_ext, my_check_zeroinflation(fit_ext_nb)),

  #----    anova approach ----
  tar_target(fit_ext_zinb,
             glmmTMB::glmmTMB(externalizing_sum ~ gender + mother * father + (1|ID_class),
                              ziformula = ~ gender + (1|ID_class),
                              data = data_cluster, family = glmmTMB::nbinom2())),

  tar_target(plot_zinb_ext,
             get_plot_zinb(model = fit_ext_zinb, attachment = "mother")),

  #----    Model Comparison ext    ----
  tar_target(fit_ext_zero,
             zinb_fit(data = data_cluster, y = "externalizing_sum",
                      formula = "gender")),
  tar_target(fit_ext_mother,
             zinb_fit(data = data_cluster, y = "externalizing_sum",
                      formula = "gender + mother")),
  tar_target(fit_ext_additive,
             zinb_fit(data = data_cluster, y = "externalizing_sum",
                      formula = "gender + mother + father")),
  tar_target(fit_ext_inter,
             zinb_fit(data = data_cluster, y = "externalizing_sum",
                      formula = "gender + mother * father")),

  # AIC BIC Weights
  tar_target(AIC_weights_ext,
             get_rel_weights(fit_ext_zero,
                             fit_ext_mother,
                             fit_ext_additive,
                             fit_ext_inter, ic = "AIC")),
  tar_target(BIC_weights_ext,
             get_rel_weights(fit_ext_zero,
                             fit_ext_mother,
                             fit_ext_additive,
                             fit_ext_inter, ic = "BIC")),

  #----    BF encompassing priors    ----

  # Encompassing model
  tar_target(encompassing_model_ext,
             get_encompassing_model(data = data_cluster,
                                    y = "externalizing_sum",
                                    prior_par = "normal(0, 3)")),
  # BF hypothesis
  tar_target(BF_null_ext,
             get_BF(hypothesis = "null", encompassing_model_ext)),
  tar_target(BF_monotropy_ext,
             get_BF(hypothesis = "monotropy", encompassing_model_ext)),
  tar_target(BF_hierarchy_ext,
             get_BF(hypothesis = "hierarchy", encompassing_model_ext)),
  tar_target(BF_independence_ext,
             get_BF(hypothesis = "independence", encompassing_model_ext)),
  tar_target(BF_integration_ext,
             get_BF(hypothesis = "integration", encompassing_model_ext)),

  tar_target(table_BF_ext,
             get_table_BF(BF_null_ext, BF_monotropy_ext, BF_hierarchy_ext,
                          BF_independence_ext, BF_integration_ext)),

  tar_target(BF_weights_ext,
             get_BF_weights(BF_null_ext, BF_monotropy_ext, BF_hierarchy_ext,
                            BF_independence_ext, BF_integration_ext)))

#----    Prior Sensitivity    ----

tar_ext_sens <- tarchetypes::tar_map(
  # Prior Sensitivity
  values = tibble::tibble(prior_par_values = c("normal(0,.5)",
                                               "normal(0,1)",
                                               "normal(0,5)",
                                               "normal(0,10)"),
                          names = c(".5", "01", "05", "10")),
  names = names,
  tar_target(encompassing_model_ps_ext,
             get_encompassing_model(data = data_cluster,
                                    y = "externalizing_sum",
                                    prior_par = prior_par_values)))

tar_ext_sems_res <- list(
  tar_target(prior_sensitivity_ext_.5, get_prior_sensitivity(encompassing_model_ps_ext_.5)),
  tar_target(prior_sensitivity_ext_01, get_prior_sensitivity(encompassing_model_ps_ext_01)),
  tar_target(prior_sensitivity_ext_05, get_prior_sensitivity(encompassing_model_ps_ext_05)),
  tar_target(prior_sensitivity_ext_10, get_prior_sensitivity(encompassing_model_ps_ext_10)),

  tar_target(summary_sensitivity_ext,
             get_summary_sensitivity(reference = BF_weights_ext,
                                     prior_sensitivity_ext_.5,
                                     prior_sensitivity_ext_01,
                                     prior_sensitivity_ext_05,
                                     prior_sensitivity_ext_10)),

  tar_target(data_prior_predict, get_data_prior_predict()),

  # selected model
  tar_target(brm_selected_ext,
             zinb_brms_selected(data = data_cluster,
                                y = "externalizing_sum",
                                prior_par = "normal(0, 3)")),
  tar_target(post_pred_ext, get_post_pred(brm_selected_ext)),
  tar_target(r2_ext, brms::bayes_R2(brm_selected_ext))
)

#===========================#
#====    All Targets    ====#
#===========================#

list(
  tar_data, tar_cluster,
  tar_int, tar_int_sens, tar_int_sems_res,
  tar_ext, tar_ext_sens, tar_ext_sems_res
  )

#----
