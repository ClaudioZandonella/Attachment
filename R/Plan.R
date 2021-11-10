#############################
####    Analysis Plan    ####
#############################


#----    get_analysis_plan    ----

#' @title Plan of the Analysis
#'
#' @description Plan of the Analysis
#'
#' @return a \code{drake_plan} object of class \code{tbl_df} with the plan of
#'   the analysis
#'

get_analysis_plan <- function(){
  drake::drake_plan(

    #----    Munge Data    ----
    data_raw = readRDS("Data/data_ECR.rds"),
    data_munged = munge_data(data = data_raw),

    #----    Cluster Analysis    ----
    # Get cluster fit
    cluster_mother_fit = get_cluster_fit(data = data_munged, parent = "mother"),
    cluster_father_fit = get_cluster_fit(data = data_munged, parent = "father"),

    # Get cluster data
    data_cluster = get_data_cluster(data = data_munged,
                                    cluster_mother_fit = cluster_mother_fit,
                                    cluster_father_fit = cluster_father_fit),

    # mclust check
    mclust_mother = mclust_BIC(data = data_munged,
                               parent = "mother"),
    mclust_father = mclust_BIC(data = data_munged,
                               parent = "father"),

    #=============================
    #====    Internalizing    ====
    #=============================

    #----    ZINB analysis    ----
    fit_int_nb = glmmTMB::glmmTMB(internalizing_sum ~ gender + mother * father + (1|ID_class),
                                  data = data_cluster, family = glmmTMB::nbinom2()),
    test_zero_inflated_int = my_check_zeroinflation(fit_int_nb),

    #----    anova approach ----
    fit_int_zinb = glmmTMB::glmmTMB(internalizing_sum ~ gender + mother * father + (1|ID_class),
                                    ziformula = ~ gender + (1|ID_class),
                                    data = data_cluster, family = glmmTMB::nbinom2()),

    plot_zinb_int = get_plot_zinb(model = fit_int_zinb, attachment = "mother"),

    #----    Model Comparison int    ----
    fit_int_zero = zinb_fit(data = data_cluster, y = "internalizing_sum",
                            formula = "gender"),
    fit_int_mother = zinb_fit(data = data_cluster, y = "internalizing_sum",
                              formula = "gender + mother"),
    fit_int_additive = zinb_fit(data = data_cluster, y = "internalizing_sum",
                                formula = "gender + mother + father"),
    fit_int_inter = zinb_fit(data = data_cluster, y = "internalizing_sum",
                             formula = "gender + mother * father"),

    # AIC BIC Weights
    AIC_weights_int = get_rel_weights(fit_int_zero,
                                      fit_int_mother,
                                      fit_int_additive,
                                      fit_int_inter, ic = "AIC"),
    BIC_weights_int = get_rel_weights(fit_int_zero,
                                      fit_int_mother,
                                      fit_int_additive,
                                      fit_int_inter, ic = "BIC"),

    #----    BF encompassing priors    ----

    # Encompassing model
    encompassing_model_int = get_encompassing_model(data = data_cluster,
                                                    y = "internalizing_sum",
                                                    prior_par = "normal(0, 3)"),
    # BF hypothesis
    BF_null_int = get_BF(hypothesis = "null", encompassing_model_int),
    BF_monotropy_int = get_BF(hypothesis = "monotropy", encompassing_model_int),
    BF_hierarchy_int = get_BF(hypothesis = "hierarchy", encompassing_model_int),
    BF_independence_int = get_BF(hypothesis = "independence", encompassing_model_int),
    BF_integration_int = get_BF(hypothesis = "integration", encompassing_model_int),

    table_BF_int = get_table_BF(BF_null_int, BF_monotropy_int, BF_hierarchy_int,
                                BF_independence_int, BF_integration_int),

    BF_weights_int = get_BF_weights(BF_null_int, BF_monotropy_int, BF_hierarchy_int,
                                    BF_independence_int, BF_integration_int),

    # Prior Sensitivity
    encompassing_model_ps_int = drake::target(
      get_encompassing_model(data = data_cluster,
                             y = "internalizing_sum",
                             prior_par = prior_par_values),
        transform = cross(prior_par_values = c("normal(0,.5)",
                                               "normal(0,1)",
                                               "normal(0,5)",
                                               "normal(0,10)"))),

    prior_sensitivity_int_.5 = get_prior_sensitivity(encompassing_model_ps_int_normal.0..5_),
    prior_sensitivity_int_01 = get_prior_sensitivity(encompassing_model_ps_int_normal.0.1_),
    prior_sensitivity_int_05 = get_prior_sensitivity(encompassing_model_ps_int_normal.0.5_),
    prior_sensitivity_int_10 = get_prior_sensitivity(encompassing_model_ps_int_normal.0.10_),

    summary_sensitivity_int = get_summary_sensitivity(reference = BF_weights_int,
                                                      prior_sensitivity_int_.5,
                                                      prior_sensitivity_int_01,
                                                      prior_sensitivity_int_05,
                                                      prior_sensitivity_int_10),

    # selected model
    brm_selected_int = zinb_brms_selected(data = data_cluster,
                                          y = "internalizing_sum",
                                          prior_par = "normal(0, 3)"),
    post_pred_int = get_post_pred(brm_selected_int),
    r2_int = brms::bayes_R2(brm_selected_int),

    #=============================
    #----    Externalizing    ----
    #=============================

    #----    ZINB analysis    ----
    fit_ext_nb = glmmTMB::glmmTMB(externalizing_sum ~ gender + mother * father + (1|ID_class),
                                  data = data_cluster, family = glmmTMB::nbinom2()),
    test_zero_inflated_ext = my_check_zeroinflation(fit_ext_nb),

    #----    anova approach ----
    fit_ext_zinb = glmmTMB::glmmTMB(externalizing_sum ~ gender + mother * father + (1|ID_class),
                                    ziformula = ~ gender + (1|ID_class),
                                    data = data_cluster, family = glmmTMB::nbinom2()),

    plot_zinb_ext = get_plot_zinb(model = fit_ext_zinb, attachment = "mother"),

    #----    Model Comparison ext    ----
    fit_ext_zero = zinb_fit(data = data_cluster, y = "externalizing_sum",
                            formula = "gender"),
    fit_ext_mother = zinb_fit(data = data_cluster, y = "externalizing_sum",
                              formula = "gender + mother"),
    fit_ext_additive = zinb_fit(data = data_cluster, y = "externalizing_sum",
                                formula = "gender + mother + father"),
    fit_ext_inter = zinb_fit(data = data_cluster, y = "externalizing_sum",
                             formula = "gender + mother * father"),

    # AIC BIC Weights
    AIC_weights_ext = get_rel_weights(fit_ext_zero,
                                      fit_ext_mother,
                                      fit_ext_additive,
                                      fit_ext_inter, ic = "AIC"),
    BIC_weights_ext = get_rel_weights(fit_ext_zero,
                                      fit_ext_mother,
                                      fit_ext_additive,
                                      fit_ext_inter, ic = "BIC"),


    #----    BF encompassing priors    ----

    # Encompassing model
    encompassing_model_ext = get_encompassing_model(data = data_cluster,
                                                y = "externalizing_sum",
                                                prior_par = "normal(0, 3)"),
    # BF hypothesis
    BF_null_ext = get_BF(hypothesis = "null", encompassing_model_ext),
    BF_monotropy_ext = get_BF(hypothesis = "monotropy", encompassing_model_ext),
    BF_hierarchy_ext = get_BF(hypothesis = "hierarchy", encompassing_model_ext),
    BF_independence_ext = get_BF(hypothesis = "independence", encompassing_model_ext),
    BF_integration_ext = get_BF(hypothesis = "integration", encompassing_model_ext),

    table_BF_ext = get_table_BF(BF_null_ext, BF_monotropy_ext, BF_hierarchy_ext,
                                BF_independence_ext, BF_integration_ext),
    BF_weights_ext = get_BF_weights(BF_null_ext, BF_monotropy_ext, BF_hierarchy_ext,
                                    BF_independence_ext, BF_integration_ext),

    # Prior Sensitivity
    encompassing_model_ps_ext = drake::target(
      get_encompassing_model(data = data_cluster,
                             y = "externalizing_sum",
                             prior_par = prior_par_values),
      transform = cross(prior_par_values = c("normal(0,.5)",
                                             "normal(0,1)",
                                             "normal(0,5)",
                                             "normal(0,10)"))),

    prior_sensitivity_ext_.5 = get_prior_sensitivity(encompassing_model_ps_ext_normal.0..5_),
    prior_sensitivity_ext_01 = get_prior_sensitivity(encompassing_model_ps_ext_normal.0.1_),
    prior_sensitivity_ext_05 = get_prior_sensitivity(encompassing_model_ps_ext_normal.0.5_),
    prior_sensitivity_ext_10 = get_prior_sensitivity(encompassing_model_ps_ext_normal.0.10_),

    summary_sensitivity_ext = get_summary_sensitivity(reference = BF_weights_ext,
                                                      prior_sensitivity_ext_.5,
                                                      prior_sensitivity_ext_01,
                                                      prior_sensitivity_ext_05,
                                                      prior_sensitivity_ext_10),

    data_prior_predict = get_data_prior_predict(),

    # selected model
    brm_selected_ext = zinb_brms_selected(data = data_cluster,
                                          y = "externalizing_sum",
                                          prior_par = "normal(0, 3)"),
    post_pred_ext = get_post_pred(brm_selected_ext),
    r2_ext = brms::bayes_R2(brm_selected_ext)
  )
}

#----
