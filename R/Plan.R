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

    #=============================#
    #====    Internalizing    ====#
    #=============================#

    #----    ZINB analysis    ----
    fit_int_nb = MASS::glm.nb(internalizing_sum ~ gender + mother * father,
                              data = data_cluster),
    test_zero_inflated_int = performance::check_zeroinflation(fit_int_nb),

    #----    anova approach ----
    fit_int_zinb = pscl::zeroinfl(internalizing_sum ~ gender + mother * father | gender,
                                  data = data_cluster, dist = "negbin"),

    plot_zinb_int = get_plot_zinb(model = fit_int_zinb),

    #----    brms Models int    ----
    brm_int_zero = zinb_brms(data = data_cluster,
                            y = "internalizing_sum",
                            formula = list("gender",
                                           "gender")),
    brm_int_mother = zinb_brms(data = data_cluster,
                              y = "internalizing_sum",
                              formula = list("gender + mother",
                                             "gender")),
    brm_int_additive = zinb_brms(data = data_cluster,
                                y = "internalizing_sum",
                                formula = list("gender + mother + father",
                                               "gender")),
    brm_int_inter = zinb_brms(data = data_cluster,
                                y = "internalizing_sum",
                                formula = list("gender + mother * father",
                                               "gender")),

    # Weights
    waic_weights_int = get_rel_weights(brm_int_zero,
                                       brm_int_mother,
                                       brm_int_additive,
                                       brm_int_inter, ic = "waic"),
    loo_weights_int = get_rel_weights(brm_int_zero,
                                      brm_int_mother,
                                      brm_int_additive,
                                      brm_int_inter, ic = "loo"),


    #----    BF encompassing priors    ----

    # Encompassing model
    encompassing_model_int = get_encompassing_model(data = data_cluster,
                                                    y = "internalizing_sum",
                                                    prior_par = "normal(0, 3)"),
    # BF hypothesis
    BF_null_int = get_BF(hypothesis = "null", encompassing_model_int),
    BF_monotropy_int = get_BF(hypothesis = "monotropy", encompassing_model_int),
    BF_hierarchical_int = get_BF(hypothesis = "hierarchical", encompassing_model_int),
    BF_independent_int = get_BF(hypothesis = "independent", encompassing_model_int),
    BF_interaction_int = get_BF(hypothesis = "interaction", encompassing_model_int),

    table_BF_int = get_table_BF(BF_null_int, BF_monotropy_int, BF_hierarchical_int,
                                BF_independent_int, BF_interaction_int),

    BF_weights_int = get_BF_weights(BF_null_int, BF_monotropy_int, BF_hierarchical_int,
                                    BF_independent_int, BF_interaction_int,
                                    encompassing_model = encompassing_model_int),

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

    #=============================#
    #====    Externalizing    ====#
    #=============================#

    #----    ZINB analysis    ----
    fit_ext_nb = MASS::glm.nb(externalizing_sum ~ gender + mother * father,
                              data = data_cluster),
    test_zero_inflated_ext = performance::check_zeroinflation(fit_ext_nb),

    #----    anova approach ----
    fit_ext_zinb = pscl::zeroinfl(externalizing_sum ~ gender + mother * father | gender,
                                  data = data_cluster, dist = "negbin"),

    plot_zinb_ext = get_plot_zinb(model = fit_ext_zinb),

    #----    brms Models ext    ----
    brm_ext_zero = zinb_brms(data = data_cluster,
                             y = "externalizing_sum",
                             formula = list("gender",
                                            "gender")),
    brm_ext_mother = zinb_brms(data = data_cluster,
                               y = "externalizing_sum",
                               formula = list("gender + mother",
                                              "gender")),
    brm_ext_additive = zinb_brms(data = data_cluster,
                                 y = "externalizing_sum",
                                 formula = list("gender + mother + father",
                                                "gender")),
    brm_ext_inter = zinb_brms(data = data_cluster,
                              y = "externalizing_sum",
                              formula = list("gender + mother * father",
                                             "gender")),

    # Weights
    waic_weights_ext = get_rel_weights(brm_ext_zero,
                                       brm_ext_mother,
                                       brm_ext_additive,
                                       brm_ext_inter, ic = "waic"),
    loo_weights_ext = get_rel_weights(brm_ext_zero,
                                      brm_ext_mother,
                                      brm_ext_additive,
                                      brm_ext_inter, ic = "loo"),


    #----    BF encompassing priors    ----

    # Encompassing model
    encompassing_model_ext = get_encompassing_model(data = data_cluster,
                                                y = "externalizing_sum",
                                                prior_par = "normal(0, 3)"),
    # BF hypothesis
    BF_null_ext = get_BF(hypothesis = "null", encompassing_model_ext),
    BF_monotropy_ext = get_BF(hypothesis = "monotropy", encompassing_model_ext),
    BF_hierarchical_ext = get_BF(hypothesis = "hierarchical", encompassing_model_ext),
    BF_independent_ext = get_BF(hypothesis = "independent", encompassing_model_ext),
    BF_interaction_ext = get_BF(hypothesis = "interaction", encompassing_model_ext),

    table_BF_ext = get_table_BF(BF_null_ext, BF_monotropy_ext, BF_hierarchical_ext,
                                BF_independent_ext, BF_interaction_ext),
    BF_weights_ext = get_BF_weights(BF_null_ext, BF_monotropy_ext, BF_hierarchical_ext,
                                    BF_independent_ext, BF_interaction_ext,
                                    encompassing_model = encompassing_model_ext),

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

  )
}

#----
