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
    raw_data = readRDS("Data/data_ECR.rds"),
    data_munged = munge_data(data = raw_data),


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
                               class = data_cluster$mother,
                               parent = "mother"),
    mclust_father = mclust_BIC(data = data_munged,
                               class = data_cluster$father,
                               parent = "father"),

    #----    ZIP analysis    ----
    fit_int_poisson = glm(internalizing_sum ~ gender + mother * father,
                          data = data_cluster, family = "poisson"),
    test_zero_inflated = performance::check_zeroinflation(fit_int_poisson),

    #----    anova approach ----
    fit_int_zip = pscl::zeroinfl(internalizing_sum ~ gender + mother * father| gender,
                                 dist = "poisson", data = data_cluster),

    #----    brms Models int    ----
    brm_int_zero = zip_brms(data = data_cluster,
                            y = "internalizing_sum",
                            formula = list("gender",
                                           "gender")),
    brm_int_mother = zip_brms(data = data_cluster,
                              y = "internalizing_sum",
                              formula = list("gender + mother",
                                             "gender")),
    brm_int_additive = zip_brms(data = data_cluster,
                                y = "internalizing_sum",
                                formula = list("gender + mother + father",
                                               "gender")),
    brm_int_inter = zip_brms(data = data_cluster,
                                y = "internalizing_sum",
                                formula = list("gender + mother * father",
                                               "gender")),

    # stan_data = make_stan_data(data = data_cluster)

    waic_weights_int = get_rel_weights(brm_int_zero,
                                       brm_int_mother,
                                       brm_int_additive,
                                       brm_int_inter, ic = "waic"),
    loo_weights_int = get_rel_weights(brm_int_zero,
                                      brm_int_mother,
                                      brm_int_additive,
                                      brm_int_inter, ic = "loo"),

    # #----    brms Models ext    ----
    # brm_ext_zero = zip_brms(data = data_cluster,
    #                         y = "externalizing_sum",
    #                         formula = list("gender",
    #                                        "gender")),
    # brm_ext_mother = zip_brms(data = data_cluster,
    #                           y = "externalizing_sum",
    #                           formula = list("gender + mother",
    #                                          "gender")),
    # brm_ext_additive = zip_brms(data = data_cluster,
    #                             y = "externalizing_sum",
    #                             formula = list("gender + mother + father",
    #                                            "gender")),
    # brm_ext_inter = zip_brms(data = data_cluster,
    #                          y = "externalizing_sum",
    #                          formula = list("gender + mother * father",
    #                                         "gender")),
    #
    # waic_weights_ext = get_rel_weights(brm_ext_zero,
    #                                    brm_ext_mother,
    #                                    brm_ext_additive,
    #                                    brm_ext_inter, ic = "waic"),
    # loo_weights_ext = get_rel_weights(brm_ext_zero,
    #                                   brm_ext_mother,
    #                                   brm_ext_additive,
    #                                   brm_ext_inter, ic = "loo"),

    #----    BF encompassing priors    ----

    stan_data = make_stan_data(data_cluster, formula = list("gender + mother", "gender")),

    fit_H1 = stan(file = "Stan/ZIP-model-H1.stan", data = stan_data),
    # prior_spec = brms::set_prior("normal(0,5)", class = c("b"), dpar = c("", "zi")),
    # prior_model = brms::brm(brms::bf(internalizing_sum ~ cluster_mother + cluster_father,
    #                                  zi ~ cluster_mother + cluster_father),
    #                         data = data_cluster, family = brms::zero_inflated_poisson(),
    #                         prior = prior_spec, cores = 4, sample_prior = "only",
    #                         iter = 5000, warmup = 0)

  )
}

#----
