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
                               class = data_cluster$cluster_mother,
                               parent = "mother"),
    mclust_father = mclust_BIC(data = data_munged,
                               class = data_cluster$cluster_father,
                               parent = "father"),

    #----    ZIP analysis    ----
    fit_int_poisson = glm(internalizing_sum ~ cluster_mother * cluster_father,
                          data = data_cluster, family = "poisson"),
    test_zero_inflated = performance::check_zeroinflation(fit_int_poisson),

    #----    anova approach ----
    fit_int_zip = pscl::zeroinfl(internalizing_sum ~ cluster_mother * cluster_father,
                                 dist = "poisson", data = data_cluster),

    #----    brms Models    ----
    brm_int_zero = zip_brms(data = data_cluster,
                            y = "internalizing_sum",
                            formula = "1"),
    brm_int_mother = zip_brms(data = data_cluster,
                              y = "internalizing_sum",
                              formula = "cluster_mother"),
    brm_int_additive = zip_brms(data = data_cluster,
                                y = "internalizing_sum",
                                formula = "cluster_mother + cluster_father"),

    # brm_int_interaction = zip_brms(data = data_cluster,
    #                                y = "internalizing_sum",
    #                                formula = "cluster_mother * cluster_father"),

    # stan_data = make_stan_data(data = data_cluster)

    waic_weights = get_rel_weights(brm_int_zero,
                                   brm_int_mother,
                                   brm_int_additive, ic = "waic"),
    loo_weights = get_rel_weights(brm_int_zero,
                                  brm_int_mother,
                                  brm_int_additive, ic = "loo"),

    #----    BF encompassing priors    ----

    prior_spec = brms::set_prior("normal(0,5)", class = c("b"), dpar = c("", "zi")),
    prior_model = brms::brm(brms::bf(internalizing_sum ~ cluster_mother + cluster_father,
                                     zi ~ cluster_mother + cluster_father),
                            data = data_cluster, family = brms::zero_inflated_poisson(),
                            prior = prior_spec, cores = 4, sample_prior = "only",
                            iter = 5000, warmup = 0)

  )
}

#----
