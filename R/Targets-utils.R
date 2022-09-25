#==================================#
#====    Utilities Targets     ====#
#==================================#

#----    load_glob_env    ----

#' Load Targets in Global Environment
#'
#' Load targets in Global Environment
#'
#' @param ... parameters passed to `targets::tar_load()` function
#' @param store the path to the tergets data store
#'
#' @return The cached value of the target
#'
#' @noRd
#' @examples
#' load_glob_env(data_plot_n_power_M_S)
#' BF_weights_int
#'

load_glob_env <- function(..., store = targets::tar_config_get("store")){
  targets::tar_load(..., envir = globalenv(), store = store)
}

#----    targets_load_all    ----

#' Load All Targets Objects
#'
#' Load all targets objects
#'
#' @return all cached value of the target
#'
#' @noRd
#' @examples
#' targets_load_all()
#'

tar_load_all <- function(store = targets::tar_config_get("store")){

  # targets
  targets <- c(
    # Data
    "data_raw", "data_munged",

    # Cluster
    "cluster_mother_fit", "cluster_father_fit",
    "data_cluster",
    "mclust_mother", "mclust_father",

    #----    Internalizing    ----

    # ZINB analysis
    "fit_int_nb", "test_zero_inflated_int",

    # ANOVA
    "fit_int_zinb", "plot_zinb_int",

    # model comparison
    "fit_int_zero", "fit_int_mother", "fit_int_additive", "fit_int_inter",
    "AIC_weights_int", "BIC_weights_int",

    # BF encompassing
    "encompassing_model_int",
    "BF_null_int", "BF_monotropy_int", "BF_hierarchy_int",
    "BF_independence_int", "BF_integration_int",
    "table_BF_int", "BF_weights_int",

    # Sensitivity
    paste0("encompassing_model_ps_int", c("_.5", "_01", "_05", "_10")),
    paste0("prior_sensitivity_int", c("_.5", "_01", "_05", "_10")),
    "summary_sensitivity_int",
    "brm_selected_int", "post_pred_int", "r2_int",

    #----    Externalizing    ----

    # ZINB analysis
    "fit_ext_nb", "test_zero_inflated_ext",

    # ANOVA
    "fit_ext_zinb", "plot_zinb_ext",

    # model comparison
    "fit_ext_zero", "fit_ext_mother", "fit_ext_additive", "fit_ext_inter",
    "AIC_weights_ext", "BIC_weights_ext",

    # BF encompassing
    "data_prior_predict",

    "encompassing_model_ext",
    "BF_null_ext", "BF_monotropy_ext", "BF_hierarchy_ext",
    "BF_independence_ext", "BF_integration_ext",
    "table_BF_ext", "BF_weights_ext",

    # Sensitivity
    paste0("encompassing_model_ps_ext", c("_.5", "_01", "_05", "_10")),
    paste0("prior_sensitivity_ext", c("_.5", "_01", "_05", "_10")),
    "summary_sensitivity_ext",
    "brm_selected_ext", "post_pred_ext", "r2_ext"
  )

  # load
  sapply(targets, load_glob_env, store = store)

  return(cat("Tartgets loaded!"))
}


tar_load_bookdown <- function(store = targets::tar_config_get("store")){

  # targets
  targets <- c(
    # Data
    "data_raw", "data_cluster",

    # Cluster
    "cluster_mother_fit", "cluster_father_fit",
    "mclust_mother", "mclust_father",

    #----    Internalizing    ----

    # ZINB analysis
    "fit_int_nb",

    # ANOVA
    "fit_int_zinb",

    # model comparison
    "fit_int_zero", "AIC_weights_int", "BIC_weights_int",

    # BF encompassing
    "encompassing_model_int", "BF_weights_int", "summary_sensitivity_int",
    "brm_selected_int", "post_pred_int", "r2_int",

    #----    Externalizing    ----

    # ZINB analysis
    "fit_ext_nb",

    # ANOVA
    "fit_ext_zinb",

    # model comparison
    "fit_ext_mother", "AIC_weights_ext", "BIC_weights_ext",

    # BF encompassing
    "data_prior_predict",

    "encompassing_model_ext", "BF_weights_ext", "summary_sensitivity_ext",
    "brm_selected_ext", "post_pred_ext", "r2_ext"
  )

  # load
  sapply(targets, load_glob_env, store = store)

  return(cat("Tartgets loaded!"))
}

tar_load_paper <- function(store = targets::tar_config_get("store")){

  # targets
  targets <- c(
    # Data
    "data_cluster",

    # BF encompassing
    "encompassing_model_ext", "BF_weights_ext", "summary_sensitivity_ext",

    "data_prior_predict"
  )

  # load
  sapply(targets, load_glob_env, store = store)

  return(cat("Tartgets loaded!"))
}

tar_load_report <- function(store = targets::tar_config_get("store")){

  # targets
  targets <- c(
    "my_data", "result"
  )

  # load
  sapply(targets, load_glob_env, store = store)

  return(cat("Tartgets loaded!"))
}
#=============
