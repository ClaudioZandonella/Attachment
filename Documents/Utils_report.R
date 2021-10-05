#=================================#
#====    Utilities Report     ====#
#=================================#

#----    load_glob_env    ----

#' Load Drake in Global Environment
#'
#' Load drake object in Global Environment
#'
#' @param ... parameters passed to `drake::loadd()` function
#'
#' @return The cached value of the target
#'
#' @examples
#' load_glob_env(BF_weights_int)
#' BF_weights_int
#'

load_glob_env <- function(...){
  drake::loadd(..., envir = globalenv())
}

#----    drake_load_all    ----

#' Load All Analysis Objects
#'
#' Load all analysis objects
#'
#' @return all cached value of the target
#'
#' @examples
#' drake_load_all()
#'

drake_load_all <- function(){
  # Data
  load_glob_env(data_raw)
  load_glob_env(data_munged)
  load_glob_env(data_cluster)

  # Cluster
  load_glob_env(cluster_mother_fit)
  load_glob_env(cluster_father_fit)

  load_glob_env(mclust_mother)
  load_glob_env(mclust_father)

  #----    Internalizing    ----

  # ZINB analysis
  load_glob_env(fit_int_nb)
  load_glob_env(test_zero_inflated_int)

  # ANOVA
  load_glob_env(fit_int_zinb)
  load_glob_env(plot_zinb_int)

  # model comparison
  load_glob_env(fit_int_zero)
  load_glob_env(fit_int_mother)
  load_glob_env(AIC_weights_int)
  load_glob_env(BIC_weights_int)

  # BF encompassing
  load_glob_env(encompassing_model_int)
  load_glob_env(table_BF_int)
  load_glob_env(BF_weights_int)
  load_glob_env(summary_sensitivity_int)
  load_glob_env(brm_selected_int)
  load_glob_env(post_pred_int)
  load_glob_env(r2_int)

  #----    Externalizing    ----

  # ZINB anslysis
  load_glob_env(fit_ext_nb)
  load_glob_env(test_zero_inflated_ext)

  # ANOVA
  load_glob_env(fit_ext_zinb)
  load_glob_env(plot_zinb_ext)

  # model comparison
  load_glob_env(fit_ext_zero)
  load_glob_env(fit_ext_mother)
  load_glob_env(AIC_weights_ext)
  load_glob_env(BIC_weights_ext)

  # BF encompassing
  load_glob_env(encompassing_model_ext)
  load_glob_env(table_BF_ext)
  load_glob_env(BF_weights_ext)
  load_glob_env(summary_sensitivity_ext)
  load_glob_env(brm_selected_ext)
  load_glob_env(post_pred_ext)
  load_glob_env(r2_ext)

  load_glob_env(data_prior_predict)
}

drake_load_paper <- function(){
  # Data
  load_glob_env(data_cluster)

  # BF encompassing
  load_glob_env(encompassing_model_ext)
  load_glob_env(BF_weights_ext)
  load_glob_env(summary_sensitivity_ext)

  load_glob_env(data_prior_predict)

}

drake_load_bookdown <- function(){
  # Data
  load_glob_env(data_raw)
  load_glob_env(data_cluster)

  # Cluster
  load_glob_env(cluster_mother_fit)
  load_glob_env(cluster_father_fit)

  load_glob_env(mclust_mother)
  load_glob_env(mclust_father)

  #----    Externalizing    ----

  # Zero inflation
  load_glob_env(fit_ext_nb)

  # ANOVA
  load_glob_env(fit_ext_zinb)

  # model comparison
  load_glob_env(fit_ext_mother)
  load_glob_env(AIC_weights_ext)
  load_glob_env(BIC_weights_ext)

  # BF encompassing
  load_glob_env(data_prior_predict)

  load_glob_env(encompassing_model_ext)
  load_glob_env(BF_weights_ext)
  load_glob_env(summary_sensitivity_ext)

  load_glob_env(brm_selected_ext)
  load_glob_env(post_pred_ext)
  load_glob_env(r2_ext)

  #----    Internalizing    ----
  # Zero inflation
  load_glob_env(fit_int_nb)

  # ANOVA
  load_glob_env(fit_int_zinb)

  # model comparison
  load_glob_env(fit_int_mother)
  load_glob_env(AIC_weights_int)
  load_glob_env(BIC_weights_int)

  # BF encompassing
  load_glob_env(encompassing_model_int)
  load_glob_env(BF_weights_int)
  load_glob_env(summary_sensitivity_int)


  load_glob_env(brm_selected_int)
  load_glob_env(post_pred_int)
  load_glob_env(r2_int)
}



#----    make_my_book    ----

#' Make My Bookdown
#'
#' Render Bookdown html and pdf. It allows setting the correct working directory
#' to build the documents.
#'
#' @param subdir string indicating the path to the bookdown main file. It is
#'   used as working directory.
#'
#' @return builded bookdown is returned in "docs/" folder
#'

make_my_book <- function(subdir = "Documents/Bookdown/") {

  origwd <- setwd(file.path(subdir))
  on.exit(setwd(origwd))
  bookdown::render_book(input='_bookdown.yml', config_file='_bookdown.yml',
                        output_format = "bookdown::gitbook",
                        params = list(format = "html"))
  bookdown::render_book(input='_bookdown.yml', config_file='_bookdown.yml',
                        output_format = "bookdown::pdf_book",
                        params = list(format = "latex"))
}

#=============
