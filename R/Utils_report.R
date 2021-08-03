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

#' Load All Analyisis Objects
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

  # ZINB anlaysis
  load_glob_env(fit_int_nb)

  # ANOVA
  load_glob_env(fit_int_zinb)
  load_glob_env(plot_zinb_int)

  # brms models
  load_glob_env(brm_int_mother)
  load_glob_env(waic_weights_int)
  load_glob_env(loo_weights_int)

  # BF encompassing
  load_glob_env(encompassing_model_int)
  load_glob_env(table_BF_int)
  load_glob_env(BF_weights_int)
  load_glob_env(summary_sensitivity_int)

  #----    Externalizing    ----

  # ZINB anlaysis
  load_glob_env(fit_ext_nb)

  # ANOVA
  load_glob_env(fit_ext_zinb)
  load_glob_env(plot_zinb_ext)

  # brms models
  load_glob_env(brm_ext_mother)
  load_glob_env(waic_weights_ext)
  load_glob_env(loo_weights_ext)

  # BF encompassing
  load_glob_env(encompassing_model_ext)
  load_glob_env(table_BF_ext)
  load_glob_env(BF_weights_ext)
  load_glob_env(summary_sensitivity_ext)

}

#----    table_grade  ----

#' Get Table School Grade
#'
#' Get table school grade from data_raw selcting subjeects accroding to age
#'
#' @param data_raw the row dataset
#'
#' @return a contingency table
#'
#' @examples
#' drake::loadd(data_raw)
#' table_grade(data_raw)
#'

table_grade <- function(data_raw){
  data_raw %>%
    dplyr::filter(age_year < 12.30) %>%
    select(classe) %>%
    table()
}

#----    normal_approximation    ----

# Extra function not used at the moment

normal_approximation <- function(){

  drake::loadd(encompassing_model_int)
  par_post <- brms::fixef(encompassing_model_int, summary = FALSE) %>%
    as.data.frame()

  mu <- apply(par_post[, c("motherAvoidant",
                           "motherAvoidant:fatherAvoidant")],2, mean)
  cov <- cov(par_post[, c("motherAvoidant",
                          "motherAvoidant:fatherAvoidant")])

  data_grid <- expand.grid(s_1 = seq(-.6, .5, length.out=100), s_2 = seq(-.5, .75, length.out=100))
  q_samp <- cbind(data_grid, prob = mvtnorm::dmvnorm(data_grid, mean = mu, sigma = cov))

  ggplot(par_post) +
    geom_density_2d(aes(x=motherAvoidant, y=`motherAvoidant:fatherAvoidant`)) +
    geom_contour(data = q_samp, aes(x=s_1, y=s_2, z=prob), col = "black")+
    theme_bw()


}

sensitivity_plot <- function(){
  drake::loadd(encompassing_model_int)
  par_post <- brms::fixef(encompassing_model_int, summary = FALSE) %>%
    as.data.frame()

  ggplot(par_post) +
    geom_density(aes(x = motherAnxious)) +
    geom_density(aes(x = motherAvoidant)) +
    geom_density(aes(x = motherFearful)) +
    # geom_density(aes(x = fatherAnxious)) +
    # geom_density(aes(x = fatherAvoidant)) +
    # geom_density(aes(x = fatherFearful)) +
    geom_density(aes(x = `motherAnxious:fatherAnxious`)) +
    geom_density(aes(x = `motherAvoidant:fatherAvoidant`)) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = .5), col = "red") +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 3), col = "blue") +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 10), col = "green") +
    xlim(-3, 3)
}

#=============
