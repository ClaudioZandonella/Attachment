#=================================#
#====    Utilities Report     ====#
#=================================#

#----    drake_load_all    ----

load_glob_env <- function(...){
  drake::loadd(..., envir = globalenv())
}

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

table_grade <- function(data_raw){
  data_raw %>%
    dplyr::filter(age_year < 12.30) %>%
    select(classe) %>%
    table()
}

#----    normal_approximation    ----

normal_approximation <- function(){
  data_plot <- data.frame(db_mother_1 = post_H1[,1],
                          db_mother_2 = post_H1[,2])
  mu <- apply(post_H1,2, mean)[c(1,2)]
  cov <- cov(post_H1[, c(1,2)])

  data_grid <- expand.grid(s_1 = seq(0, .35, length.out=100), s_2 = seq(-.1, .25, length.out=100))
  q_samp <- cbind(data_grid, prob = mvtnorm::dmvnorm(data_grid, mean = mu, sigma = cov))

  ggplot(data_plot) +
    geom_density_2d(aes(x=db_mother_1, y=db_mother_2)) +
    geom_contour(data = q_samp, aes(x=s_1, y=s_2, z=prob), col = "black")+
    theme_bw()

}

#=============
