#================================#
#====    Model Definition    ====#
#================================#

#-----    zip_brms    ----

zip_brms <- function(data){

  fit <- brms::brm(brms::bf(
      internalizing_sum ~ genere + cluster_mother_tat + cluster_father_tat,
      zi ~ genere + cluster_mother_tat + cluster_father_tat),
    data = data, family = brms::zero_inflated_poisson())

  return(fit)
}

#----    make_stan_data    ----

make_stan_data <- function(data = data_cluster_tat){
  formula <-  brms::bf(
    internalizing_sum ~ genere + cluster_mother_tat + cluster_father_tat,
    zi ~ genere + cluster_mother_tat + cluster_father_tat)

  formula <- brms:::validate_formula(formula, data = data, family = brms::zero_inflated_poisson(),
                                     autocor = NULL, sparse = NULL, cov_ranef = NULL)

  bterms <- brms:::brmsterms(formula)


  data_name <- brms:::substitute_name(data)
  data <- brms:::validate_data(data, bterms = bterms, data2 = NULL, knots = NULL)
  attr(data, "data_name") <- data_name

  sdata <- brms:::.make_standata(bterms, data = data, prior = NULL,
                          data2 = NULL, stanvars = NULL, threads = NULL)

  return(sdata)
}

#-----
