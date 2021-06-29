#================================#
#====    Model Definition    ====#
#================================#

#-----    zip_brms    ----

zip_brms <- function(data, y, formula){

  formula_lambda <- paste0("internalizing_sum ~ ", formula)
  formula_zi <- paste0("zi ~ ", formula)

  fit <- brms::brm(brms::bf(as.formula(formula_lambda),
                            as.formula(formula_zi)),
    data = data, family = brms::zero_inflated_poisson())

  return(fit)
}

#----    make_stan_data    ----

make_stan_data <- function(data){
  formula <-  brms::bf(
    internalizing_sum ~ gender + cluster_mother + cluster_father,
    zi ~ gender + cluster_mother + cluster_father)

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
