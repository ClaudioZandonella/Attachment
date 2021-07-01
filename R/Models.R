#================================#
#====    Model Definition    ====#
#================================#

#-----    zip_brms    ----

zip_brms <- function(data, y, formula){

  formula_lambda <- paste0("internalizing_sum ~ ", formula)
  formula_zi <- paste0("zi ~ ", formula)

  fit <- brms::brm(brms::bf(as.formula(formula_lambda),
                            as.formula(formula_zi)),
    data = data, family = brms::zero_inflated_poisson(), cores = 4)

  fit <- brms::add_criterion(fit, criterion = c("loo", "waic"))
  return(fit)
}

#----    get_waic_weights    ----

get_rel_weights <- function(..., ic = c("waic", "loo")){
  ic <- match.arg(ic)

  names_fit <- as.list(match.call(), )[-1] %>%
    as.character(.)

  if(ic %in% names_fit) names_fit <- names_fit[which(names_fit != ic)]

  list_fit <- list(...)

  if(ic == "waic"){
    fit_ics <- lapply(list_fit, FUN = function(x){
      brms::waic(x)$estimates["waic", 1]
      })
  } else {
    fit_ics <- lapply(list_fit, FUN = function(x){
      brms::loo(x)$estimates["looic", 1]
    })
  }


  res <- data.frame(names = names_fit,
                    ic = unlist(fit_ics)) %>%
    mutate(diff_ic=max(ic)-ic,         # Compute difference
           rel_lik=exp(diff_ic/2),       # Compute relative likelihood
           weights=rel_lik/sum(rel_lik))

  return(res)
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
