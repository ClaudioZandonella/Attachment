#================================#
#====    Model Definition    ====#
#================================#

#----    zinb_brms    ----

#' Fit ZINB Model
#'
#' Given the dataframe with the cluster groups, fit the Zero Inflated Negative
#' Binomial model to predict the dependent variable (y) according to the
#' specified formula. Random effect (1|ID_class) is included for both mu and zi
#' regressions.
#'
#' @param data dataframe with the cluster groups and other subjects' information
#'   ("data_cluster")
#' @param y character indicanting the dependent variable ("internalizin_sum" or
#'   "externalizing_sum")
#' @param formula list with two character values indicating the predictors
#'   formula used for mu and zi respectively. If single character is passed th
#'   same formula is used for mu and zi regressions.
#'
#' @return An object of class "brmsfit" with added WAIC and LOO values
#'
#' @examples
#' drake::loadd(data_cluster)
#' zinb_brms(data = data_cluster, y = "internalizing_sum",
#'           formula = list("gender + mother", "gender"))
#'

zinb_brms <- function(data, y, formula){

  if(!is.list(formula)){
    formula <- list(formula,
                    formula)
  }

  formula_mu<- paste0(y ," ~ ", formula[[1]], " + (1|ID_class)")
  formula_zi <- paste0("zi ~ ", formula[[2]], " + (1|ID_class)")

  fit <- brms::brm(brms::bf(as.formula(formula_mu),
                            as.formula(formula_zi)),
    data = data, family = brms::zero_inflated_negbinomial(),
    cores = 4, seed = 2021)

  fit <- brms::add_criterion(fit, criterion = c("loo", "waic"))
  return(fit)
}

#----    get_rel_weights    ----

#' Get Relative Fit Criterion Weights
#'
#' @param ... brms models to compare
#' @param ic character indicating the fit criterion to consider ("waic" or
#'   "loo")
#'
#' @return a dataframe with the following columns:
#'   - `names` - name of the model
#'   - `ic` - fit criteria value
#'   - `diff_ic` - difference with the worst model
#'   - `rel_lik` - relative likelihood
#'   - `weights` - fit criteria weights
#'
#' @examples
#' drake::loadd(c(brm_int_zero, brm_int_mother,
#'        brm_int_additive, brm_int_inter))
#' get_rel_weights(brm_int_zero,
#'                 brm_int_mother,
#'                 brm_int_additive,
#'                 brm_int_inter, ic = "loo")
#'

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

# function no longer used in thee analysis

make_stan_data <- function(data, formula = "mother + father"){

  if(!is.list(formula)){
    formula <- list(formula,
                    formula)
  }

  formula <-  brms::bf(
    as.formula(paste0("internalizing_sum ~ ", formula[[1]])),
    as.formula(paste0("zi ~ ", formula[[2]])))

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
