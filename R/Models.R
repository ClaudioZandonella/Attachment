#================================#
#====    Model Definition    ====#
#================================#

#----    my_check_zeroinflation    ----

# fix bug glmmTMB in performance::check_zeroinflation()
# https://github.com/easystats/performance/issues/367

#' Check Zero Infalation
#'
#' Given the model, compare the proportion of predicted zeros and observed zeros
#'
#' @param x model fit
#' @param tolerance numeric value
#'
#' @return object of class check_zi
#'
#' @examples
#' drake::loadd(fit_int_nb)
#' my_check_zeroinflation(fit_int_nb)
#'

my_check_zeroinflation <- function(x, tolerance = 0.05){
  model_info <- insight::model_info(x)
  if (!model_info$is_count) {
    stop("Model must be from Poisson-family.", call. = FALSE)
  }
  obs.zero <- sum(insight::get_response(x, verbose = FALSE) ==
                    0)
  if (obs.zero == 0) {
    insight::print_color("Model has no observed zeros in the response variable.\n",
                         "red")
    return(NULL)
  }

  # get theta
  if(is(x, "glmmTMB")){
    theta <- stats::sigma(x)
  } else {
    theta <- x$theta
  }
  mu <- stats::fitted(x)
  if (model_info$is_negbin && !is.null(theta)) {
    pred.zero <- round(sum(stats::dnbinom(x = 0, size = theta,
                                          mu = mu)))
  }
  else {
    pred.zero <- round(sum(stats::dpois(x = 0, lambda = mu)))
  }
  structure(class = "check_zi", list(predicted.zeros = pred.zero,
                                     observed.zeros = obs.zero, ratio = pred.zero/obs.zero,
                                     tolerance = tolerance))

}


#----    zinb_fit    ----

#' Fit a ZINB Model
#'
#' Fit a ZINB model with `glmmTMB::glmmTMB()`. Given the dataframe with the
#' cluster groups, fit the model for the outcome variable `y` considering the
#' given formula for `mu` and `zi ~ gender + (1|ID_class)`.
#'
#' @param data_cluster dataframe with the cluster groups and other subjects'
#'   information
#' @param y character indicanting the dependent variable ("internalizin_sum" or
#'   "externalizing_sum")
#' @param formula string indicating the predictors formula used for mu.
#'
#' @return a glmmTMB fit object
#'
#' @examples
#' drake::loadd(data_cluster)
#' zinb_fit(data = data_cluster, y = "internalizing_sum",
#'          formula = "gender + mother + father")
#'

zinb_fit<- function(data_cluster, y, formula){

  formula_mu<- paste0(y ," ~ ", formula, " + (1|ID_class)")

  fit <- glmmTMB::glmmTMB(as.formula(formula_mu),
                          ziformula = ~ gender + (1|ID_class),
                          data = data_cluster, family = glmmTMB::nbinom2())

  return(fit)
}

#----    zinb_brms_selected    ----

#' Fit Selected brms ZINB Model
#'
#' Fit selected brms ZINB model according to BF model comparison. Given the
#' dataframe with the cluster groups, fit the model for the outcome variable `y`
#' considering the formula `mu ~ gender + mother + (1|ID_class)` and
#' `zi ~ gender + (1|ID_class)`. Prior are set according to `prior_par` for
#' class "b".
#'
#' @param data dataframe with the cluster groups and other subjects' information
#'   ("data_cluster")
#' @param y character indicanting the dependent variable ("internalizin_sum" or
#'   "externalizing_sum")
#' @param prior_par string indicating the prior for parameeters of class "b"
#'
#' @return a brms fit object
#'
#' @examples
#' drake::loadd(data_cluster)
#' zinb_brms_selected(data = data_cluster, y = "internalizing_sum",
#'                    prior_par = "normal(0, 3)")
#'

zinb_brms_selected <- function(data, y, prior_par){

  formula_mu<- paste0(y ," ~ gender + mother + (1|ID_class)")

  my_prior <- brms::set_prior(prior_par, class = "b")

  fit <- brms::brm(brms::bf(as.formula(formula_mu),
                            zi ~ gender + (1|ID_class)),
                   family = brms::zero_inflated_negbinomial(),
                   data = data, prior = my_prior,
                   chains = 6, iter = 6000, cores = 6, warmup = 2000, seed = 2021)

  return(fit)
}


#----    get_model_df    ----

#' Get Model Degrees of Freedom
#'
#' Get the model df according to the number of parameters
#'
#' @param fit a model (brms fit or other)
#'
#' @return an integer value
#'
#' @examples
#' drake::loadd(fit_int_inter)
#' get_model_df(fit = fit_int_inter)
#'

get_model_df<- function(fit){

  if(is(fit, "brmsfit")){
    n_fixed <- nrow(brms::fixef(fit))
    n_random <- nrow(fit[["ranef"]])
  } else {
    fixed <- glmmTMB::fixef(fit)
    n_fixed <- length(fixed$cond) + length(fixed$zi)

    random <- glmmTMB::ranef(fit)
    n_random <- length(random$cond) + length(random$zi)
  }

  res <- n_fixed + n_random

  return(res)
}

#----    get_rel_weights    ----

#' Get Relative Fit Criterion Weights
#'
#' @param ... models to compare (brms fit for waic and loo)
#' @param ic character indicating the fit criterion to consider ("waic", "loo",
#'   "AIC", "BIC")
#'
#' @return a dataframe with the following columns:
#'   - `names` - name of the model
#'   - `ic` - fit criteria value
#'   - `diff_ic` - difference with the worst model
#'   - `rel_lik` - relative likelihood
#'   - `weights` - fit criteria weights
#'
#' @examples
#' drake::loadd(c(fit_int_zero, fit_int_mother,
#'        fit_int_additive, fit_int_inter))
#' get_rel_weights(fit_int_zero,
#'                 fit_int_mother,
#'                 fit_int_additive,
#'                 fit_int_inter, ic = "AIC")
#'

get_rel_weights <- function(..., ic = c("waic", "loo", "AIC", "BIC")){
  ic <- match.arg(ic)

  names_fit <- as.list(match.call(), )[-1] %>%
    as.character(.)

  if(ic %in% names_fit) names_fit <- names_fit[which(names_fit != ic)]

  list_fit <- list(...)

  if(ic == "waic"){
    fit_ics <- lapply(list_fit, FUN = function(x){
      brms::waic(x)$estimates["waic", 1]
      })
  } else if(ic == "loo"){
    fit_ics <- lapply(list_fit, FUN = function(x){
      brms::loo(x)$estimates["looic", 1]
    })
  } else if(ic == "AIC"){
    fit_ics <- lapply(list_fit, FUN = AIC)
  } else {
    fit_ics <- lapply(list_fit, FUN = BIC)
  }

  model_df <- vapply(list_fit, FUN = get_model_df, FUN.VALUE = numeric(1))

  res <- data.frame(names = names_fit,
                    df = model_df,
                    ic = unlist(fit_ics)) %>%
    mutate(diff_ic=max(ic)-ic,         # Compute difference
           rel_lik=exp(diff_ic/2),       # Compute relative likelihood
           weights=rel_lik/sum(rel_lik))

  return(res)
}

#----    get_data_prior_predict    ----

#' Get Data Prior Prediction
#'
#' Get values at -1\*SD, -.5\*SD, 0\*SD, .5\*SD, and 1\*SD for normal prior
#' distribution with different SD (see `prior_sd`). Values are `exp()` to get
#' values on the response scale
#'
#' @return a tibble
#'
#' @examples
#' get_data_prior_predict()
#'

get_data_prior_predict <- function(){
  tibble(prior_sd = c(.5, 1, 3, 5, 10),
         prior = paste0("$\\mathcal{N}(0, ", prior_sd, ")$")) %>%
    mutate(q_1 = exp(1 - 1 * prior_sd),
           q_2 = exp(1 - .5 * prior_sd),
           q_mean = exp(1 + 0 * prior_sd),
           q_3 = exp(1 + .5 * prior_sd),
           q_4 = exp(1 + 1 * prior_sd))
}

#----    get_post_pred    ----

#' Get Model Posterior Prediction
#'
#' Get posterior linear predicted values (not on the response variable scale)
#'
#' @param model a `brms` fit object with `gender` and `mother` as dependent
#'   variables
#'
#' @return a dataframe with predicted linear values for each condition of
#'   `gender` and `mother`on different columns.
#'
#' @examples
#' drake::loadd(brm_selected_int)
#' model <- brm_selected_int
#' get_post_pred(model = model)
#'

get_post_pred <- function(model){
  levels <- c("Secure", "Anxious", "Avoidant", "Fearful")
  new_data <- expand_grid(gender = factor(c("F", "M")),
                          mother = factor(levels, levels = levels))

  post_pred <- brms::posterior_linpred(model, newdata = new_data,
                                       re_formula = NA, summary = FALSE)

  colnames(post_pred) <- paste(new_data$gender, new_data$mother, sep = "_")

  return(as.data.frame(post_pred))
}


#=============
