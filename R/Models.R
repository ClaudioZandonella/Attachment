#================================#
#====    Model Definition    ====#
#================================#

#----    *my_check_zeroinflation    ----

# fix bug glmmTMB in performance::check_zeroinflation()
# https://github.com/easystats/performance/issues/367

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


#----    *zinb_fit    ----

zinb_fit<- function(data_cluster, y, formula){

  formula_mu<- paste0(y ," ~ ", formula, " + (1|ID_class)")

  fit <- glmmTMB::glmmTMB(as.formula(formula_mu),
                          ziformula = ~ gender + (1|ID_class),
                          data = data_cluster, family = glmmTMB::nbinom2())

  return(fit)
}

#----    *zinb_brms_selected    ----

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


#----    *get_model_df    ----

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

#----    *get_rel_weights    ----

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

#----    *get_data_prior_predict    ----

get_data_prior_predict <- function(){
  tibble(prior_sd = c(.5, 1, 3, 5, 10),
         prior = paste0("$\\mathcal{N}(0, ", prior_sd, ")$")) %>%
    mutate(q_1 = exp(1 - 1 * prior_sd),
           q_2 = exp(1 - .5 * prior_sd),
           q_mean = exp(1 + 0 * prior_sd),
           q_3 = exp(1 + .5 * prior_sd),
           q_4 = exp(1 + 1 * prior_sd))
}

#----    *get_post_pred    ----

get_post_pred <- function(model){
  levels <- c("Secure", "Anxious", "Avoidant", "Fearful")
  new_data <- expand_grid(gender = factor(c("F", "M")),
                          mother = factor(levels, levels = levels))

  post_pred <- brms::posterior_linpred(model, newdata = new_data,
                                       re_formula = NA, summary = FALSE)

  colnames(post_pred) <- paste(new_data$gender, new_data$mother, sep = "_")

  return(as.data.frame(post_pred))
}


#-----
