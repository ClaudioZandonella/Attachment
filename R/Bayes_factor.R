#============================#
#====    Bayes factor    ====#
#============================#

#----    get_encompassing_model   ----

#' Fit Encompassing ZINB Model
#'
#' Given the dataframe with the cluster groups, fit the encompassing ZINB model
#' to predict the dependent variable (y). The formula is `mu ~ gender + mother *
#' father + (1|ID_class)` and `zi ~ gender + (1|ID_class)`. Prior for the fixed
#' effects can be specified. The model is estimated using 6 chains with 10000
#' iteration each (50% warmup).
#'
#' @param data dataframe with the cluster groups and other subjects' information
#'   ("data_cluster")
#' @param y character indicanting the dependent variable ("internalizing_sum" or
#'   "externalizing_sum")
#' @param prior_par character indicating the prior or the fixed effect
#'
#' @return An object of class "brmsfit" with added WAIC and LOO values
#'
#' @examples
#' drake::loadd(data_cluster)
#' get_encompassing_model(data = data_cluster,
#'                        y = "internalizing_sum",
#'                        prior_par = "normal(0, 3)")
#'

get_encompassing_model <- function(data, y,  prior_par){

  formula_mu<- paste0(y ," ~ gender + mother * father + (1|ID_class)")
  formula_zi <- paste0("zi ~ gender + (1|ID_class)")

  my_prior <- brms::set_prior(prior_par, class = "b")

  fit <- brms::brm(brms::bf(as.formula(formula_mu),
                            as.formula(formula_zi)),
                   data = data, family = brms::zero_inflated_negbinomial(),
                   prior = my_prior, chains = 6, iter = 1e4, cores = 6, warmup = 2000,
                   save_pars = brms::save_pars(all = TRUE), seed = 2021)

  return(fit)
}

#----    get_par_names    ----

#' Get Model Parameter Names
#'
#' Given the encompassing model, get model parameter names. Only parameter of
#' interest for the informative hypothesis are returned.
#'
#' @param encompassing_model a `brms` fit object of the encompassing model
#'
#' @return character sting
#'
#' @examples
#' drake::loadd(encompassing_model_int)
#' get_par_names(encompassing_model = encompassing_model_int)
#'

get_par_names<- function(encompassing_model){
  # Keep only fixed effect "b_*" excluding gender, Intercepts and "zi" related parameters
  par_names <- brms::parnames(encompassing_model) %>%
    .[grep("^b_(?!.*(Intercept|zi|gender))", ., perl = TRUE)] %>%
    gsub("^b_", replacement = "", .) %>%  # remove initial "b_"
    gsub("mother", replacement = "M_", .) %>%
    gsub("father", replacement = "F_", .)
    # gsub(":", replacement = "_", .)

  return(par_names)
}

#----    get_prior_sd    ----

#' Get Prior SD
#'
#' Given the encompassing model, geet the prior sd specified by the user for
#' fixed parameters
#'
#' @param encompassing_model a `brms` fit object of the encompassing model
#'
#' @return numeric value
#'
#' @examples
#' drake::loadd(encompassing_model_int)
#' get_prior_sd(encompassing_model = encompassing_model_int)
#'

get_prior_sd <- function(encompassing_model){
  # get prior sd from prior summary
  sd_prior <- brms::prior_summary(encompassing_model) %>%
    .[1, "prior"] %>%
    gsub("^normal\\(0,\\s*([0-9.]+)\\s*\\)", "\\1", .) %>%
    as.numeric()

  return(sd_prior)
}
#----    get_model_matrix    ----

#' Get Model Matrix
#'
#' Get the model matrix considering mother and father attachment in interaction
#'
#' @return a matrix
#'
#' @examples
#' get_model_matrix()
#'

get_model_matrix <- function(){

  levels <- c("Sec", "Anx", "Av", "Fear")

  new_data <- expand.grid(mother = factor(levels, levels = levels),
                          father = factor(levels, levels = levels))

  mm <- model.matrix(~mother*father, new_data)[,-1] # remove intercept column
  row.names(mm) <- paste0("M_",new_data$mother,
                          "_F_",new_data$father)

  return(mm)
}

#----    get_hypothesis_matrix    ----

#' Get Hypothesis Matrix
#'
#' Given the type of hypothesis and the encompassing model, retur the matrix
#' with equality constaints, the matrix with inequlity constraints, and the
#' matrix given by the union of the previous two.
#'
#' @param hypothesis a character indicating the hypothesis ("null", "monotropy",
#'   "hierarchy", "independence", "integration")
#' @param encompassing_model a `brms` fit object of the encompassing model
#'
#' @return a list with the following matrixes:
#'   - `eq_matrix` - matrix with equality constraints
#'   - `ineq_matrix` - matrix with inequality constraints
#'   - `hyp` - the resulting matrix
#'
#' @examples
#' drake::loadd(encompassing_model_int)
#' get_hypothesis_matrix(hypothesis = "null", encompassing_model = encompassing_model_int)
#'

get_hypothesis_matrix <- function(hypothesis = c("null", "monotropy", "hierarchy",
                                                 "independence", "integration"),
                                  encompassing_model){
  hypothesis <- match.arg(hypothesis)

  # Keep only fixed effect "b_*" excluding gender, Intercepts and "zi" related parameters
  par_names <- get_par_names(encompassing_model)
  n_pars <- length(par_names)

  mm <- get_model_matrix()

  if(hypothesis == "null"){
    #----    Null Hypothesis    ----

    # equality constraints
    eq_matrix <- diag(n_pars) # All terms = 0

    # inequality constraints
    ineq_matrix <- NULL

  } else if(hypothesis == "monotropy"){
    #----    Monotropy Hypothesis    ----

    # equality constraints
    eq_matrix <- rbind(mm["M_Anx_F_Sec", ] - mm["M_Av_F_Sec", ], # M_Anxious - M_Avoidant = 0
                       diag(n_pars)[4:n_pars, ])                 # All father and interaction terms = 0

    # inequality constraints
    ineq_matrix <- rbind(mm["M_Anx_F_Sec", ],                       # M_Anxious > 0
                         mm["M_Fear_F_Sec", ] - mm["M_Av_F_Sec", ]) # M_Fearful - M_Avoidant > 0

  } else if(hypothesis == "hierarchy"){
    #----    Hierarchy Hypothesis    ----

    # equality constraints
    eq_matrix <- rbind(mm["M_Anx_F_Sec", ] - mm["M_Av_F_Sec", ], # M_Anxious - M_Avoidant = 0
                       mm["M_Sec_F_Anx", ] - mm["M_Sec_F_Av", ], # F_Anxious - F_Avoidant = 0
                       diag(n_pars)[7:n_pars, ])                 # All interaction terms = 0

    # inequality constraints
    ineq_matrix <- rbind(mm["M_Anx_F_Sec", ],                         # M_Anxious > 0
                         mm["M_Fear_F_Sec", ] - mm["M_Av_F_Sec", ],   # M_Fearful - M_Avoidant > 0
                         mm["M_Sec_F_Anx", ],                         # F_Anxious > 0
                         mm["M_Sec_F_Fear", ] - mm["M_Sec_F_Av", ],   # F_Fearful - F_Avoidant > 0
                         mm["M_Anx_F_Sec", ] - mm["M_Sec_F_Anx", ],   # M_Anxious > F_Anxious
                         mm["M_Av_F_Sec", ] - mm["M_Sec_F_Av", ],     # M_Avoidant > F_Avoidant
                         mm["M_Fear_F_Sec", ] - mm["M_Sec_F_Fear", ]) # M_Fearful > F_Fearful

  } else if(hypothesis == "independence"){
    #----    Independence Hypothesis    ----

    # equality constraints
    eq_matrix <- rbind(mm["M_Anx_F_Sec", ] - mm["M_Av_F_Sec", ],  # M_Anxious - M_Avoidant = 0
                       diag(n_pars)[7:n_pars, ])                  # All interaction terms = 0

    # inequality constraints
    ineq_matrix <- rbind(mm["M_Anx_F_Sec", ],                       # M_Anxious > 0
                         mm["M_Fear_F_Sec", ] - mm["M_Av_F_Sec", ], # M_Fearful - M_Avoidant > 0
                         mm["M_Sec_F_Anx", ],                       # F_Anxious > 0
                         mm["M_Sec_F_Av", ] - mm["M_Sec_F_Anx", ],  # F_Avoidant - F_Anxious > 0
                         mm["M_Sec_F_Fear", ] - mm["M_Sec_F_Av", ]) # F_Fearful - F_Avoidant > 0

  } else {
    #----    Integration Hypothesis    ----

    # equality constraints
    eq_matrix <- rbind(mm["M_Anx_F_Sec", ] - mm["M_Av_F_Sec", ],  # M_anx - M_av = 0
                       mm["M_Anx_F_Sec", ] - mm["M_Sec_F_Anx", ], # M_anx - F_anx = 0
                       mm["M_Anx_F_Sec", ] - mm["M_Sec_F_Av", ],  # M_anx - F_av = 0

                       mm["M_Anx_F_Anx", ] - mm["M_Anx_F_Av", ],  # F_anx + M_anx_F_anx - F_av - M_anx_F_av = 0
                       mm["M_Anx_F_Anx", ] - mm["M_Av_F_Anx", ],  # M_anx + M_anx_F_anx - M_av - M_av_F_anx = 0
                       mm["M_Anx_F_Anx", ] - mm["M_Av_F_Av", ],   # M_anx + F_anx + M_anx_F_anx - M_av - F_av - M_av_F_av = 0

                       mm["M_Fear_F_Anx", ] - mm["M_Fear_F_Av", ],   # F_anx + M_f_F_anx - F_av - M_f_F_av = 0
                       mm["M_Fear_F_Anx", ] - mm["M_Anx_F_Fear", ],  # M_f + F_anx + M_f_F_anx - M_anx - F_f - M_anx_F_f = 0
                       mm["M_Fear_F_Anx", ] - mm["M_Av_F_Fear", ])   # M_f + F_anx + M_f_F_anx - M_av - F_f - M_av_F_f= 0

    # inequality constraints
    ineq_matrix <- rbind(mm["M_Anx_F_Sec", ],    # M_Anxious > 0
                         mm["M_Anx_F_Anx", ] - mm["M_Anx_F_Sec", ],  # F_anx + M_anx_F_anx > 0
                         mm["M_Fear_F_Anx", ] - mm["M_Anx_F_Anx", ],  # M_f + M_f_F_anx - M_anx - M_anx_F_anx> 0
                         mm["M_Fear_F_Fear", ] - mm["M_Fear_F_Anx", ])  # F_f + M_f_F_f - F_anx - M_f_F_anx > 0
    }

  if(hypothesis == "null"){
    colnames(eq_matrix) <- par_names
  } else {
    colnames(eq_matrix) <- colnames(ineq_matrix) <- par_names
  }
  hyp <- rbind(eq_matrix, ineq_matrix)

  # if(Matrix::rankMatrix(hyp)[1] < nrow(hyp)) stop("rank matrix < nrow matrix")

  return(list(eq = eq_matrix,
              ineq = ineq_matrix,
              hyp = hyp))
}

#----    compute_density    ----

#' Compute Density
#'
#' Compute the density of the multivariate normal distribution considering
#' equality constraints.
#'
#' @param mean vector with the parameters mean
#' @param sigma covariance matrix of the parameters
#' @param n_eq integer indicating the number of equality constraints
#'
#' @return a numeric single value
#'
#' @examples
#' compute_density(mean = c(0,0, 0), sigma = diag(3), n_eq = 2)
#'

compute_density <- function(mean, sigma, n_eq){

  seq <-  if(n_eq == 1) 1 else 1:n_eq

  res <- mvtnorm::dmvnorm(x = rep(0, n_eq),
                   mean = mean[seq],
                   sigma = as.matrix(sigma[seq, seq]))

  return(res)
}

#----    compute_cond_prob    ----

#' Compute Conditional Probability
#'
#' Compute the conditional probability of order constraints oft the multivariate
#' normal distribution given the equality constraints.
#'
#' @param mean vector with the parameters mean
#' @param sigma covariance matrix of the parameters
#' @param n_eq integer indicating the number of equality constraints
#' @param n_ineq integer indicating the number of inequality constraints
#'
#' @return a numeric single value
#'
#' @examples
#' compute_cond_prob(mean = c(0,0, 0), sigma = diag(3), n_eq = 2, n_ineq = 1)
#'

compute_cond_prob <- function(mean, sigma, n_eq, n_ineq){

  res <- condMVNorm::pcmvnorm(
    lower = rep(0,n_ineq), upper = rep(Inf,n_ineq),
    mean = mean, sigma = sigma,
    dependent.ind = (n_eq+1):(n_eq + n_ineq),
    given.ind = 1:n_eq, X.given = rep(0,n_eq))

  return(res)
}

#----    get_prior_info    ----

#' Get Prior Info
#'
#' Given the encompassing model, get prior mean and covariance matrix
#'
#' @param encompassing_model a `brms` fit object of the encompassing model
#' @param hyp a matrix with the hypothesis constraints returned from
#'   `get_hypothesis_matrix()` function
#'
#' @return list with the prior mean and covariance matrix
#'
#' @examples
#' drake::loadd(encompassing_model_int)
#' hyp <- get_hypothesis_matrix(hypothesis = "hierarchy", encompassing_model_int)$hyp
#' get_prior_info(encompassing_model = encompassing_model_int, hyp = hyp)
#'

get_prior_info <- function(encompassing_model, hyp){
  prior_sd <- get_prior_sd(encompassing_model)
  n_par <- ncol(hyp)
  prior_mean <- rep(0, n_par)
  prior_cov <- diag(n_par) * prior_sd^2

  return(list(mean = prior_mean,
              cov = prior_cov))
}

#----    get_posterior_info    ----

#' Get Posterior Info
#'
#' Given the encompassing model, get posterior mean and covariance matrix
#'
#' @param encompassing_model a `brms` fit object of the encompassing model
#'
#' @return list with the posterior mean and covariance matrix
#'
#' @examples
#' drake::loadd(encompassing_model_int)
#' get_posterior_info(encompassing_model = encompassing_model_int)
#'

get_posterior_info <- function(encompassing_model){
  posterior <- brms::fixef(encompassing_model, summary = FALSE) %>%
    .[ , !colnames(.) %in% c("Intercept", "zi_Intercept", "zi_genderM", "genderM")]
  posterior_mean <- colMeans(posterior)
  posterior_cov <- cov(posterior)

  return(list(mean = posterior_mean,
              cov = posterior_cov))
}

#----    transform_param    ----

#' Linear Transformation of the Parameters
#'
#' Compute mean and covariance of the linear transformed parameters. If prior
#' the vector mean is set to zero.
#'
#' @param param_info result of `get_posterior_info()` or `get_prior_info()`
#'   function
#' @param hyp matrix of the linear transformation
#' @param prior logical value indicating whether is prior.
#'
#' @return list with the transformed mean and covariance matrix
#'
#' @examples
#' drake::loadd(encompassing_model_int)
#' param_info <- get_posterior_info(encompassing_model_int)
#' hyp <- get_hypothesis_matrix("hierarchy", encompassing_model_int)$hyp[1:15, ]
#' transform_param(param_info, hyp)

transform_param <- function(param_info, hyp, prior = FALSE){

  if(isTRUE(prior)){
    param_mean <- matrix(rep(0, length(param_info$mean)), ncol = 1)
  } else {
    param_mean <- hyp %*% param_info$mean
  }
  param_cov <- hyp %*% param_info$cov %*% t(hyp)

  return(list(mean = param_mean,
              cov = param_cov))
}

#----    get_BF    ----

#' Compute the Bayes Factor
#'
#' Given the hypothesis and the encompassing model, compute the Bayes Factor
#'
#' @param hypothesis a character indicating the hypothesis ("null", "monotropy",
#'   "hierarchy", "independence", "iteraction")
#' @param encompassing_model a `brms` fit object of the encompassing model
#'
#' @return numeric value
#'
#' @examples
#' drake::loadd(encompassing_model_int)
#' get_BF(hypothesis = "monotropy", encompassing_model = encompassing_model_int)
#'

get_BF <- function(hypothesis = c("null", "monotropy", "hierarchy",
                                  "independence", "integration"),
                   encompassing_model){
  hypothesis <- match.arg(hypothesis)

  # Hypothesis info
  hyp_matrix <- get_hypothesis_matrix(hypothesis = hypothesis,
                                      encompassing_model = encompassing_model)
  n_eq <- nrow(hyp_matrix$eq)
  n_ineq <- nrow(hyp_matrix$ineq)

  # get prior info
  prior <- get_prior_info(encompassing_model = encompassing_model,
                          hyp = hyp_matrix$hyp)
  # get posterior info
  posterior <- get_posterior_info(encompassing_model = encompassing_model)

  if(hypothesis == "null"){
    #----    Null Hypotesis    ----

    # prior density equality
    prior_den_eq <- compute_density(prior$mean, prior$cov, n_eq)

    # posterior density equality
    post_den_eq <- compute_density(posterior$mean, posterior$cov, n_eq)

    # prior conditional probability
    prior_cond_prob <- 1

    # posterior conditional probability
    post_cond_prob <- 1


    } else if(hypothesis == "monotropy"){
      #----    Monotropy Hypothesis    ----

      # Linear transformation of the parameters
      prior <- transform_param(prior, hyp_matrix$hyp, prior = TRUE)
      posterior <- transform_param(posterior, hyp_matrix$hyp)

      # prior density equality
      prior_den_eq <- compute_density(prior$mean, prior$cov, n_eq)

      # posterior density equality
      post_den_eq <- compute_density(posterior$mean, posterior$cov, n_eq)

      # prior conditional probability
      prior_cond_prob <- compute_cond_prob(prior$mean, prior$cov,
                                           n_eq = n_eq, n_ineq = n_ineq)

      # posterior conditional probability
      post_cond_prob <- compute_cond_prob(posterior$mean, posterior$cov,
                                          n_eq = n_eq, n_ineq = n_ineq)

    } else if(hypothesis == "hierarchy"){
      #----    Hierarchy Hypothesis    ----

      # In the Hierarchy hypothesis the constraints at line 1 to 15 are
      # linearly independence. The last three inequality constraints are
      # not linearly independence:
      # beta_16 = beta_12 - beta_14
      # beta_17 = - beta_1 + beta_2 + beta_12 - beta_14 = - beta_1 + beta_2 + beta_16
      # beta_18 = - beta_1 + beta_2 + beta_12 + beta_13 - beta_14 - beta_15 = beta_17 + beta_13 - beta_15

      independent_rows <- 1:15
      composition_betas <- find_composition_betas(hyp_matrix$hyp,
                                                  independent_rows = independent_rows)

      # Linear transformation of the parameters
      prior <- transform_param(prior, hyp_matrix$hyp[1:15, ], prior = TRUE)
      posterior <- transform_param(posterior, hyp_matrix$hyp[1:15, ])

      # prior density equality
      prior_den_eq <- compute_density(prior$mean, prior$cov, n_eq)

      # posterior density equality
      post_den_eq <- compute_density(posterior$mean, posterior$cov, n_eq)

      # prior conditional probability
      obs <- condMVNorm::rcmvnorm(
        5e7, mean = prior$mean, sigma = prior$cov,
        dependent.ind = (n_eq + 1):(n_eq + n_ineq -3),
        given.ind = 1:n_eq, X.given = rep(0,n_eq))

      # given beta_1 = beta_2 = 0 we have:
      #   beta_16 = beta_12 - beta_14
      #   beta_17 = beta_16
      #   beta_18 = beta_12 + beta_13 - beta_14 - beta_15

      test <- composition_betas[c(12:16, 18), paste0("beta_",12:15)] %*% t(obs)
      prior_cond_prob <- mean(colSums(test >= 0) == nrow(test))


      # posterior conditional probability
      obs <- condMVNorm::rcmvnorm(
        5e7, mean = posterior$mean, sigma = posterior$cov,
        dependent.ind = (n_eq + 1):(n_eq + n_ineq -3),
        given.ind = 1:n_eq, X.given = rep(0,n_eq))

      test <- composition_betas[c(12:16, 18), paste0("beta_",12:15)] %*% t(obs)
      post_cond_prob <- mean(colSums(test >= 0) == nrow(test))

    } else if(hypothesis == "independence"){
      #----    Independence Hypothesis    ----

      # Linear transformation of the parameters
      prior <- transform_param(prior, hyp_matrix$hyp, prior = TRUE)
      posterior <- transform_param(posterior, hyp_matrix$hyp)

      # prior density equality
      prior_den_eq <- compute_density(prior$mean, prior$cov, n_eq)

      # posterior density equality
      post_den_eq <- compute_density(posterior$mean, posterior$cov, n_eq)

      # prior conditional probability
      prior_cond_prob <- compute_cond_prob(prior$mean, prior$cov,
                                           n_eq = n_eq, n_ineq = n_ineq)

      # posterior conditional probability
      post_cond_prob <- compute_cond_prob(posterior$mean, posterior$cov,
                                          n_eq = n_eq, n_ineq = n_ineq)

    } else if(hypothesis == "integration"){
      #----    Integration Hypothesis    ----

      # Linear transformation of the parameters
      prior <- transform_param(prior, hyp_matrix$hyp, prior = TRUE)
      posterior <- transform_param(posterior, hyp_matrix$hyp)

      # prior density equality
      prior_den_eq <- compute_density(prior$mean, prior$cov, n_eq)

      # posterior density equality
      post_den_eq <- compute_density(posterior$mean, posterior$cov, n_eq)

      # prior conditional probability
      prior_cond_prob <- compute_cond_prob(prior$mean, prior$cov,
                                           n_eq = n_eq, n_ineq = n_ineq)

      # posterior conditional probability
      post_cond_prob <- compute_cond_prob(posterior$mean, posterior$cov,
                                          n_eq = n_eq, n_ineq = n_ineq)
    }

  # Resulting BF

  BF <-  (post_den_eq * post_cond_prob) / (prior_den_eq * prior_cond_prob)[1]

  return(BF)
}

#----    find_transform_parameters    ----

#' Get Matrix Parameters transformed
#'
#' Obtain the matrix that define how the original parameters (mother*father) are
#' expressed as composition of the new linear independent parameters betas
#'
#' @param hyp the hypothesis matrix with th enew parameters betas
#' @param independent_rows numeric vector indicating the linearly independent betas
#'
#' @return a matrix
#'
#' @examples
#' drake::loadd(encompassing_model_int)
#' independent_rows <- c(1, 2, 3, 4, 5, 9, 13, 15, 17, 19, 21, 25, 29, 33, 37)
#' hyp <- get_hypothesis_matrix(hypothesis = "independence", encompassing_model_int)$hyp
#' find_transform_parameters(hyp = hyp, independent_rows)
#'

find_transform_parameters <- function(hyp, independent_rows){

  bases <- lapply(as.list(colnames(hyp)), function(x){
    betas <- vector("numeric", length = ncol(hyp))
    names(betas) <- paste0("beta_", independent_rows)
    return(betas)
  })
  names(bases) <- colnames(hyp)


  # find simple basis
  simple <- which(rowSums(abs(hyp))==1)
  for(i in simple){
    col_index <- which(hyp[i,] != 0)

    if(length(col_index) != 1L) stop("fail retrive simple basis")

    bases[[col_index]][paste0("beta_", i)] <- 1
  }

  # find complex bases

  for (i in seq_along(bases)){

    if(sum(bases[[i]]) != 0) next # skip simple basis

    row_index <- which(abs(hyp[ ,i])==1L)[1] # which row is used as the base

    if(abs(hyp[row_index, i]) !=1L) stop("issue identifing row to use as base")

    sig_index <- sign(hyp[row_index, i]) # used to invert in case of -1

    # consider -1
    minus <- sapply(which(hyp[row_index,] == -1), function(x){ bases[[x]]}) %>%
      rowSums()
    # consider +1
    plus <- sapply(which(hyp[row_index,] == 1), function(x){bases[[x]]}) %>%
      rowSums()

    betas <- - 1 * sig_index * plus + sig_index * minus
    betas[paste0("beta_", row_index)] <- betas[paste0("beta_", row_index)] + sig_index

    bases[[i]] <- betas
  }

  res <- do.call("rbind", bases)

  return(res)
}

#----    find_composition_betas    ----

#' Get Matrix Composition Betas
#'
#' Obtain the matrix that define how all the betas are expressed as composition
#' of the linear independent parameters betas.
#'
#' @param hyp the hypothesis matrix with th enew parameters betas
#' @param independent_rows numeric vector indicating the linearly independent betas
#'
#' @return a matrix
#'
#' @examples
#' drake::loadd(encompassing_model_int)
#' independent_rows <- c(1, 2, 3, 4, 5, 9, 13, 15, 17, 19, 21, 25, 29, 33, 37)
#' hyp <- get_hypothesis_matrix(hypothesis = "integration", encompassing_model_int)$hyp
#' find_composition_betas(hyp = hyp, independent_rows)
#'

find_composition_betas <- function(hyp, independent_rows){

  betas <- lapply(as.list(seq_len(nrow(hyp))), function(x){
    beta <- vector("numeric", length = ncol(hyp))
    names(beta) <- paste0("beta_", independent_rows)
    return(beta)
  })
  names(betas) <- paste0("beta_", seq_len(nrow(hyp)))

  matrix_comp <- find_transform_parameters(hyp = hyp, independent_rows = independent_rows)

  for(i in seq_len(nrow(hyp))){
    beta <- rbind(matrix_comp[which(hyp[i,] == 1), ], - matrix_comp[which(hyp[i,] == - 1), ]) %>%
      colSums()

    betas[[i]] <- beta
  }

  res <- do.call("rbind", betas)

  return(res)
}

#----    get_table_BF    ----

#' Get Table BF Comparison
#'
#' Get Table of Bayes Factor comparison, each cell represent the Bayes Factor
#' between the model indicated in the row and the model indicated in the column
#'
#' @param ... BF to compare
#'
#' @return a matrix
#'
#' @examples
#' drake::loadd(c(BF_null_int, BF_monotropy_int,
#'        BF_hierarchy_int, BF_independence_int))
#' get_table_BF(BF_null_int,
#'              BF_monotropy_int,
#'              BF_hierarchy_int,
#'              BF_independence_int)
#'

get_table_BF <- function(...){

  names_bf <- as.list(match.call(), )[-1] %>%
    as.character(.)
  n <- length(names_bf)


  list_bf <- list(...)

  bf <- lapply(list_bf, FUN = function(x){x[1]}) %>%
    unlist()

  res <- matrix(rep(bf, n), ncol = n)/matrix(rep(bf,n), ncol = n, byrow = TRUE)

  colnames(res) <- rownames(res) <- names_bf


  return(res)
}

#----    get_BF_weights    ----

#' Get BF Weights
#'
#' Given a set of Bayes Factor, compute the relative weights
#'
#' @param ... BF to compare
#'
#' @return a dataframe with columns:
#'  - `names` - name of the model
#'  - `bf` - bayes factor value
#'  - `weights` - relative weights
#'
#' @examples
#' drake::loadd(c(BF_null_int, BF_monotropy_int,
#'        BF_hierarchy_int, BF_independence_int, BF_integration_int))
#' get_BF_weights(BF_null_int,
#'                BF_monotropy_int,
#'                BF_hierarchy_int,
#'                BF_independence_int,
#'                BF_integration_int,
#'                encompassing_model = encompassing_model_int)
#'

get_BF_weights <- function(...){

  names_bf <- as.list(match.call(), )[-1] %>%
    as.character(.)

  list_bf <- list(...)

  bf <- lapply(list_bf[], FUN = function(x){x[1]}) %>%
    unlist()

  res <- data.frame(names = names_bf,
                    bf = bf) %>%
    mutate(weights= bf /sum(bf))

  return(res)
}
#----    get_BF_weights_old    ----

#' Get BF Weights OLD
#'
#' Given a seet of Bayes Factor, compute the relative weights
#'
#' @param ... BF to compare
#' @param encompassing_model a `brms` fit object of the encompassing model
#'
#' @return a dataframe with columns:
#'  - `names` - name of the model
#'  - `bf` - bayes factor value
#'  - `logml` - log marginal likelihood
#'  - `diff_logml` - difference  with the log morginal likelihood of the worst
#'  model
#'  - `rel_lik` - relative likelihood
#'  - `weights` - relative weights
#'
#' @examples
#' drake::loadd(c(BF_null_int, BF_monotropy_int,
#'        BF_hierarchy_int, BF_independence_int, BF_integration_int))
#' drake::loadd(encompassing_model_int)
#' get_BF_weights_old(BF_null_int,
#'                BF_monotropy_int,
#'                BF_hierarchy_int,
#'                BF_independence_int,
#'                BF_integration_int,
#'                encompassing_model = encompassing_model_int)
#'

get_BF_weights_old <- function(..., encompassing_model){

  names_bf <- as.list(match.call(), )[-1] %>%
    as.character(.)

  names_bf <- names_bf[!grepl("(^encompassing_model)", names_bf)] %>%
    gsub("BF_(.+)_.*", "\\1", ., perl = TRUE)
  bridge_logml <- bridgesampling::bridge_sampler(encompassing_model)

  list_bf <- list(...)

  bf <- lapply(list_bf[], FUN = function(x){x[1]}) %>%
    unlist()

  res <- data.frame(names = names_bf,
                    bf = bf) %>%
    mutate(logml = log(bf) + bridge_logml$logml,
           diff_logml = logml - min(logml) ,
           rel_lik = exp(diff_logml),
           weights= rel_lik /sum(rel_lik))

  return(res)
}

#----    get_prior_sensitivity    ----

#' Get Prior Sensitivity
#'
#' Given the encompassing model compute the bayes Factor for the attachment
#' models and return the relative weights
#'
#' @param encompassing_model a `brms` fit object of the encompassing model
#'
#' @return a dataframe result of `get_BF_weights()` function
#'
#' @examples
#' drake::loadd(data_cluster)
#' get_prior_sensitivity(data = data_cluster,
#'                       y = "internalizing_sum",
#'                       list_prior = list("normal(0,.5)"))
#'

get_prior_sensitivity <- function(encompassing_model){

  print("Computing BF...")

  BF_null <- get_BF(hypothesis = "null",  encompassing_model = encompassing_model)
  BF_monotropy <- get_BF(hypothesis = "monotropy",  encompassing_model = encompassing_model)
  BF_hierarchy <- get_BF(hypothesis = "hierarchy",  encompassing_model = encompassing_model)
  BF_independence <- get_BF(hypothesis = "independence",  encompassing_model = encompassing_model)
  BF_integration <- get_BF(hypothesis = "integration",  encompassing_model = encompassing_model)

  print("Computing summary BF...")

  res <- get_BF_weights(BF_null, BF_monotropy, BF_hierarchy,
                        BF_independence, BF_integration)
  return(res)

}

#----    get_summary_sensitivity    ----

#' Get Summary Sensitivity
#'
#' @param ... dataframes of prior sensitivity to summarize
#' @param reference dataframes with the default results
#'
#' @return a data frame with all the prior sensitivity information
#'
#' @examples
#' drake::loadd(prior_sensitivity_int_.5, prior_sensitivity_int_01,
#'              prior_sensitivity_int_05, prior_sensitivity_int_10,
#'              BF_weights_int)
#' get_summary_sensitivity(reference = BF_weights_int,
#'              prior_sensitivity_int_.5, prior_sensitivity_int_01,
#'              prior_sensitivity_int_05, prior_sensitivity_int_10)
#'

get_summary_sensitivity <- function(reference, ...){
  names_condition <- as.list(match.call(), )[-1] %>%
    as.character(.) %>%
    gsub("(^prior_sensitivity_|^BF_weights_)", "", .)

  names_condition[1] <- paste0(names_condition[1], "_03")

  list_sensitivity <- list(reference, ...)

  res <- map2(list_sensitivity, names_condition, function(x, y){
    cbind(case = y, x)
  }) %>%
  do.call("rbind", .) %>%
    arrange(case)

  return(res)
}

#----
























