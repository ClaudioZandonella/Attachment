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
#' @param y character indicanting the dependent variable ("internalizin_sum" or
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
                   prior = my_prior, chains = 6, iter = 1e4, cores = 6)

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
#' drake::loadd(encompassing_model)
#' get_par_names(encompassing_model)
#'

get_par_names<- function(encompassing_model){
  # Keep only fixed effect "b_*" excluding gender, Intercepts and "zi" related parameters
  par_names <- brms::parnames(encompassing_model) %>%
    .[grep("^b_(?!.*(Intercept|zi|gender))", ., perl = TRUE)] %>%
    gsub("^b_", replacement = "", .) %>%  # remove initial "b_"
    gsub("mother", replacement = "M_", .) %>%
    gsub("father", replacement = "F_", .) %>%
    gsub(":", replacement = "_", .)

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
#' drake::loadd(encompassing_model)
#' get_prior_sd(encompassing_model)
#'

get_prior_sd <- function(encompassing_model){
  # get prior sd from prior summary
  sd_prior <- brms::prior_summary(encompassing_model) %>%
    .[1, "prior"] %>%
    gsub("^normal\\(0,\\s*([0-9]+)\\s*\\)", "\\1", .) %>%
    as.numeric()

  return(sd_prior)
}
#----    get_model_matrix    ----

#' Get Model Matrix
#'
#' Get the model matrix considering mother and father attachmnt in interaction
#'
#' @return a matrix
#'
#' @examples
#' get_model_matrix()
#'

get_model_matrix <- function(){

  levels <- c("Sec", "Anx", "Av", "Fear")

  new_data <- expand_grid(mother = factor(levels, levels = levels),
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
#'   "hierarchical", "independent", "iteraction")
#' @param encompassing_model a `brms` fit object of the encompassing model
#'
#' @return a list with the following matrixes:
#'   - `eq_matrix` - matrix with equality constraints
#'   - `ineq_matrix` - matrix with inequality constraints
#'   - `hyp` - the resulting matrix
#'
#' @examples
#' drake::loadd(encompassing_model)
#' get_hypothesis_matrix(hypothesis = "null", encompassing_model)
#'

get_hypothesis_matrix <- function(hypothesis = c("null", "monotropy", "hierarchical",
                                                 "independent", "interaction"),
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
    ineq_matrix <- rbind(mm["M_Anx_F_Sec", ],             # M_Anxious > 0
                         c(0, -1, 1, rep(0, n_pars - 3))) # M_Fearful - M_Avoidant > 0

  } else if(hypothesis == "hierarchical"){
    #----    Hierarchical Hypothesis    ----

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

  } else if(hypothesis == "independent"){
    #----    Independent Hypothesis    ----

    # equality constraints
    eq_matrix <- rbind(mm["M_Anx_F_Sec", ] - mm["M_Av_F_Sec", ],  # M_Anxious - M_Avoidant = 0
                       mm["M_Sec_F_Av", ] - mm["M_Sec_F_Fear", ], # F_Avoidant - F_Fearful= 0
                       diag(n_pars)[7:n_pars, ])                  # All interaction terms = 0

    # inequality constraints
    ineq_matrix <- rbind(mm["M_Anx_F_Sec", ],                       # M_Anxious > 0
                         mm["M_Fear_F_Sec", ] - mm["M_Av_F_Sec", ], # M_Fearful - M_Avoidant > 0
                         mm["M_Sec_F_Anx", ],                       # F_Anxious > 0
                         mm["M_Sec_F_Av", ] - mm["M_Sec_F_Anx", ])  # F_Avoidant - F_Anxious> 0

  } else {
    #----    Interaction Hypothesis    ----

    # equality constraints
    eq_matrix <- NULL

    # inequality constraints
    ineq_matrix <- rbind(mm["M_Anx_F_Sec", ],    # M_Anxious > 0
                         mm["M_Av_F_Sec", ],     # M_Avoidance > 0
                         mm["M_Sec_F_Anx", ],    # F_Anxious > 0
                         mm["M_Sec_F_Av", ],     # F_Avoidance > 0

                         mm["M_Fear_F_Sec", ] - mm["M_Anx_F_Sec", ],  # M_Fearful - M_Anxious > 0
                         mm["M_Fear_F_Sec", ] - mm["M_Av_F_Sec", ],   # M_Fearful - M_Avoidance > 0
                         mm["M_Fear_F_Sec", ] - mm["M_Sec_F_Anx", ],  # M_Fearful - F_Anxious > 0
                         mm["M_Fear_F_Sec", ] - mm["M_Sec_F_Av", ],   # M_Fearful - F_Avoidance > 0

                         mm["M_Sec_F_Fear", ] - mm["M_Anx_F_Sec", ],  # F_Fearful - M_Anxious > 0
                         mm["M_Sec_F_Fear", ] - mm["M_Av_F_Sec", ],   # F_Fearful - M_Avoidance > 0
                         mm["M_Sec_F_Fear", ] - mm["M_Sec_F_Anx", ],  # F_Fearful - F_Anxious > 0
                         mm["M_Sec_F_Fear", ] - mm["M_Sec_F_Av", ],   # F_Fearful - F_Avoidance > 0

                         mm["M_Anx_F_Anx", ] - mm["M_Fear_F_Sec", ],  # M_anx + F_anx + M_anx_F_anx - M_f > 0
                         mm["M_Anx_F_Anx", ] - mm["M_Sec_F_Fear", ],  # M_anx + F_anx + M_anx_F_anx - F_f > 0
                         mm["M_Av_F_Anx", ] - mm["M_Fear_F_Sec", ],   # M_av + F_anx + M_av_F_anx - M_f > 0
                         mm["M_Av_F_Anx", ] - mm["M_Sec_F_Fear", ],   # M_av + F_anx + M_av_F_anx - F_f > 0
                         mm["M_Anx_F_Av", ] - mm["M_Fear_F_Sec", ],   # M_anx + F_av + M_anx_F_av - M_f > 0
                         mm["M_Anx_F_Av", ] - mm["M_Sec_F_Fear", ],   # M_anx + F_av + M_anx_F_av - F_f > 0
                         mm["M_Av_F_Av", ] - mm["M_Fear_F_Sec", ],    # M_av + F_av + M_av_F_av - M_f > 0
                         mm["M_Av_F_Av", ] - mm["M_Sec_F_Fear", ],    # M_av + F_av + M_av_F_av - F_f > 0

                         mm["M_Fear_F_Anx", ] - mm["M_Anx_F_Anx", ],  # M_f + M_f_F_anx - M_anx - M_anx_F_anx> 0
                         mm["M_Fear_F_Anx", ] - mm["M_Av_F_Anx", ],   # M_f + M_f_F_anx - M_av - M_av_F_anx> 0
                         mm["M_Fear_F_Anx", ] - mm["M_Anx_F_Av", ],   # M_f + F_anx + M_f_F_anx - M_anx - F_av - M_anx_F_av> 0
                         mm["M_Fear_F_Anx", ] - mm["M_Av_F_Av", ],    # M_f + F_anx + M_f_F_anx - M_av - F_av - M_av_F_av> 0

                         mm["M_Fear_F_Av", ] - mm["M_Anx_F_Anx", ],   # M_f + F_av + M_f_F_av - M_anx - F_anx - M_anx_F_anx> 0
                         mm["M_Fear_F_Av", ] - mm["M_Av_F_Anx", ],    # M_f + F_av + M_f_F_av - M_av - F_anx - M_av_F_anx> 0
                         mm["M_Fear_F_Av", ] - mm["M_Anx_F_Av", ],    # M_f + M_f_F_av - M_anx - M_anx_F_av> 0
                         mm["M_Fear_F_Av", ] - mm["M_Av_F_Av", ],     # M_f + M_f_F_av - M_av - M_av_F_av> 0

                         mm["M_Anx_F_Fear", ] - mm["M_Anx_F_Anx", ],  # F_f + M_anx_F_f - F_anx - M_anx_F_anx> 0
                         mm["M_Anx_F_Fear", ] - mm["M_Av_F_Anx", ],   # M_anx + F_f + M_anx_F_f - M_av - F_anx - M_av_F_anx> 0
                         mm["M_Anx_F_Fear", ] - mm["M_Anx_F_Av", ],   # F_f + M_anx_F_f - F_av - M_anx_F_av> 0
                         mm["M_Anx_F_Fear", ] - mm["M_Av_F_Av", ],    # M_anx + F_f + M_anx_F_f - M_av -  F_av - M_av_F_av> 0

                         mm["M_Av_F_Fear", ] - mm["M_Anx_F_Anx", ],   # M_av + F_f + M_av_F_f - M_anx - F_anx - M_anx_F_anx> 0
                         mm["M_Av_F_Fear", ] - mm["M_Av_F_Anx", ],    # F_f + M_av_F_f - F_anx - M_av_F_anx> 0
                         mm["M_Av_F_Fear", ] - mm["M_Anx_F_Av", ],    # M_av + F_f + M_av_F_f - M_anx - F_av - M_anx_F_av> 0
                         mm["M_Av_F_Fear", ] - mm["M_Av_F_Av", ],     # F_f + M_av_F_f -  F_av - M_av_F_av> 0

                         mm["M_Fear_F_Fear", ] - mm["M_Fear_F_Anx", ],  # F_f + M_f_F_f - F_anx - M_f_F_anx > 0
                         mm["M_Fear_F_Fear", ] - mm["M_Fear_F_Av", ],  # F_f + M_f_F_f - F_av - M_f_F_av > 0
                         mm["M_Fear_F_Fear", ] - mm["M_Anx_F_Fear", ],  # M_f + M_f_F_f - M_anx - M_anx_F_f > 0
                         mm["M_Fear_F_Fear", ] - mm["M_Av_F_Fear", ])  # M_f + M_f_F_f - M_av - M_av_F_f > 0


    # independent rows
    # 1  2  3  4  5  9 13 15 17 19 21 25 29 33 37

    # Matrix::rankMatrix(hyp[c(1:5, 9),])
    }

  if(hypothesis == "null"){
    colnames(eq_matrix) <- par_names
  } else if (hypothesis == "interaction"){
    colnames(ineq_matrix) <- par_names
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
#' @param n_eqinteger indicating thee number of equality constraints
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
#' @param n_eqinteger indicating thee number of equality constraints
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

#----    get_BF    ----

#' Title
#'
#' @param hypothesis
#' @param encompassing_model
#'
#' @return
#' @export
#'
#' @examples
#' drake::loadd(encompassing_model)
#' hypothesis <-  "interaction"
#'

get_BF <- function(hypothesis = c("null", "monotropy", "hierarchical",
                                  "independent", "interaction"),
                   encompassing_model){
  hypothesis <- match.arg(hypothesis)

  # Hypothesis info
  hyp_matrix <- get_hypothesis_matrix(hypothesis = hypothesis,
                                      encompassing_model = encompassing_model)
  n_eq <- nrow(hyp_matrix$eq)
  n_ineq <- nrow(hyp_matrix$ineq)

  # get posterior info
  posterior <- brms::fixef(encompassing_model, summary = FALSE) %>%
    .[ , !colnames(.) %in% c("Intercept", "zi_Intercept", "zi_genderM", "genderM")]
  posterior_mean <- colMeans(posterior)
  posterior_cov <- cov(posterior)

  # get prior info
  prior_sd <- get_prior_sd(encompassing_model)
  prior_mean <- rep(0, ncol(posterior))
  prior_cov <- diag(ncol(posterior)) * prior_sd^2


  if(hypothesis == "null"){
    #----    Null Hypotesis    ----

    # prior density equality
    prior_den_eq <- compute_density(prior_mean, prior_cov, n_eq)

    # posterior density equality
    post_den_eq <- compute_density(posterior_mean, posterior_cov, n_eq)

    # prior conditional probability
    prior_cond_prob <- 1

    # posterior conditional probability
    post_cond_prob <- 1


    } else if(hypothesis == "monotropy"){
      #----    Monotropy Hypothesis    ----

      # Linear transformation of the parameters
      posterior_mean <- hyp_matrix$hyp %*% posterior_mean
      posterior_cov <- hyp_matrix$hyp %*% posterior_cov %*% t(hyp_matrix$hyp)

      prior_mean <- hyp_matrix$hyp %*% prior_mean
      prior_cov <- hyp_matrix$hyp %*% prior_cov %*% t(hyp_matrix$hyp)

      # prior density equality
      prior_den_eq <- compute_density(prior_mean, prior_cov, n_eq)

      # posterior density equality
      post_den_eq <- compute_density(posterior_mean, posterior_cov, n_eq)

      # prior conditional probability
      prior_cond_prob <- compute_cond_prob(prior_mean, prior_cov,
                                           n_eq = n_eq, n_ineq = n_ineq)

      # posterior conditional probability
      post_cond_prob <- compute_cond_prob(posterior_mean, posterior_cov,
                                          n_eq = n_eq, n_ineq = n_ineq)

    } else if(hypothesis == "hierarchical"){
      #----    Hierarchical Hypothesis    ----

      # In the Hierarchical hypothesis the last three inequality constraints are
      # not linearly independent:
      # beta_{16} = beta_{12} - beta_{14}
      # beta_{17} = - beta_{1} + beta_{2} + beta_{12} - beta_{14} = - beta_{1} + beta_{2} + beta_{16}
      # beta_{18} = - beta_{1} + beta_{2} + beta_{12} + beta_{13} - beta_{14} - beta_{15} =

      # Linear transformation of the parameters
      posterior_mean <- hyp_matrix$hyp[1:15, ] %*% posterior_mean
      posterior_cov <- hyp_matrix$hyp[1:15, ] %*% posterior_cov %*% t(hyp_matrix$hyp[1:15, ])

      prior_mean <- hyp_matrix$hyp[1:15, ] %*% prior_mean
      prior_cov <- hyp_matrix$hyp[1:15, ] %*% prior_cov %*% t(hyp_matrix$hyp[1:15, ])

      # prior density equality
      prior_den_eq <- compute_density(prior_mean, prior_cov, n_eq)

      # posterior density equality
      post_den_eq <- compute_density(posterior_mean, posterior_cov, n_eq)

      # prior conditional probability
      obs <- condMVNorm::rcmvnorm(
        1e7, mean = prior_mean, sigma = prior_cov,
        dependent.ind = (n_eq + 1):(n_eq + n_ineq -3),
        given.ind = 1:n_eq, X.given = rep(0,n_eq))

      # given beta_1 = beta_2 = 0 we have:
      #   beta_16 = beta_12 - beta_14
      #   beta_17 = beta_16
      #   beta_18 = beta_12 + beta_13 - beta_14 - beta_15

      test <- cbind(obs, # beta_12, beta_13, beta_14, beta_15
                    beta_16 = obs[,1] - obs[,3],  # beta_16
                    beta_18 = obs[,1] + obs[,2] - obs[,3] - obs[,4]) # beta_18

      prior_cond_prob <- mean(rowSums(test >= 0) == ncol(test))

      # posterior conditional probability
      obs <- condMVNorm::rcmvnorm(
        1e7, mean = posterior_mean, sigma = posterior_cov,
        dependent.ind = (n_eq + 1):(n_eq + n_ineq -3),
        given.ind = 1:n_eq, X.given = rep(0,n_eq))

      test <- cbind(obs, # beta_12, beta_13, beta_14, beta_15
                    beta_16 = obs[,1] - obs[,3],  # beta_16
                    beta_18 = obs[,1] + obs[,2] - obs[,3] - obs[,4]) # beta_18

      post_cond_prob <- mean(rowSums(test >= 0) == ncol(test))

    } else if(hypothesis == "independent"){
      #----    Independent Hypothesis    ----

      # Linear transformation of the parameters
      posterior_mean <- hyp_matrix$hyp %*% posterior_mean
      posterior_cov <- hyp_matrix$hyp %*% posterior_cov %*% t(hyp_matrix$hyp)

      prior_mean <- hyp_matrix$hyp %*% prior_mean
      prior_cov <- hyp_matrix$hyp %*% prior_cov %*% t(hyp_matrix$hyp)

      # prior density equality
      prior_den_eq <- compute_density(prior_mean, prior_cov, n_eq)

      # posterior density equality
      post_den_eq <- compute_density(posterior_mean, posterior_cov, n_eq)

      # prior conditional probability
      prior_cond_prob <- compute_cond_prob(prior_mean, prior_cov,
                                           n_eq = n_eq, n_ineq = n_ineq)

      # posterior conditional probability
      post_cond_prob <- compute_cond_prob(posterior_mean, posterior_cov,
                                          n_eq = n_eq, n_ineq = n_ineq)

    } else if(hypothesis == "interaction"){
      #----    Interaction Hypothesis    ----

      # In the Hierarchical hypothesis only the constraints at line: 1, 2, 3, 4,
      # 5, 9, 13, 15, 17, 19, 21, 25, 29, 33, 37 are linearly independent. The
      # other constaints can be obtained as composition of the other lineearly
      # independent betas.

      independent_rows <- c(1, 2, 3, 4, 5, 9, 13, 15, 17, 19, 21, 25, 29, 33, 37)
      composition_betas <- find_composition_betas(hyp_matrix$hyp,
                                                  independent_rows = independent_rows)

      # Linear transformation of the parameters
      posterior_mean <- hyp_matrix$hyp[independent_rows, ] %*% posterior_mean
      posterior_cov <- hyp_matrix$hyp[independent_rows, ] %*% posterior_cov %*% t(hyp_matrix$hyp[independent_rows, ])

      prior_mean <- hyp_matrix$hyp[independent_rows, ] %*% prior_mean
      prior_cov <- hyp_matrix$hyp[independent_rows, ] %*% prior_cov %*% t(hyp_matrix$hyp[independent_rows, ])

      # prior density equality
      prior_den_eq <- 1

      # posterior density equality
      post_den_eq <- 1

      # prior conditional probability
      obs <- MASS::mvrnorm(1e7, mu = prior_mean, Sigma = prior_cov)

      test <- composition_betas %*% t(obs)

      prior_cond_prob <- mean(colSums(test >= 0) == nrow(test))

      # posterior conditional probability
      obs <- MASS::mvrnorm(1e7, mu = posterior_mean, Sigma = posterior_cov)

      test <- composition_betas %*% t(obs)

      post_cond_prob <- mean(colSums(test >= 0) == nrow(test))
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
#' drake::loadd(encompassing_model)
#' independent_rows <- c(1, 2, 3, 4, 5, 9, 13, 15, 17, 19, 21, 25, 29, 33, 37)
#' hyp <- get_hypothesis_matrix(hypothesis = "interaction", encompassing_model)$hyp
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

    row_index <- which(hyp[ ,i]==1L)[1] # which row is used as the base

    if(hyp[row_index, i] !=1L) stop("issue identifing row to use as base")

    #bases[[i]][paste0("beta_", row_index)] <- 1

    # consider -1
    minus <- sapply(which(hyp[row_index,] == -1), function(x){ bases[[x]]}) %>%
      rowSums()
    # consider +1
    plus <- sapply(which(hyp[row_index,] == 1), function(x){bases[[x]]}) %>%
      rowSums()

    betas <- - plus + minus
    betas[paste0("beta_", row_index)] <- betas[paste0("beta_", row_index)] + 1

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
#' drake::loadd(encompassing_model)
#' independent_rows <- c(1, 2, 3, 4, 5, 9, 13, 15, 17, 19, 21, 25, 29, 33, 37)
#' hyp <- get_hypothesis_matrix(hypothesis = "interaction", encompassing_model)$hyp
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
#' @param ... BF to compare
#'
#' @return a matrix
#'
#' @examples
#' drake::loadd(c(BF_null, BF_monotropy,
#'        BF_hierarchical, BF_independent))
#' get_table_BF(BF_null,
#'              BF_monotropy,
#'              BF_hierarchical,
#'              BF_independent)
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

#----
























