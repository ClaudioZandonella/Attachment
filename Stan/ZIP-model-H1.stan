// generated with brms 2.15.0
functions {
  /* zero-inflated poisson log-PDF of a single response
   * Args:
   *   y: the response value
   *   lambda: mean parameter of the poisson distribution
   *   zi: zero-inflation probability
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real zero_inflated_poisson_lpmf(int y, real lambda, real zi) {
    if (y == 0) {
      return log_sum_exp(bernoulli_lpmf(1 | zi),
                         bernoulli_lpmf(0 | zi) +
                         poisson_lpmf(0 | lambda));
    } else {
      return bernoulli_lpmf(0 | zi) +
             poisson_lpmf(y | lambda);
    }
  }
  /* zero-inflated poisson log-PDF of a single response
   * logit parameterization of the zero-inflation part
   * Args:
   *   y: the response value
   *   lambda: mean parameter of the poisson distribution
   *   zi: linear predictor for zero-inflation part
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real zero_inflated_poisson_logit_lpmf(int y, real lambda, real zi) {
    if (y == 0) {
      return log_sum_exp(bernoulli_logit_lpmf(1 | zi),
                         bernoulli_logit_lpmf(0 | zi) +
                         poisson_lpmf(0 | lambda));
    } else {
      return bernoulli_logit_lpmf(0 | zi) +
             poisson_lpmf(y | lambda);
    }
  }
  /* zero-inflated poisson log-PDF of a single response
   * log parameterization for the poisson part
   * Args:
   *   y: the response value
   *   eta: linear predictor for poisson distribution
   *   zi: zero-inflation probability
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real zero_inflated_poisson_log_lpmf(int y, real eta, real zi) {
    if (y == 0) {
      return log_sum_exp(bernoulli_lpmf(1 | zi),
                         bernoulli_lpmf(0 | zi) +
                         poisson_log_lpmf(0 | eta));
    } else {
      return bernoulli_lpmf(0 | zi) +
             poisson_log_lpmf(y | eta);
    }
  }
  /* zero-inflated poisson log-PDF of a single response
   * log parameterization for the poisson part
   * logit parameterization of the zero-inflation part
   * Args:
   *   y: the response value
   *   eta: linear predictor for poisson distribution
   *   zi: linear predictor for zero-inflation part
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real zero_inflated_poisson_log_logit_lpmf(int y, real eta, real zi) {
    if (y == 0) {
      return log_sum_exp(bernoulli_logit_lpmf(1 | zi),
                         bernoulli_logit_lpmf(0 | zi) +
                         poisson_log_lpmf(0 | eta));
    } else {
      return bernoulli_logit_lpmf(0 | zi) +
             poisson_log_lpmf(y | eta);
    }
  }
  // zero-inflated poisson log-CCDF and log-CDF functions
  real zero_inflated_poisson_lccdf(int y, real lambda, real zi) {
    return bernoulli_lpmf(0 | zi) + poisson_lccdf(y | lambda);
  }
  real zero_inflated_poisson_lcdf(int y, real lambda, real zi) {
    return log1m_exp(zero_inflated_poisson_lccdf(y | lambda, zi));
  }
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> K_zi;  // number of population-level effects
  matrix[N, K_zi] X_zi;  // population-level design matrix
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  int Kc_zi = K_zi - 1;
  matrix[N, Kc_zi] Xc_zi;  // centered version of X_zi without an intercept
  vector[Kc_zi] means_X_zi;  // column means of X_zi before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
  for (i in 2:K_zi) {
    means_X_zi[i - 1] = mean(X_zi[, i]);
    Xc_zi[, i - 1] = X_zi[, i] - means_X_zi[i - 1];
  }
}
parameters {
  // lambda regression parameters
  real b_gender;
  vector[3] db_mother; // order constraints mother
  // vector[3] b_father; // parameters farther
  real Intercept;  // temporary intercept for centered predictors

  // zi regression parameters
  vector[Kc_zi] b_zi;  // population-level effects
  real Intercept_zi;  // temporary intercept for centered predictors
}
transformed parameters {
  vector[Kc] b;  // population-level effects

  b[1] = b_gender;
  b[2] = db_mother[1];
  b[3] = db_mother[2];
  b[4] = b[2] + db_mother[3];
  // b[4:6] = b_father;
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Intercept + Xc * b;
    // initialize linear predictor term
    vector[N] zi = Intercept_zi + Xc_zi * b_zi;
    for (n in 1:N) {
      target += zero_inflated_poisson_log_logit_lpmf(Y[n] | mu[n], zi[n]);
    }
  }
  // priors including constants
  target += normal_lpdf(db_mother | 0, 5);
  // target += normal_lpdf(b_father | 0, 5);
  target += normal_lpdf(b_zi | 0, 5);
  target += student_t_lpdf(Intercept | 3, 0.7, 2.5);
  target += logistic_lpdf(Intercept_zi | 0, 1);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  // actual population-level intercept
  real b_zi_Intercept = Intercept_zi - dot_product(means_X_zi, b_zi);
}
