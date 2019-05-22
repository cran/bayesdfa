data {
  int<lower=0> N; // number of data points
  int<lower=0> P; // number of time series of data
  int<lower=0> K; // number of trends
  int<lower=0> nZ; // number of unique z elements
  int<lower=0> row_indx[nZ];
  int<lower=0> col_indx[nZ];
  int<lower=0> nVariances;
  int<lower=0> varIndx[P];
  int<lower=0> nZero;
  int<lower=0> row_indx_z[nZero];
  int<lower=0> col_indx_z[nZero];
  int<lower=0> n_pos; // number of non-missing observations
  int<lower=0> row_indx_pos[n_pos]; // row indices of non-missing obs
  int<lower=0> col_indx_pos[n_pos]; // col indices of non-missing obs
  real y[n_pos]; // vectorized matrix of observations
  int<lower=0> n_na; // number of missing observations
  int<lower=0> row_indx_na[n_na]; // row indices of missing obs
  int<lower=0> col_indx_na[n_na]; // col indices of missing obs
  real<lower=1> nu_fixed; // df on student-t
  int estimate_nu; // Estimate degrees of freedom?
  int use_normal; // flag, for large values of nu > 100, use normal instead
  int est_cor; // whether to estimate correlation in obs error (=1) or not (=0)
  int est_phi; // whether to estimate autocorrelation in trends (=1) or not (= 0)
  int est_theta; // whether to estimate moving-average in trends (=1) or not (= 0
  int<lower=0> num_obs_covar; // number of unique observation covariates, dimension of matrix
  int<lower=0> n_obs_covar; // number of unique covariates included
  int obs_covar_index[num_obs_covar,3] ;// indexed by time, trend, covariate #, covariate value. +1 because of indexing issues
  real obs_covar_value[num_obs_covar];
  int<lower=0> num_pro_covar; // number of unique process covariates, dimension of matrix
  int<lower=0> n_pro_covar; // number of unique process covariates included
  int pro_covar_index[num_pro_covar,3] ;// indexed by time, trend, covariate #, covariate value. +1 because of indexing issues
  real pro_covar_value[num_pro_covar];
}
transformed data {
  int n_pcor; // dimension for cov matrix
  int n_loglik; // dimension for loglik calculation
  vector[K] zeros;

  for(k in 1:K) {
    zeros[k] = 0; // used in MVT / MVN below
  }

  if(est_cor == 0) {
     n_loglik = P * N;
  } else {
    n_loglik = N; // TODO: likely needs to be fixed
  }

  if(est_cor == 0) {
    n_pcor = P;
    if(nVariances < 2) {
      n_pcor = 2;
    }
  } else {
    n_pcor = P;
  }
}
parameters {
  matrix[K,N-1] devs; // random deviations of trends
  vector[K] x0; // initial state
  vector<lower=0>[K] psi; // expansion parameters
  vector[nZ] z; // estimated loadings in vec form
  vector[K] zpos; // constrained positive values
  matrix[n_obs_covar, P] b_obs; // coefficients on observation model
  matrix[n_pro_covar, K] b_pro; // coefficients on process model
  real<lower=0> sigma[nVariances];
  real<lower=2> nu[estimate_nu]; // df on student-t
  real ymiss[n_na];
  real<lower=-1,upper=1> phi[est_phi*K];
  real<lower=-1,upper=1> theta[est_theta*K];
  cholesky_factor_corr[n_pcor] Lcorr;
}
transformed parameters {
  matrix[P,N] pred; //vector[P] pred[N];
  matrix[P,K] Z;
  //vector[N] yall[P]; // combined vectors of missing and non-missing values
  matrix[P,N] yall;
  vector[P] sigma_vec;
  vector[K] phi_vec; // for AR(1) part
  vector[K] theta_vec; // for MA(1) part
  matrix[K,N] x; //vector[N] x[P]; // random walk-trends
  vector[K] indicator; // indicates whether diagonal is neg or pos
  vector[K] psi_root; // derived sqrt(expansion parameter psi)

  // phi is the ar(1) parameter, fixed or estimated
  if(est_phi == 1) {
    for(k in 1:K) {phi_vec[k] = phi[k];}
  } else {
    for(k in 1:K) {phi_vec[k] = 1;}
  }

  // theta is the ma(1) parameter, fixed or estimated
  if(est_theta == 1) {
    for(k in 1:K) {theta_vec[k] = theta[k];}
  } else {
    for(k in 1:K) {theta_vec[k] = 0;}
  }

  for(p in 1:P) {
    sigma_vec[p] = sigma[varIndx[p]]; // convert estimated sigmas to vec form
  }

  // Fill yall with non-missing values
  for(i in 1:n_pos) {
    yall[row_indx_pos[i], col_indx_pos[i]] = y[i];
  }
  // Include missing observations
  if(n_na > 0) {
    for(i in 1:n_na) {
      yall[row_indx_na[i], col_indx_na[i]] = ymiss[i];
    }
  }

  for(i in 1:nZ) {
    Z[row_indx[i],col_indx[i]] = z[i]; // convert z to from vec to matrix
  }
  // fill in zero elements in upper diagonal
  if(nZero > 2) {
    for(i in 1:(nZero-2)) {
      Z[row_indx_z[i],col_indx_z[i]] = 0;
    }
  }

  for(k in 1:K) {
    Z[k,k] = zpos[k];// add constraint for Z diagonal
  }

  // this block is for the expansion prior
  for(k in 1:K) {
    if(zpos[k] < 0) {
      indicator[k] = -1;
    } else {
      indicator[k] = 1;
    }
    psi_root[k] = sqrt(psi[k]);
    for(p in 1:P) {
      Z[p,k] = Z[p,k] * indicator[k] * (1/psi_root[k]);
    }
  }

  // initial state for each trend
  for(k in 1:K) {
    x[k,1] = x0[k];
    // trend is modeled as random walk, with optional
    // AR(1) component = phi, and optional MA(1) component
    // theta. Theta is included in the model block below.
    for(t in 2:N) {
      x[k,t] = phi_vec[k]*x[k,t-1] + devs[k,t-1];
    }
  }
  // this block also for the expansion prior, used to convert trends
  for(k in 1:K) {
    //  x[k,1:N] = x[k,1:N] * indicator[k] * psi_root[k];
    for(t in 1:N) {
      x[k,t] = x[k,t] * indicator[k] * psi_root[k];
    }
  }

  // adjust predictions if process covariates exist
  if(num_pro_covar > 0) {
    for(i in 1:num_pro_covar) {
      // indexed by time, trend, covariate #, covariate value
      x[pro_covar_index[i,2],pro_covar_index[i,1]] = x[pro_covar_index[i,2],pro_covar_index[i,1]] + b_pro[pro_covar_index[i,3], pro_covar_index[i,2]] * pro_covar_value[i];
    }
  }

  // N is sample size, P = time series, K = number trends
  // [PxN] = [PxK] * [KxN]
  pred = Z * x;

  // adjust predictions if observation covariates exist
  if(num_obs_covar > 0) {
    for(i in 1:num_obs_covar) {
      // indexed by time, trend, covariate #, covariate value
      pred[obs_covar_index[i,2],obs_covar_index[i,1]] = pred[obs_covar_index[i,2],obs_covar_index[i,1]] + b_obs[obs_covar_index[i,3], obs_covar_index[i,2]] * obs_covar_value[i];
    }
  }
}
model {
  // initial state for each trend
  x0 ~ normal(0, 1); // initial state estimate at t=1
  psi ~ gamma(2, 1); // expansion parameter for par-expanded priors

  // This is deviations - either normal or Student t, and
  // if Student-t, df parameter nu can be estimated or fixed
  for(k in 1:K) {
    if(use_normal == 0) {
      for(t in 1:1) {
        if (estimate_nu == 1) {
          devs[k,t] ~ student_t(nu[1], 0, 1); // random walk
        } else {
          devs[k,t] ~ student_t(nu_fixed, 0, 1); // random walk
        }
      }
      for(t in 2:(N-1)) {
        // if MA is not included, theta_vec = 0
        if (estimate_nu == 1) {
          devs[k,t] ~ student_t(nu[1], theta_vec[k]*devs[k,t-1], 1); // random walk
        } else {
          devs[k,t] ~ student_t(nu_fixed, theta_vec[k]*devs[k,t-1], 1); // random walk
        }
      }
    } else {
      devs[k,1] ~ normal(0, 1);
      for(t in 2:(N-1)) {
        // if MA is not included, theta_vec = 0
        devs[k,t] ~ normal(theta_vec[k]*devs[k,t-1], 1);
      }
    }

  }

  // prior for df parameter for t-distribution
  if (estimate_nu == 1) {
    nu[1] ~ gamma(2, 0.1);
  }
  // prior on AR(1) component if included
  if(est_phi == 1) {
    phi ~ uniform(0,1); // K elements
  }
  // prior on MA(1) component if included
  if(est_theta == 1) {
    theta ~ uniform(0,1); // K elements
  }

  // prior on loadings
  z ~ normal(0, 1);
  zpos ~ normal(0, 1);// diagonal

  // observation variance
  sigma ~ student_t(3, 0, 2);
  if(est_cor == 1) {
    Lcorr ~ lkj_corr_cholesky(1);
  }

  // likelihood for independent
  if(est_cor == 0) {
    for(i in 1:P){
      target += normal_lpdf(yall[i] | pred[i], sigma_vec[i]);
    }
  } else {
    // need to loop over time slices / columns - each ~ MVN
    for(i in 1:N) {
      target += multi_normal_cholesky_lpdf(col(yall,i) | col(pred,i), diag_pre_multiply(sigma_vec, Lcorr));
    }
  }
}
generated quantities {
  vector[n_loglik] log_lik;
  matrix[n_pcor, n_pcor] Omega;
  matrix[n_pcor, n_pcor] Sigma;
  int<lower=0> j;

  if(est_cor == 1) {
  Omega = multiply_lower_tri_self_transpose(Lcorr);
  Sigma = quad_form_diag(Omega, sigma_vec);
  }

  // calculate pointwise log_lik for loo package:
  if(est_cor == 0) {
    j = 0;
    for(n in 1:N) {
      for(p in 1:P) {
        j = j + 1;
        log_lik[j] = normal_lpdf(yall[p,n] | pred[p,n], sigma_vec[p]);
      }
    }
  } else {
    // TODO: this needs to be fixed:
    for(i in 1:N) {
      log_lik[i] = multi_normal_cholesky_lpdf(col(yall,i) | col(pred,i), diag_pre_multiply(sigma_vec, Lcorr));
    }
  }
}
