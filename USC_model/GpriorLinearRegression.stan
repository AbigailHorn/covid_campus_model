
  data {
    int N; // the number of individuals
    int P; // the number of exposures
    int Q; // the number of covariates
    real Y[N]; // the outcome
    matrix[N,P] X; // exposure matrix
    matrix[N,Q] W; // the covariate matrix
    vector[P] mu; // prior means
    matrix[P,P] XtXinv; // information matrix
  }
  parameters {
    vector[P] beta;
    vector[Q] delta;
    real alpha;
    real invsigma2;
    real invG;
  }
  transformed parameters {
    real sigma;
    vector[N] eta;
    real b0;
    real G;
    cov_matrix[P] Sigma;
    sigma = 1/sqrt(invsigma2);
    eta = X*beta + W*delta + alpha;
    b0 = 0.5*N;
    G = 1/invG;
    for(j in 1:P) {
      for(k in 1:P) {
        Sigma[j,k] = g*sigma^2*XtXinv[j,k];
      }
    }
  }
  model {
    Y ~ normal(eta, sigma);
    beta ~ multi_normal(mu, Sigma);
    invsigma2 ~ gamma(0.5, 2);
    invG ~ gamma(0,5, b0)
  }
