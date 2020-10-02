data {
  int N; // the number of observations
  int I; // the number of items
  int R; // the number of reviewers
  int J; // the number of reviewees
  int C; // the number of concepts being measured
  
  int y[N]; // a vector of reviewer-reviewee-item grades
  int<lower = 1, upper = R> reviewer[N];
  int<lower = 1, upper = J> reviewee[N];
  int<lower = 1, upper = C> concept[N];
  int<lower = 1, upper = I> item[N];
}
parameters {
  matrix[J, C] Z;
  vector[C-1] tau_raw;
  cholesky_factor_corr[C] L_Omega; 
  ordered[max(y)-1] cut_points_2[R];
}
transformed parameters {
  vector[C] tau = append_row(rep_vector(1.0, 1), tau_raw);
  matrix[J, C] latent_scores = Z*diag_pre_multiply(tau, L_Omega);
}
model {
  // likelihood
  for(n in 1:N) {
    y[n] ~ ordered_logistic(latent_scores[reviewee[n],concept[n]], cut_points_2[reviewer[n]]);
  }
  
  // priors for the rest
  to_vector(Z) ~ std_normal();
  tau_raw ~ normal(1, 1);
  L_Omega ~ lkj_corr_cholesky(4);
  // priors for the cut-points
  for(r in 1:R) {
    cut_points_2[r] ~ normal(0, 2);
  }
}