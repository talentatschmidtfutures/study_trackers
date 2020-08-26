data {
  int N; // the number of observations
  int R; // the number of reviewers
  int J; // the number of reviewees
  int C; // the number of concepts being measured
  
  int y[N]; // a vector of reviewer-reviewee-item grades
  int<lower = 1, upper = R> reviewer[N];
  int<lower = 1, upper = J> reviewee[N];
  int<lower = 1, upper = C> concept[N];
}
parameters {
  
  matrix[J, C] latent_scores;
  
  ordered[max(y)-1] cut_points_2[R];
}
model {
  // likelihood
  for(n in 1:N) {
    y[n] ~ ordered_logistic(latent_scores[reviewee[n],concept[n]], cut_points_2[concept[n]]);
  }
  
  // priors for the rest
  to_vector(latent_scores) ~ normal(0,1);
  
  // priors for the cut-points
  for(r in 1:R) {
    cut_points_2[r] ~ normal(0, 2);
  }
}