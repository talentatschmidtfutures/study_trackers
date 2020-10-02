data {
  int N; // number of rows
  int I; // number of candidates
  int C; // number of concepts
  int S; // number of groups
  int score[N]; // score
  int student[N];
  int concept[N]; // concept
  int school[N]; // school
  int individual_school[I];
}
parameters {
  real<lower = 0, upper = 1> rho;
  corr_matrix[C] Omega_s[S];
  corr_matrix[C] Omega;
  vector<lower =0>[C] tau[S];
  matrix[I, C] theta;
  ordered[max(score)-1] cut_points[S];
}
model {
  // priors
  rho ~ beta(2, 2);
  for(s in 1:S) {
    tau[s] ~ student_t(3, 0, 1);
    Omega_s[s] ~ lkj_corr(4);
  }
  Omega ~ lkj_corr(4);
  for(s in 1:S) cut_points[s] ~ normal(0, 2);
  
  for(i in 1:I) {
    theta[i] ~ multi_normal(rep_vector(0.0, C), 
    quad_form_diag(rho*Omega + (1 - rho) * Omega_s[individual_school[i]], tau[individual_school[i]]));
  }
  
  
  for(n in 1:N) {
    score[n] ~ ordered_logistic(theta[student[n], concept[n]], cut_points[school[n]]);
  }
}