data {
  int N; // number of choices
  int N_possibles; // length of possibles index
  int I; // number of candidates
  int C; // concepts
  int S; // number of schools
  int school[N]; // which school group does the observation belong to?
  int concept[N]; // the concept belonging to each 
  int winner[N]; // index of the winning student
  int possibles_lower[N]; // indexes 
  int possibles_upper[N];
  int possibles[N_possibles];
  int individual_school[I];
}
parameters {
  real<lower = 0, upper = 1> rho;
  corr_matrix[C] Omega_s[S];
  corr_matrix[C] Omega;
  vector<lower =0>[C] tau[S];
  matrix[I, C] theta;
}
model {
  rho ~ beta(2, 2);
  for(s in 1:S) {
    tau[s] ~ student_t(3, 0, 1);
    Omega_s[s] ~ lkj_corr(4);
  }
  Omega ~ lkj_corr(4);
  
  for(i in 1:I) {
    theta[i] ~ multi_normal(rep_vector(0.0, C), 
    quad_form_diag(rho*Omega + (1 - rho) * Omega_s[individual_school[i]], tau[individual_school[i]]));
  }
  
  for(n in 1:N) {
    target += theta[winner[n], concept[n]] - log_sum_exp(theta[possibles[possibles_lower[n]:possibles_upper[n]],concept[n]]);
  }
}
