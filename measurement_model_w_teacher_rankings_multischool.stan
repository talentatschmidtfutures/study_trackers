data {
  int S; // number of students
  int A; // attributes
  int N; // number of nominations
  int N2; // number of teacher scores
  int C; // number of classes
  int<lower = 1, upper = S> student[N]; // student index
  int<lower = 1, upper = S> student_2[N2]; // student index for teacher scores
  int<lower = 1, upper = A> attribute[N]; // attribute index
  int<lower = 1, upper = A> attribute_2[N2]; // attribute index for teacher scores
  int<lower = 1, upper = C> group_2[N2];
  int<lower = 1, upper = C> group_3[S]; // which group does each student belong to?
  matrix[N, S] comparisons;
  int teacher_score[N2];
  int n_cut;
}
parameters {
  matrix[S, A] z;
  cholesky_factor_corr[A] L_Omega[C];
  vector<lower = 0>[A-1] tau_raw[C];
  vector<lower = 0>[C] teacher_sigma;
  real<lower = 0> student_sigma;
  matrix[S, A] student_noise;
  matrix[S, A] teacher_noise[C];
  ordered[n_cut] cut_points[C];
}
transformed parameters {
  vector[A] tau[C]; 
  matrix[S, A] theta; 
  matrix[S, A] student_theta;
  matrix[S, A] teacher_theta[C];
  for(c in 1:C) {
    tau[c] = append_row(rep_vector(1.0, 1), tau_raw[c]);
  }
  for(s in 1:S) {
    theta[s] = z[s]*diag_pre_multiply(tau[group_3[s]], L_Omega[group_3[s]]);
  }
  
  student_theta = theta + student_sigma * student_noise;
  
  for(c in 1:C) {
    teacher_theta[c] = theta + teacher_sigma[c] * teacher_noise[c];
  }
}
model {
  // priors
  to_vector(z) ~ std_normal();
  to_vector(student_noise) ~ std_normal();
  student_sigma ~ normal(0, .3);
  for(c in 1:C) {
    to_vector(teacher_noise[c]) ~ std_normal();
    cut_points[c] ~ normal(0, 2);
    tau_raw[c] ~ normal(0, 1);
    L_Omega[c] ~ lkj_corr_cholesky(4);
  }
  teacher_sigma ~ student_t(4, 0, .2);
  // peer nominations bit
  
  for(i in 1:N) {
    row_vector[S] comps = comparisons[i, :];
    vector[S] exp_scores = exp(student_theta[:,attribute[i]]);
    real denom = comps*exp_scores;
    target += log(exp(student_theta[student[i],attribute[i]])/denom);
  }
  
  for(i in 1:N2) {
    teacher_score[i] ~ ordered_logistic(teacher_theta[group_2[i]][student_2[i], attribute_2[i]], cut_points[group_2[i]]);
  }
}
generated quantities {
  matrix[A, A] Sigma[C]; 
  vector[A] eigenvalues[C]; 
  matrix[A, A] eigenvectors[C]; 
  for(c in 1:C) {
    Sigma[c] = quad_form_diag(L_Omega[c]*L_Omega[c]', tau[c]);
    eigenvalues[c] = eigenvalues_sym(Sigma[c]);
    eigenvectors[c] = eigenvectors_sym(Sigma[c]);
  }
}