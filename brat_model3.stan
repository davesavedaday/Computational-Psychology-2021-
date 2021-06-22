data {
  int<lower=1> N;            // Number of subjects
  int<lower=1> T;            // Maximum number of trials
  int<lower=0> Tsubj[N];     // Number of trials for each subject
  int<lower=0> I;            // Initial size of the balloon
  int<lower=0> pumps[N, T];  // Number of pump
  int<lower=0,upper=1> explosion[N, T]; 
}
parameters {
  vector[4] mu_pr;
  vector<lower=0>[4] sigma;
  
  vector[N] phi_pr;
  vector[N] eta_pr;
  vector[N] gam_pr;
  vector[N] tau_pr;
}
transformed parameters {
  vector<lower=0, upper = 1> [N] phi;
  vector<lower=0>[N] eta;
  vector<lower=0, upper = 2>[N] gam;
  vector<lower=0>[N] tau;
  
  for(i in 1:N) {
    phi[i] = Phi_approx(mu_pr[1] + sigma[1] * phi_pr[i]);
    eta[i] = exp(mu_pr[2] + sigma[2] * eta_pr[i]);
    gam[i] = Phi_approx(mu_pr[3] + sigma[3] * gam_pr[i]) * 2;
    tau[i] = exp(mu_pr[4] + sigma[4] * tau_pr[i]);
  }
}
model {
  mu_pr  ~ normal(0, 1);
  sigma ~ normal(0, 0.2);

  phi_pr ~ normal(0, 1);
  eta_pr ~ normal(0, 1);
  gam_pr ~ normal(0, 1);
  tau_pr ~ normal(0, 1);
  
  for(j in 1:N) {
    int n_succ = 0;
    int n_pump = 0;
    
    for (k in 1:Tsubj[j]) {
      real p_burst;
      vector[I] curUtil;
      
      p_burst = 1 - ((phi[j] + eta[j] * n_succ) / (1 + eta[j] * n_pump));
      for( l in 1:I) {
        curUtil[l] = -(1-p_burst)^(I-l)*(l)^gam[j]- (1-(1-p_burst)^(I-l))*I^gam[j];
      }
      pumps[j,k]  ~ categorical_logit(curUtil*tau[j]);
      
      if(explosion[j,k] == 0) {
        n_succ += pumps[j,k];
      }
      n_pump += pumps[j,k];
    }
  }
}
generated quantities {
  real<lower=0, upper=1> mu_phi = Phi_approx(mu_pr[1]);
  real<lower=0> mu_eta = exp(mu_pr[2]);
  real<lower=0, upper=2> mu_gam = Phi_approx(mu_pr[3]) * 2;
  real<lower=0> mu_tau = exp(mu_pr[4]);
  
  // Log-likelihood for model fit
  real log_lik[N];

  // For posterior predictive check
  real y_pred[N, T];
  
  // Set all posterior predictions to 0 (avoids NULL values)
  for (j in 1:N)
    for (k in 1:T)
        y_pred[j, k] = 0;

  // Local section to save time and space
    for (j in 1:N) {
      int n_succ = 0;
      int n_pump = 0;

      log_lik[j] = 0;

      for (k in 1:Tsubj[j]) {
        real p_burst;  // Belief on a balloon to be burst
        vector[I] curUtil;

        p_burst = 1 - ((phi[j] + eta[j] * n_succ) / (1 + eta[j] * n_pump));
        for( l in 1:I) {
        curUtil[l] = -(1-p_burst)^(I-l)*(l)^gam[j]- (1-(1-p_burst)^(I-l))*I^gam[j];
      }
      log_lik[j] += categorical_logit_lpmf( pumps[j,k] | curUtil*tau[j] );
      y_pred[j,k] = categorical_rng(softmax(curUtil * tau[j]));
      
      if(explosion[j,k] == 0) {
        n_succ += pumps[j,k];
      }
      n_pump += pumps[j,k];
    }
  }
}
