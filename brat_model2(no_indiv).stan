data {
  int<lower=1> N;            // Number of subjects
  int<lower=1> T;            // Maximum number of trials
  int<lower=0> Tsubj[N];     // Number of trials for each subject
  int<lower=0> I;            // Initial size of the balloon
  int<lower=0> pumps[N, T];  // Number of pump
  int<lower=0,upper=1> explosion[N, T]; 
}
parameters {
  
  real<lower=0, upper = 1> phi;
  real<lower=0> eta;
  real<lower=0, upper = 2> gam;
  real<lower=0> tau;
}
model {
  phi ~ uniform(0, 1);
  eta ~ uniform(0, 5);
  gam ~ uniform(0, 2);
  tau ~ uniform(0, 15);
  
  for(j in 1:N) {
    int n_succ = 0;
    int n_pump = 0;
    
    for (k in 1:Tsubj[j]) {
      real p_deflate;
      vector[I] curUtil;
      
      p_deflate = (phi + eta * (n_pump-n_succ)) / (1 + eta *  n_pump);
      for( l in 1:I) {
        curUtil[l] = (1-p_deflate^l)*(I-l)^gam;
      }
      pumps[j,k]  ~ categorical_logit(curUtil*tau);
      
      if(explosion[j,k] == 0) {
        n_succ += pumps[j,k];
      }
      n_pump += pumps[j,k];
    }
  }
}
generated quantities {
  
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
        real p_deflate;  // Belief on a balloon to be burst
        vector[I] curUtil;

        p_deflate = (phi + eta * (n_pump-n_succ)) / (1 + eta *  n_pump);
        for( l in 1:I) {
        curUtil[l] = (1-p_deflate^l)*(I-l)^gam;
      }
      log_lik[j] += categorical_logit_lpmf( pumps[j,k] | curUtil * tau );
      y_pred[j,k] = categorical_rng(softmax(curUtil * tau));
      
      if(explosion[j,k] == 0) {
        n_succ += pumps[j,k];
      }
      n_pump += pumps[j,k];
    }
  }
}
