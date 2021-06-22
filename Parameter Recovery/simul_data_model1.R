# rm(list=ls())

rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
  qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
}
softmax <- function(par){
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk))) 
  }
  val <- exp(par - Lk)
  return(val)
}
set.seed(1000)
num_subjs <- 10
num_trials <- 30
I <- 128

simul_pars = data.frame(phi_pos = rtnorm(num_subjs, 0.50, 0.10, a = 0, b = 1),
                        eta_pos = rtnorm(num_subjs, 1.00, 0.50, a = 0),
                        gam_pos = rtnorm(num_subjs, 0.10, 0.10, a = 0, b = 2),
                        tau_pos = rtnorm(num_subjs, 0.5, 0.10, a = 0),
                        subjID = 1:num_subjs)

all_data <- NULL

for( i in 1:num_subjs) {
  phi_pos <- simul_pars$phi_pos[i]
  eta_pos <- simul_pars$eta_pos[i]
  gam_pos <- simul_pars$gam_pos[i]
  tau_pos <- simul_pars$tau_pos[i]
  
  break_point = rtnorm(1, 64, 30, a = 0, b = I)
  n_succ = 0
  n_total = 0
  p = phi_pos
  
  tmp_data = data.frame(subjID = NULL, trial = NULL, pumped=NULL, explosion=NULL)
  
  curUtil <- rep(0, I)
  for( t in 1:num_trials) {
    p = 1- (phi_pos + eta_pos * (n_total - n_succ )) / (1+ eta_pos * n_total)
    for(l in 1:I) {
      curUtil[l] =(1 - p)^(I-l) * (I - l)^gam_pos
    }
    deflation = sample(1:I, size = 1, replace = TRUE, prob = softmax(curUtil * tau_pos))
    break_val = rnorm(1, mean = 64, sd = 20)
    if(I - deflation > break_val) {
      explosion = 1
    }
    else {
      explosion = 0
      n_succ = n_succ + deflation
    }
    n_total = n_total + deflation
    
    tmp_data[t, "subjID"] = i
    tmp_data[t, "trial"] = t
    tmp_data[t, "pumped"] = deflation
    tmp_data[t, "explosion"] = explosion
  }
  all_data = rbind(all_data, tmp_data)
}
write.table(all_data, file = "simul_data_model1.txt", row.names = FALSE, col.names = TRUE, sep = "\t")
