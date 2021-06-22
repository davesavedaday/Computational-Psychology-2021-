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

simul_pars = data.frame(subjID = 1:num_subjs)

all_data <- NULL

for( i in 1:num_subjs) {
  phi = 0.9965212
  eta = 2.226753e-06
  gam = 0.7808239
  tau = 0.9526595
  
  break_point = rtnorm(1, 64, 30, a = 0, b = I)
  n_succ = 0
  n_total = 0
  p = phi
  
  tmp_data = data.frame(subjID = NULL, trial = NULL, pumped=NULL, explosion=NULL)
  
  curUtil <- rep(0, I)
  for( t in 1:num_trials) {
    p = (phi + eta * (n_total - n_succ )) / (1+ eta * n_total)
    for(l in 1:I) {
      curUtil[l] =(1 - p^l) * (I - l)^gam
    }
    deflation = sample(1:I, size = 1, replace = TRUE, prob = softmax(curUtil * tau))
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
write.table(all_data, file = "simul_data_model2(no_indiv).txt", row.names = FALSE, col.names = TRUE, sep = "\t")
