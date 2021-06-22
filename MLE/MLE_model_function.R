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
N = 20
T = 30
I = 128

mle_model1 <-function(param, data) {
  pump <- data[,3]
  explosion <- data[,7]
  sum_minusLL = 0
  
  n_succ = 0
  n_total = 0
  p = param[1]
  curUtil = rep(0,I)
  for(t in 1:T) {
    p = 1- (param[1] + param[2] * (n_total - n_succ)) / (1+ param[2] * n_total)
    for(l in 1:I) {
      curUtil[l] = (1-p)^(I-l) * (I-l)^param[3]
    }
    prob = softmax(curUtil)[pump[t]] #Softmax function already normalized the value.
    prob = prob * 0.9998 + 0.0001
    sum_minusLL = sum_minusLL -log(prob)
  }
  sum_minusLL
}

mle_model2 <-function(param, data) {
  pump <- data[,3]
  explosion <- data[,7]
  sum_minusLL = 0
  
  n_succ = 0
  n_total = 0
  p = param[1]
  curUtil = rep(0,I)
  for(t in 1:T) {
    p = (param[1] + param[2] *  n_succ) / (1+ param[2] * n_total)
    for(l in 1:I) {
      curUtil[l] = (1-p^l) * (I-l)^param[3]
    }
    prob = softmax(curUtil)[pump[t]] #Softmax function already normalized the value.
    prob = prob * 0.9998 + 0.0001
    sum_minusLL = sum_minusLL -log(prob)
  }
  sum_minusLL
}

mle_model3 <-function(param, data) {
  pump <- data[,3]
  explosion <- data[,7]
  sum_minusLL = 0
  
  n_succ = 0
  n_total = 0
  p = param[1]
  curUtil = rep(0,I)
  for(t in 1:T) {
    p = 1- (param[1] + param[2] *  n_succ) / (1+ param[2] * n_total)
    for(l in 1:I) {
      curUtil[l] = -(1-p)^(I-l) * l^param[3] - (1-(1-p)^(I-l))* I^param[3]
    }
    prob = softmax(curUtil)[pump[t]] #Softmax function already normalized the value.
    prob = prob * 0.9998 + 0.0001
    sum_minusLL = sum_minusLL -log(prob)
  }
  sum_minusLL
}

mle_model4 <-function(param, data) {
  pump <- data[,3]
  explosion <- data[,7]
  sum_minusLL = 0
  
  n_succ = 0
  n_total = 0
  p = param[1]
  curUtil = rep(0,I)
  for(t in 1:T) {
    p = (param[1] + param[2] *  n_succ) / (1 + param[2] * n_total)
    for(l in 1:I) {
      curUtil[l] = -(1-p^l) * l^param[3] - p^l * I^param[3]
    }
    prob = softmax(curUtil)[pump[t]] #Softmax function already normalized the value.
    prob = prob * 0.9998 + 0.0001
    sum_minusLL = sum_minusLL -log(prob)
  }
  sum_minusLL
}
