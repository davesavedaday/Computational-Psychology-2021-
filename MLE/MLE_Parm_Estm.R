rm(list=ls())
graphics.off()

source("MLE_model_function.R")

dat = read.table("export_brat_time1(n=20).txt", header = T)
allSubjs <- unique(dat$Subject)
param_init <- runif(4)
param_low <- c(0,0,0,0); param_up <- c(1,10,2,10)


mle_model1_fit <- optim(param_init, mle_model1, method="L-BFGS-B",
                    lower=param_low, upper=param_up, data=subset(dat, dat$Subject == 100))
mle_model2_fit <- optim(param_init, mle_model2, method="L-BFGS-B",
                    lower=param_low, upper=param_up, data=subset(dat, dat$Subject == 100))
mle_model3_fit <- optim(param_init, mle_model3, method="L-BFGS-B",
                    lower=param_low, upper=param_up, data=subset(dat, dat$Subject == 100))
mle_model4_fit <- optim(param_init, mle_model4, method="L-BFGS-B",
                    lower=param_low, upper=param_up, data=subset(dat, dat$Subject == 100))

param_model1 <- array(numeric(), c(N,4))
param_model2 <- array(numeric(), c(N,4))
param_model3 <- array(numeric(), c(N,4))
param_model4 <- array(numeric(), c(N,4))


mle_model1_value <- array(numeric(), c(N,1))
mle_model2_value <- array(numeric(), c(N,1))
mle_model3_value <- array(numeric(), c(N,1))
mle_model4_value <- array(numeric(), c(N,1))


flag <- 1

for(i in allSubjs) {
  mle_model1_fit <- optim(param_init, mle_model1, method="L-BFGS-B",
                      lower=param_low, upper=param_up, data=subset(dat, dat$Subject == i))
  mle_model2_fit <- optim(param_init, mle_model2, method="L-BFGS-B",
                      lower=param_low, upper=param_up, data=subset(dat, dat$Subject == i))
  mle_model3_fit <- optim(param_init, mle_model3, method="L-BFGS-B",
                      lower=param_low, upper=param_up, data=subset(dat, dat$Subject == i))
  mle_model4_fit <- optim(param_init, mle_model4, method="L-BFGS-B",
                      lower=param_low, upper=param_up, data=subset(dat, dat$Subject == i))
  for(j in 1:10) {
    param_init <- runif(4)
    temp_mle_model1 <- optim(param_init, mle_model1, method="L-BFGS-B",
                        lower=param_low, upper=param_up, data=subset(dat, dat$Subject == i))
    temp_mle_model2 <- optim(param_init, mle_model2, method="L-BFGS-B",
                        lower=param_low, upper=param_up, data=subset(dat, dat$Subject == i))
    temp_mle_model3 <- optim(param_init, mle_model3, method="L-BFGS-B",
                        lower=param_low, upper=param_up, data=subset(dat, dat$Subject == i))
    temp_mle_model4 <- optim(param_init, mle_model4, method="L-BFGS-B",
                        lower=param_low, upper=param_up, data=subset(dat, dat$Subject == i))
    if(temp_mle_model1$value < mle_model1_fit$value)
      mle_model1_fit <- temp_mle_model1
    if(temp_mle_model2$value < mle_model2_fit$value)
      mle_model2_fit <- temp_mle_model2
    if(temp_mle_model3$value < mle_model3_fit$value)
      mle_model3_fit <- temp_mle_model3
    if(temp_mle_model4$value < mle_model4_fit$value)
      mle_model4_fit <- temp_mle_model4
  }
  param_model1[flag,] <- mle_model1_fit$par
  param_model2[flag,] <- mle_model2_fit$par
  param_model3[flag,] <- mle_model3_fit$par
  param_model4[flag,] <- mle_model4_fit$par
  
  mle_model1_value[flag,] <- mle_model1_fit$value
  mle_model2_value[flag,] <- mle_model2_fit$value
  mle_model3_value[flag,] <- mle_model3_fit$value
  mle_model4_value[flag,] <- mle_model4_fit$value
  flag = flag + 1
}



# compute the AIC
AIC_model1 <- 0
AIC_model2 <- 0
AIC_model3 <- 0
AIC_model4 <- 0

for(i in 1:N) {
  AIC_model1 = AIC_model1 + 2*mle_model1_value[i] + 2*4
  AIC_model2 = AIC_model2 + 2*mle_model2_value[i] + 2*4
  AIC_model3 = AIC_model3 + 2*mle_model3_value[i] + 2*4
  AIC_model4 = AIC_model4 + 2*mle_model4_value[i] + 2*4
  
  AIC_model1 = AIC_model1 / N
  AIC_model2 = AIC_model2 / N
  AIC_model3 = AIC_model3 / N
  AIC_model4 = AIC_model4 / N
}

# compute the BIC
BIC_model1 <- 0
BIC_model2 <- 0
BIC_model3 <- 0
BIC_model4 <- 0

for(i in 1:N) {
  BIC_model1 = BIC_model1 + 2*mle_model1_value[i] + 4*log(T)
  BIC_model2 = BIC_model2 + 2*mle_model2_value[i] + 4*log(T)
  BIC_model3 = BIC_model3 + 2*mle_model3_value[i] + 4*log(T)
  BIC_model4 = BIC_model4 + 2*mle_model4_value[i] + 4*log(T)
  BIC_model1 = BIC_model1 / N
  BIC_model2 = BIC_model2 / N
  BIC_model3 = BIC_model3 / N
  BIC_model4 = BIC_model4 / N
}

# Generate summary
all_AIC = round(c(AIC_model1, AIC_model2, AIC_model3, AIC_model4), 3)
all_BIC = round(c(BIC_model1, BIC_model2, BIC_model3, BIC_model4), 3)
names = c("model1", "model2", "model3", "model4")

modelcomp_summary = data.frame(Models = names, AIC = all_AIC, BIC = all_BIC)

print(modelcomp_summary)

#Plot MLE against Bayesian Inference
fit_2 <- readRDS("fit_2.rds")
bayes_phi <- summary(fit_2, pars = "phi")$summary
bayes_eta <- summary(fit_2, pars = "eta")$summary
bayes_gam <- summary(fit_2, pars = "gam")$summary
bayes_tau <- summary(fit_2, pars = "tau")$summary
plot(bayes_phi[,1], param_model2[,1], xlab = "Bayesian Inference of phi", ylab = "MLE of phi"
     ,xlim = c(0,1), ylim = c(0,1)); abline(0,1);
plot(bayes_eta[,1], param_model2[,2], xlab = "Bayesian Inference of eta", ylab = "MLE of eta"
     ,xlim = c(0,1), ylim = c(0,1)); abline(0,1);
plot(bayes_gam[,1], param_model2[,3], xlab = "Bayesian Inference of gam", ylab = "MLE of gam"
     ,xlim = c(0,1), ylim = c(0,1)); abline(0,1);
plot(bayes_tau[,1], param_model2[,4], xlab = "Bayesian Inference of tau", ylab = "MLE of tau"); abline(0,1)

