# rm(list=ls())
# graphics.off()

library(rstan)
library(bayesplot)
library(loo)
options(mc.cores = parallel::detectCores())

dat = read.table("export_brat_time1(n=20).txt", header = T)
dat_shortened = read.table("export_brat_time1(n=2).txt", header = T)

allSubjs <- unique(dat_shortened$Subject)
N <- length(allSubjs)
T <- table(dat_shortened$Subject)[1]
numPars <- 5
Tsubj <- numeric()
for(i in 1:N) {
  Tsubj = c(Tsubj, table(dat_shortened$Subject)[i])
}

dataList <- list(
  N = N,
  T = T,
  I = 128,
  Tsubj = Tsubj,
  pumps = matrix(nrow = N, dat_shortened$pumped),
  explosion = matrix(nrow = N, dat_shortened$explosion)
)

fit_1 = stan("brat_model1.stan", data = dataList,
             pars = c("mu_pr", "phi_pr", "eta_pr", "gam_pr", "tau_pr"),
             include = FALSE, iter = 3000, warmup = 1000, chains=4, cores=4,
              seed = "1000")

fit_2 = stan("brat_model2.stan", data = dataList,
             pars = c("mu_pr", "phi_pr", "eta_pr", "gam_pr", "tau_pr"),
             include = FALSE, iter = 3000, warmup = 1000, chains=4, cores=4,
             seed = "1000")

fit_3 = stan("brat_model3.stan", data = dataList,
             pars = c("mu_pr", "phi_pr", "eta_pr", "gam_pr", "tau_pr"),
             include = FALSE, iter = 3000, warmup = 1000, chains=4, cores=4,
             seed = "1000")

fit_4 = stan("brat_model4.stan", data = dataList,
             pars = c("mu_pr", "phi_pr", "eta_pr", "gam_pr", "tau_pr"),
             include = FALSE, iter = 3000, warmup = 1000, chains=4, cores=4,
             seed = "1000")

loo1 <- loo(fit_1, save_psis = TRUE)
loo2 <- loo(fit_2, save_psis = TRUE)
loo3 <- loo(fit_3, save_psis = TRUE)
loo4 <- loo(fit_4, save_psis = TRUE)