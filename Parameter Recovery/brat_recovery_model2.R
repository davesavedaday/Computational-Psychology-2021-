# rm(list=ls())
# graphics.off()

library(rstan)
library(bayesplot)
library(loo)
options(mc.cores = parallel::detectCores())

sim_model2 = read.table("simul_data_model2.txt", header = T)

allSubjs <- unique(sim_model2$subjID)
N <- length(allSubjs)
T <- table(sim_model2$subjID)[1]
Tsubj <- numeric()
for(i in 1:N) {
  Tsubj = c(Tsubj, table(sim_model2$subjID)[i])
}

dataList <- list(
  N = N,
  T = T,
  I = 128,
  Tsubj = Tsubj,
  pumps = matrix(nrow = N, sim_model2$pumped),
  explosion = matrix(nrow = N, sim_model2$explosion)
)

fit_2 = stan("brat_model2.stan", data = dataList,
             pars = c("mu_pr", "phi_pr", "eta_pr", "gam_pr", "tau_pr"),
             include = FALSE, iter = 2000, warmup = 1000, chains=2, cores=2,
             seed = "1000")
parameters <- rstan::extract(fit_2)


phi_mean = apply(parameters$phi, 2, mean)
phi_sd = apply(parameters$phi, 2, sd)
plot(simul_pars$phi_pos, phi_mean, xlim=c(0,1), ylim=c(0,1)); abline(0,1)
arrows(x0=simul_pars$phi_pos, y0= phi_mean - phi_sd, y1= phi_mean + phi_sd, length=0.02, angle=90, code=3)

eta_mean = apply(parameters$eta, 2, mean)
eta_sd = apply(parameters$eta, 2, sd)
plot(simul_pars$eta_pos, eta_mean, xlim=c(0,2), ylim=c(0,2)); abline(0,1)
arrows(x0=simul_pars$eta_pos, y0= eta_mean - eta_sd, y1= phi_mean + eta_sd, length=0.02, angle=90, code=3)

gam_mean = apply(parameters$gam, 2, mean)
gam_sd = apply(parameters$gam, 2, sd)
plot(simul_pars$gam_pos, gam_mean, xlim=c(0,0.3), ylim=c(0,0.3)); abline(0,1)
arrows(x0=simul_pars$gam_pos, y0= gam_mean - gam_sd, y1= gam_mean + gam_sd, length=0.02, angle=90, code=3)

tau_mean = apply(parameters$tau, 2, mean)
tau_sd = apply(parameters$tau, 2, sd)
plot(simul_pars$tau_pos, tau_mean, xlim=c(0,4), ylim=c(0,4)); abline(0,1)
arrows(x0=simul_pars$tau_pos, y0= tau_mean - tau_sd, y1= tau_mean + tau_sd, length=0.02, angle=90, code=3)

