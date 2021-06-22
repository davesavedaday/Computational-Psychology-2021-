rm(list=ls())
graphics.off()

library(rstan)
library(bayesplot)
library(loo)
options(mc.cores = parallel::detectCores())

dat = read.table("export_brat_time1(n=20).txt", header = T)
dat_shortened = read.table("export_brat_time1(n=2).txt", header = T)

allSubjs <- unique(dat$Subject)
N <- length(allSubjs)
T <- table(dat$Subject)[1]
numPars <- 5
Tsubj <- numeric()
for(i in 1:N) {
  Tsubj = c(Tsubj, table(dat$Subject)[i])
}

dataList <- list(
  N = N,
  T = T,
  I = 128,
  Tsubj = Tsubj,
  pumps = matrix(nrow = N, dat$pumped),
  explosion = matrix(nrow = N, dat$explosion)
)
fit_modified = stan("brat_model2(modified).stan", data = dataList,
                    iter = 3000, warmup = 1000, chains =4, cores=4,
                    seed = "1000")
fit_no_indiv = stan("brat_model2(no_indiv).stan",data = dataList,
                    iter = 3000, warmup = 1000, chains=4, cores=4,
                    seed = "1000" )

fit_no_hier = stan("brat_model2(no_hier).stan", data= dataList,
                   iter = 3000, warmup = 1000, chains=4, cores=4,
                   seed = "1000")

fit_hier = readRDS("fit_2.rds")
