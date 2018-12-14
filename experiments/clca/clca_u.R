rm(list = ls())

setwd('D:/git-repo/latent-variable-models/latent-variable-models/experiments/clca/')

library(poLCA)

df <- read.table('../data/benchmarks/u6.dat')
df <- df + 1

f1 <- cbind(V1,V2,V3) ~ 1
lc1 <- poLCA(f1, df, nclass=1)

f2 <- cbind(V4,V5,V6) ~ 1
lc2 <- poLCA(f2, df, nclass=1)
