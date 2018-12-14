rm(list = ls())

setwd('D:/git-repo/latent-variable-models/latent-variable-models/experiments/cfa/')

library(lavaan)

df <- read.table('../data/benchmarks/c6.dat')

myModel <- readLines('../data/benchmarks/c-c-6-model.lav')

fit <- cfa(myModel, data=df)
summary(fit, standardized=TRUE)
