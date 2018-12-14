rm(list = ls())

setwd('D:/git-repo/latent-variable-models/latent-variable-models/experiments/cfa/')

library(lavaan)

df <- read.table('../data/benchmarks/u6.dat')

df[, ] <- lapply(df[, ], ordered)

myModel <- readLines('../data/benchmarks/u-u-6-model.lav')

fit <- cfa(myModel, data=df)
summary(fit, standardized=TRUE)
