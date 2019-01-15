rm(list = ls())

setwd('D:/Documents/git-repo/latent-variable-models/latent-variable-models/experiments/cfa/')

library(lavaan)

df <- read.table('../data/benchmarks/m6.dat')


myModel <- readLines('../data/benchmarks/m-c-6-model.lav')

fit <- cfa(myModel, data=df, ordered=c("V1", "V2", "V3"), std.lv=TRUE)
lavPredict(fit)

summary(fit, standardized=TRUE)
