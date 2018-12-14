setwd('D:/git-repo/latent-variable-models/latent-variable-models/experiments/demo/')

library(lavaan)

myModel <- readLines('./ex2.lav')

fit <- sem(myModel, data=PoliticalDemocracy)
summary(fit, standardized=TRUE)