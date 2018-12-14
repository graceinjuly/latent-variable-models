library(lavaan)
myModel <- readLines("./ex1.lav")

fit <- cfa(myModel, data=HolzingerSwineford1939)
summary(fit, fit.measures=TRUE)