# Confirmatory factor analysis in factortools package

## Model specification
When the outcome variable is categorical, the default behaviour of fixing the variance of residule as 0 (correspoond to syntax *auto.fix.single = TRUE*) is no longer taking effect for single indicator factor model.

## Performance
When estimating the latent variable representation, it is sensitive to the size of data.

## Inherent behaviour
For learning the latent variable representation, only very a few different values can be obtained for the factor scores (*?Inconsistent result has been obtained for the single indicator variable model*).