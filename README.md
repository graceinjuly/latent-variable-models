# latent-variable-models

## The factortools R package

To install this package, use 

```R
install_github("while519/latent-variable-models", subdir="factortools")
```

Two functions are used respectively for doing CFA and FA, which are *cfa_from_matrix* and *fa_fn*

For testing, please load the data from *experiments/data/benchmarks/mock-m6*, and RUN:

```R
# factor anlysis
fa_fn(mock_X, mock_selectedContinuousID, mock_selectedBinaryID, latentNum = 2)

# confirmatory factor analysis
cfa_from_matrix(mock_X, mock_selectedContinuousID, mock_selectedBinaryID, mock_adjM)
```

#### Estimators
For estimation options, refer to manual section *lavOption::estimator*, the relevant code in the lavaan source code is listed as below:

```R
# lav_options.R::lav_options_set
# default estimator
if(opt$estimator == "default") {
    if(opt$categorical) {
        opt$estimator <- "wlsmv"
    } else {
        opt$estimator <- "ml"
    }
}
```

By default lavaan use the *'wlsmv'* option for estimating the latent factor model, this option refers to the DWLS approach. In my own experience, all estimators in *('WLS', 'DWLS', 'ULS')* are valid for processing ordinal categorical data.

#### Prediction methods
see lavPredict function in the manual, I quote the following:
> In the linear case (when the indicators are continuous), the possible options are "regression" or "Bartlett". In the categorical case, the two options are "EBM" for the Empirical Bayes Modal approach, and "ML" for the maximum likelihood approach. 

However, all options are valid to run. In accordance to the manual, I would like to avoid the bad practice. Note we could also make choices about the function used for optimization in the *optim.method*.