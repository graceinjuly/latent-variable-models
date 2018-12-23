# latent-variable-models

## factortools R package

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