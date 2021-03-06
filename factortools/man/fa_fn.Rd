% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fa.R
\name{fa_fn}
\alias{fa_fn}
\title{Factor Analysis}
\usage{
fa_fn(X, selectedContinuousID, selectedBinaryID, latentNum = 2)
}
\arguments{
\item{X}{A matrix (n-by-d) containing n data samples of d variables}

\item{selectedContinuousID}{Vector of indices of continuous variables}

\item{selectedBinaryID}{Vector of indices of binary or ordinal categorical variables}

\item{latentNum}{An integer for specifying the number of latent variables (default 2)}
}
\value{
A list consisting of correlation matrix, loading factors, factor scores, and fit measures
}
\description{
Perform factor analysis on a given dataset
}
