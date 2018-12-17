#' Factor Analysis
#' 
#' Perform factor analysis on a given dataset
#' @param X A matrix (n-by-d) containing n data samples of d variables
#' @param selectedContinuousID Vector of indices of continuous variables
#' @param selectedBinaryID Vector of indices of binary or ordinal categorical variables
#' @param latentNum An integer for specifying the number of latent variables (default 2)
#' @return A list consisting of correlation matrix, loading factors and fit measures
#' @export 
fa_fn <- function(X, selectedContinuousID, selectedBinaryID, latentNum=2) {
  # library(lavaan)
	# library(semTools)
  
  # convert to lavaan acceptable format
  binaryIDs <- convert2names(selectedBinaryID)
  continuousIDs <- convert2names(selectedContinuousID)
  df <- data.frame(X)
  
  unrotated  <- semTools::efaUnrotate(X, nf = latentNum, ordered=binaryIDs)
  fit <- semTools::oblqRotate(unrotated, method = "quartimin")
  
  obs_IDs <- c(selectedBinaryID, selectedContinuousID)
  obs_IDs <- obs_IDs[order(obs_IDs)]
  obs_IDs <- convert2names(obs_IDs)
  
  fc_IDs <- c(1:latentNum)
  fc_IDs <- convert2names(fc_IDs, prefix="factor")
  
  mat1 <- fit@loading
  mat2 <- fit@phi
  
  colnames(mat1) <- NULL
  rownames(mat1) <- NULL
  
  colnames(mat2) <- NULL
  rownames(mat2) <- NULL
  
  s <- semTools::fitMeasures(unrotated, c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr"))
  s <- c(s)
  return(list(mat2, mat1, s))
  
}

convert2names <- function(selectedIDs, prefix='V'){
  resultList <- list()
  i <- 1
  for(ind in selectedIDs){
    ind_string <- toString(ind)
    resultList[[i]] <- paste(prefix, ind_string, sep="")
    i <- i + 1
  }
  return(resultList)
}


