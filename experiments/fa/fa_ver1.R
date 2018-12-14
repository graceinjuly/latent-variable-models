fa_fn <- function(X, selectedContinuousID, selectedBinaryID, latentNum=NULL) {
	library(semTools)
  
  # convert to lavaan acceptable format
  binaryIDs <- convert2names(selectedBinaryID)
  continuousIDs <- convert2names(selectedContinuousID)
  df <- data.frame(X)
  
  unrotated  <- efaUnrotate(X, nf = 3, ordered=binaryIDs)
  fit <- oblqRotate(unrotated, method = "quartimin")
  cat('\nsummary: \n')
  summary(fit)
  
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


