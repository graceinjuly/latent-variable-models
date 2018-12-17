cfa_from_edges <- function(X, selectedContinuousID, selectedBinaryID, latentEdges) {
	# library(lavaan)
  
  # convert to lavaan acceptable format
  binaryIDs <- convert2names(selectedBinaryID)
  continuousIDs <- convert2names(selectedContinuousID)
  df <- data.frame(X)
  
  # The model
  rvec <- convert2lav(latentEdges)
  myModel <- rvec[[1]]
  cat("The model is specified as follows:")
  cat(myModel)
  
  fit <- lavaan::cfa(myModel, data=df, ordered=binaryIDs)
  cat('\nsummary: \n')
  summary(fit, standardized=TRUE)
  
  
  
  obs_IDs <- c(selectedBinaryID, selectedContinuousID)
  obs_IDs <- obs_IDs[order(obs_IDs)]
  obs_IDs <- convert2names(obs_IDs)
  
  fc_IDs <- rvec[[2]]
  fc_IDs <- fc_IDs[order(fc_IDs)]
  fc_IDs <- convert2names(fc_IDs, prefix="factor")
  
  # res.mat <- resid(fit)
  # res.mat <- res.mat$cov

  matList <- assign_corr(fc_IDs, obs_IDs, fit)

  
	return(matList)
}

#' Confirmatory Factor Analysis
#' 
#' Performs confirmatory factor analysis on a user-specified network
#' @param X A matrix (n-by-d) containing n data samples of d variables
#' @param selectedContinuousID Vector of indices of continuous variables
#' @param selectedBinaryID Vector of indices of binary or ordinal categorical variables
#' @param adjM The adjacency matrix between indicators and latent variables, value of 1 indicate a connection
#' @return A list consisting of correlation matrix, loading factors  and fit measures
#' @export  
cfa_from_matrix <- function(X, selectedContinuousID, selectedBinaryID, adjM) {
  # library(lavaan)
  
  # convert to lavaan acceptable format
  binaryIDs <- convert2names(selectedBinaryID)
  continuousIDs <- convert2names(selectedContinuousID)
  df <- data.frame(X)
  
  latentEdges <- which(adjM==1, arr.ind=T)
  latentEdges <- latentEdges[, c(2, 1)]
  colnames(latentEdges) <- NULL
  # print(latentEdges)
  # The model
  rvec <- convert2lav(latentEdges)
  myModel <- rvec[[1]]
  cat("The model is specified as follows:")
  cat(myModel)
  
  fit <- lavaan::cfa(myModel, data=df, ordered=binaryIDs)
  # cat('\nsummary: \n')
  # summary(fit, standardized=TRUE)
  
  
  
  obs_IDs <- c(selectedBinaryID, selectedContinuousID)
  obs_IDs <- obs_IDs[order(obs_IDs)]
  obs_IDs <- convert2names(obs_IDs)
  
  fc_IDs <- c(1:ncol(adjM))
  fc_IDs <- convert2names(fc_IDs, prefix="factor")
  
  # res.mat <- resid(fit)
  # res.mat <- res.mat$cov
  
  matList <- assign_corr(fc_IDs, obs_IDs, fit)
  
  
  return(matList)
}

assign_corr <- function(fc_IDs, obs_IDs, fit) {
  mat1 <- matrix(nrow = length(obs_IDs),
                ncol = length(fc_IDs), 
                dimnames = list(obs_IDs, fc_IDs))
  
  params_dframe <- lavaan::parameterEstimates(fit, standardized = TRUE)
  df1 <- params_dframe[(params_dframe$op == '=~'),]
  
  for (row in 1:nrow(df1)) {
    lhs <- df1[row, "lhs"]
    rhs <- df1[row, "rhs"]
    est <- df1[row, "est"]
    mat1[rhs, lhs] <- est
  }
  
  df2 <- params_dframe[(params_dframe$op == '~~'),]
  df2 <- df2[(df2$lhs  %in% fc_IDs),]
  df2 <- df2[(df2$rhs  %in% fc_IDs),]
  
  mat2 <- matrix(nrow = length(fc_IDs),
                 ncol = length(fc_IDs), 
                 dimnames = list(fc_IDs, fc_IDs))
  for (row in 1:nrow(df2)) {
    lhs <- df2[row, "lhs"]
    rhs <- df2[row, "rhs"]
    est <- df2[row, "est"]
    mat2[lhs, rhs] <- est
    mat2[rhs, lhs] <- est
  }
  
  # res.mat <- resid(fit)
  # res.mat <- res.mat$cov
  # # Matrix::forceSymmetric(res.mat, uplo = "L")
  # 
  # for (i in 1:length(obs_IDs)){
  #   for (j in 1:i){
  #     mat[i, j] <- res.mat[i,j]
  #     mat[j, i] <- res.mat[i,j]
  #   }
  # }
  colnames(mat1) <- NULL
  rownames(mat1) <- NULL

  colnames(mat2) <- NULL
  rownames(mat2) <- NULL
  
  s <- lavaan::fitMeasures(fit, c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr"))
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

convert2lav <- function(edgeMatrix) {
  edgeMatrix <- edgeMatrix[order(edgeMatrix[,2]),]
  edgeMatrix <- edgeMatrix[order(edgeMatrix[,1]), ]
  
  temp_ID = NULL
  fc_IDs <- list()
  i <- 1
  myModel <- ""
  for(rid in 1:nrow(edgeMatrix)) {
    if(identical(temp_ID, edgeMatrix[rid, 1])){
      myModel <-  paste(myModel, ' + V', edgeMatrix[rid, 2], sep="")
    } else{
      temp_ID <- edgeMatrix[rid, 1]
      fc_IDs[[i]] <- temp_ID
      i <- i + 1
      myModel <-  paste(myModel, '\nfactor', temp_ID, ' =~ V', edgeMatrix[rid, 2], sep="")
    }
    
  }
  myModel <- paste(myModel, '\n')
  fc_IDs <- unlist(fc_IDs, use.names=FALSE)
  return(list(myModel, fc_IDs))
}

