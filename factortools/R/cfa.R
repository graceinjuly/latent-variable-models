#' Confirmatory Factor Analysis
#' 
#' Performs confirmatory factor analysis on a user-specified network
#' @param X A matrix (n-by-d) containing n data samples of d variables
#' @param selectedContinuousID Vector of indices of continuous variables
#' @param selectedBinaryID Vector of indices of binary or ordinal categorical variables
#' @param adjM The adjacency matrix between indicators and latent variables, value of 1 indicate a connection
#' @param lav_estimator The estimator to be used. Can be one of the following: "ML" for maximum likelihood, "GLS" for generalized leastsquares, "WLS" for weighted least squares (sometimes called ADF estimation), "ULS" for unweighted least squares and "DWLS" for diagonally weighted least squares. These are the main options that affect the estimation. For convenience, the "ML" option can be extended as "MLM", "MLMV", "MLMVS", "MLF", and "MLR"
#' @param predict_method A character string. In the linear case (when the indicators are continuous), the possible options are "regression" or "Bartlett". In the categorical case, the two options are "EBM" for the Empirical Bayes Modal approach, and "ML" for the maximum likelihood approach.
#' @param verbose Boolean, whether to output debug information. 
#' @param std.lv Boolean, if the argument std.lv=TRUE is used, the factor loadings of the first indicator of each latent variable will no longer be fixed to 1.
#' @param iter_max Integer, maximum number of iterations.
#' @param require_factrscore Boolean, if TRUE compute the factor scores for latent factors. 
#' @return A list consisting of correlation matrix, loading factors, factor scores  and fit measures
#' @export  
cfa_from_matrix <- function(X, selectedContinuousID, selectedBinaryID, adjM,
                            lav_estimator='default', predict_method='EBM',
                            verbose=FALSE, std.lv=FALSE, iter_max=10000,
                            require_factrscore=TRUE) {
  
  # convert to lavaan acceptable format
  binaryIDs <- convert2names(selectedBinaryID)
  continuousIDs <- convert2names(selectedContinuousID)
  df <- data.frame(X)
  
  # convert adjacent matrix to edges
  latentEdges <- which(adjM==1, arr.ind=T)
  latentEdges <- latentEdges[, c(2, 1)]
  colnames(latentEdges) <- NULL
  if(verbose){
    print(latentEdges)
  }

  # The model
  rvec <- convert2lav(latentEdges)
  myModel <- rvec[[1]]
  cat("The model is specified as follows:")
  cat(myModel)
  
  # call lavaan
  fit <- lavaan::cfa(myModel, data=df, ordered=binaryIDs, estimator=lav_estimator,
                     std.lv=std.lv, verbose=verbose, control=list(iter.max=iter_max))
  if(verbose){
    cat('\nsummary: \n')
    # fit summary
    lavaan::summary(fit, standardized=TRUE)

    # gradient information
    cat(lavaan::lavInspect(fit, "optim.gradient"))
  }
  
  
  # parsing names of factors and variables
  obs_IDs <- c(1:nrow(adjM))
  obs_IDs <- convert2names(obs_IDs)
  
  fc_IDs <- c(1:ncol(adjM))
  fc_IDs <- convert2names(fc_IDs, prefix="factor")
  
  # extracting information
  matList <- assign_corr(fc_IDs, obs_IDs, fit, method=predict_method,
    require_factrscore=require_factrscore)
  
  return(matList)
}

assign_corr <- function(fc_IDs, obs_IDs, fit, method='EBM', 
                        require_factrscore=TRUE) {
  weightM <- matrix(nrow = length(obs_IDs),
                ncol = length(fc_IDs), 
                dimnames = list(obs_IDs, fc_IDs))
  
  params_dframe <- lavaan::parameterEstimates(fit, standardized = TRUE)
  df1 <- params_dframe[(params_dframe$op == '=~'),]
  
  for (row in 1:nrow(df1)) {
    lhs <- df1[row, "lhs"]
    rhs <- df1[row, "rhs"]
    est <- df1[row, "est"]
    weightM[rhs, lhs] <- est
  }
  
  df2 <- params_dframe[(params_dframe$op == '~~'),]
  df2 <- df2[(df2$lhs  %in% fc_IDs),]
  df2 <- df2[(df2$rhs  %in% fc_IDs),]
  
  # correlation matrix between factors
  factorcorrM <- matrix(nrow = length(fc_IDs),
                 ncol = length(fc_IDs), 
                 dimnames = list(fc_IDs, fc_IDs))
  for (row in 1:nrow(df2)) {
    lhs <- df2[row, "lhs"]
    rhs <- df2[row, "rhs"]
    est <- df2[row, "est"]
    factorcorrM[lhs, rhs] <- est
    factorcorrM[rhs, lhs] <- est
  }
  
  # fit measure may not be computed
  s <- tryCatch(
  lavaan::fitMeasures(fit, c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr")),
  error = function(e) {warning('fit measures not available'); s <-c(); return(s)}
  )
  
  if(require_factrscore){
        mat3_tmp <- lavaan::lavPredict(fit, method=method, optim.method='bfgs')
        factorscoresM <- matrix(nrow = nrow(mat3_tmp),
                                ncol = length(fc_IDs))
        colnames(factorscoresM) <- fc_IDs
        factorscoresM[, colnames(mat3_tmp)] <- mat3_tmp
        rownames(factorscoresM) <- NULL
        colnames(factorscoresM) <- NULL
    } else{
      factorscoresM <- NULL
    }
  
  colnames(weightM) <- NULL
  rownames(weightM) <- NULL
  
  colnames(factorcorrM) <- NULL
  rownames(factorcorrM) <- NULL
  
  return(list(factorcorrM, weightM, factorscoresM, s))
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
  jj <- 0
  for(rid in 1:nrow(edgeMatrix)) {
    if(identical(temp_ID, edgeMatrix[rid, 1])){
      myModel <-  paste(myModel, ' + V', edgeMatrix[rid, 2], sep="")
      jj <- jj + 1
    } else{
      if(jj == 1){
        myModel <-  paste(myModel, '\nV', edgeMatrix[rid - 1, 2], '~~',
                          '0.005*V', edgeMatrix[rid - 1, 2], sep="")
      }
      temp_ID <- edgeMatrix[rid, 1]
      fc_IDs[[i]] <- temp_ID
      i <- i + 1
      myModel <-  paste(myModel, '\nfactor', temp_ID, ' =~ V', edgeMatrix[rid, 2], sep="")
      jj <- 1
    }
  }
  if(jj == 1){
    myModel <-  paste(myModel, '\nV', edgeMatrix[rid, 2], '~~',
                      '0.005*V', edgeMatrix[rid, 2], sep="")
  }
  myModel <- paste(myModel, '\n')
  fc_IDs <- unlist(fc_IDs, use.names=FALSE)
  return(list(myModel, fc_IDs))
}

