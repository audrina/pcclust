determineOptimalSubset <- function(subsets) {
  # Purpose:
  #     Determine the index of the subset with the optimal BIC and loglikelihood.
  # Parameters:
  #     subsets     list of summary.mclustBIC() lists where each sublist is a unique PC subset.
  # Value:
  #     integer (or vector of 2 integers)

  size <- length(subsets)
  # initialize closures
  bic <- initBuffer(size)
  loglikelihood <- initBuffer(size)

  for (i in seq(size)) {
    bic(subsets[[i]]$bic)
    loglikelihood(subsets[[i]]$loglik)
  }

  idxOptimalBIC <- which(bic() == max(unlist(bic()))) # subset index
  idxOptimalLoglik <- which(loglikelihood() == max(unlist(loglikelihood())))

  if (all(idxOptimalBIC == idxOptimalLoglik)) {
    # optimal subset is unambiguous
    return(idxOptimalBIC)
  }

  else {
    # retain both subsets for further downstream filtering
    return(c(idxOptimalBIC, idxOptimalLoglik)) # vector of a pair of indices
  }
}
