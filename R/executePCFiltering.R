#' Filter principal component data
#'
#' Filter raw principal component data automatically until only the top 2 PCs remain.
#' @param pcData a numerical R matrix or data frame.
#' @details Indirect recursion in the case of optimal subset ambiguity (i.e. different models for maxBIC and maxLikelihood)
#           Use closure to store the resultant PC matrix from each filtering iteration in a list.
#' @return list of iteration results from PCA filtering, where each iteation has one less PC. List elements are PCA matrices. Last entry in the list corresponds to the top 2 PCs for clustering.
#' @seealso \code{\link{evaluateClusterQuality}}
#' @export
#' @examples
#' data <- validateAndLoadData(iris)
#' pcObj <- prcomp(data)
#' pcData <- pcObj$x
#' iterationResults <- executePCFiltering(pcData)
executePCFiltering <- function(pcData) {
  # Purpose:
  #     Filter raw principal component data automatically until only the top 2 PCs remain.
  #     Indirect recursion in the case of optimal subset ambiguity (i.e. different models for maxBIC and maxLikelihood)
  #     Use closure to store the resultant PC matrix from each filtering iteration in a list.
  # Parameters:
  #     pcData      a validated R matrix containing numeric scaled data.
  # Value:
  #     list of iteration results from PCA filtering, where each iteation has one less PC.
  #     List elements are PCA matrices. Last entry in the list corresponds to the top 2 PCs for clustering.

  cat("Running PC filtering ...............\n")

  nIterations <- ncol(pcData) - 2

  # base case for indirect recursion on ambiguous subsets: stop iterations once left with top 2 PCs
  if (nIterations <= 0) {
    return(pcData)
  }

  iterations <- initBuffer(nIterations)
  selIn <- pcData

  for (iter in 1:nIterations) {
    cat(sprintf("ITERATION %d\n", iter))
    subsets <- generatePCSubsets(selIn)
    subsetsClusterQuality <- lapply(subsets, evaluateClusterQuality)
    idxOptimalSubset <- determineOptimalSubset(subsetsClusterQuality)
    # sel <- subsets[[idxOptimalSubset]]
    sel <- checkSubsetAmbiguity(subsets, idxOptimalSubset) # conditional indirect recusion
    iterations(sel)
    selIn <- sel
  }
  cat("PC filtering complete!\n")
  return(iterations())
}
