checkSubsetAmbiguity <- function(subsets, idxOptimalSubset) {
  # In the case of ambiguity, recursively execute PC filtering until there is an unambiguous
  # optimal subset of 2 PCs (i.e. BIC and log likelihood criterions agree on the best model)

  if (length(idxOptimalSubset) == 1) {
    sel <- subsets[[idxOptimalSubset]]
  }

  else {
    # (length(idxOptimalSubset) > 1)
    # different models for maxBIC and maxLikelihood
    # need to further evaluate both subsets to determine which
    # criterion is a more stringent quantification of cluster quality.

    # indirect recursion - returns when 2 PCs remain
    sel1 <- executePCFiltering(subsets[[idxOptimalSubset[0]]])
    sel2 <- executePCFiltering(subsets[[idxOptimalSubset[1]]])

    # recursion complete. Compare the optimal PC sets from the final iteration.
    result <- list(sel1, sel2)
    subsetsClusterQuality <- lapply(result, evaluateClusterQuality)
    idxOptimalSubset <- determineOptimalSubset(subsetsClusterQuality)
    sel <- subsets[[idxOptimalSubset]]
  }
  return(sel)
}
