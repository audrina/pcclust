#' Evaluate PC Cluster Quality
#'
#' compute Bayesian information criterion (BIC) for Gaussian mixture models (GMM) and associated log likelihood to quantify cluster quality for a subset of PCs.
#' @param pcMatrix a matrix whose columns contain the principal components.
#' @return list containing optimal model characteristics and classification
#' @seealso \code{\link[mclust]{mclustBIC}}
#' @seealso \code{\link[mclust]{summary.mclustBIC}}
#' @import mclust
#' @export
#' @examples
#' data <- validateAndLoadData(iris)
#' pcObj <- prcomp(data)
#' pcData <- pcObj$x
#' iterationResults <- executePCFiltering(pcData)
#' bestPCSet <- iterationResults[[length(iterationResults)]]
#' clusterResults <- evaluateClusterQuality(bestPCSet)
evaluateClusterQuality <- function(pcMatrix) {
  # Purpose: compute Bayesian information criterion (BIC)
  #          for Gaussian mixture models (GMM) and
  #          associated log likelihood to quantify
  #          cluster quality for a subset of PCs.
  # Parameters:
  #     pcMatrix    a matrix whose columns contain the principal components.
  # Value:
  #     list containing optimal model characteristics and classification

  pcBIC <- mclust::mclustBIC(pcMatrix)
  summaryBIC <- mclust::summary.mclustBIC(pcBIC, pcMatrix)

  return(summaryBIC)
}
