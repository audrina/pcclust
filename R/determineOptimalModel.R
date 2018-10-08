#' Determine Optimal GMM
#'
#' Use log likelihood criterion to select optimal model for best PC set.
#' @param bestPCSet a matrix whose columns contain the optimal principal components.
#' @return summary of output from mclust::mclustBIC(<bestPCSet>) computed with optimal model parameters.
#' @seealso \code{\link[mclust]{mclustBIC}}
#' @export
#' @examples
#' data <- validateAndLoadData(iris)
#' pcObj <- prcomp(data)
#' pcData <- pcObj$x
#' iterationResults <- executePCFiltering(pcData)
#' bestPCSet <- iterationResults[[length(iterationResults)]]
#' clusterResults <- evaluateClusterQuality(bestPCSet)
#' optimalModel <- determineOptimalModel(bestPCSet)
determineOptimalModel <- function(bestPCSet) {
  modelInfo <- initBuffer(3)
  loglikelihood <- initBuffer(3)

  cat("Evaluating BIC and log likelihood for top 3 models...........\n")

  pcBIC <- mclust::mclustBIC(bestPCSet)

  # top 3 models
  # character vector of length 3, where each element has the form "<model>,<num components>"
  models <- attributes(summary(pcBIC))$names

  for (model in models) {
    # parse model info
    params <- unlist(strsplit(model,","))

    # learn model using parameters
    info <- summary(pcBIC, bestPCSet, G=as.numeric(params[2]), modelNames=params[1])
    modelInfo(info)
    loglikelihood(info$loglik)
  }
  idxOptimalLoglik <- which(loglikelihood() == max(unlist(loglikelihood())))

  cat("Optimal model found!\n")

  return(modelInfo()[[idxOptimalLoglik]])
}
