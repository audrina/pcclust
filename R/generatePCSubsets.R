#' generatePCSubsets
#'
#' @param pcData a validated R matrix containing numeric scaled data.
#' @param subsetSize number of principal components
#' @importFrom utils combn
#' @return list of PCA matrices
generatePCSubsets <- function(pcData, subsetSize=NULL) {
  # Purpose:
  #     Perform principle components analysis on matrix
  #     and generate all possible subsets of principle components.
  # Parameters:
  #     pcData          a validated R matrix containing numeric scaled data.
  #     subsetSize      number of principal components
  # Value:
  #     list of matrices, each a unique subset of principal components.

  #pcObj <- prcomp(df)
  #pcData <- pcObj$x
  nPCs <- ncol(pcData)
  pcSet <- seq(nPCs)

  if (is.null(subsetSize)) {
    subsetSize <- nPCs - 1
  }

  # iteratively remove PCs
  # reduce the PC subset size by 1 in each iteration until a subset size of 2 is reached.
  # use closure function to store iterations

  pcSubsets <- utils::combn(nPCs, subsetSize)

  pcDataSubsets <- initBuffer(subsetSize)
  for (i in seq(subsetSize)) {
    pcDataSubsets(pcData[ ,pcSubsets[ ,i]])
  }

  # pcDataSubsets() contains a list of length subsetSize, wher each element in the list is a unique subset of PCs.

  # evaluate cluster quality for each PC subset in buffer
  # pcMatrix <- pcDataSubsets()[[1]]
  # clusterQuality <- makeBuffer(subsetSize)

  return(pcDataSubsets())
}
