# pcclust.R
#
# Purpose:  Pcclust is a PCA analysis and visualization
#           package tailored for optimizing downstream
#           clustering for any numerical multidimensional
#           data matrix.
# Version: 1.0
# Date: 25-09-18
# Author: Audrina Z.
#
# Input: numerical multidimensional data matrix
# Output: informative optimized PCA plot
# Dependencies: mclust, ggplot2, rio
#
# ToDo:
# Notes:
#
# ==============================================================================

# setwd("/Users/audrina/Documents/BCB410/pcclust")

# ====  PARAMETERS  ============================================================
# Define and explain all parameters. No "magic numbers" in your code below.



# ====  PACKAGES  ==============================================================
# Load all required packages.

# if (! require(mclust, quietly=TRUE)) {
#   install.packages("mclust")
#   library(mclust)
# }
#
# # streamlined data file I/O
# if (! require(rio, quietly=TRUE)) {
#   install.packages("rio")
#   library(rio)
# }
#
# if (! require(ggplot2, quietly=TRUE)) {
#   install.packages("ggplot2")
#   library(ggplot2)
# }
#
# # convert to percent
# library(scales)
#
# # 3D pie chart
# if (! require(plotrix, quietly=TRUE)) {
#   install.packages("plotrix")
#   library(plotrix)
# }
#
# # interactive 3D scatterplot
# if (! require(rgl, quietly=TRUE)) {
#   install.packages("rgl")
#   library(rgl)
# }
# if (! require(car, quietly=TRUE)) {
#   install.packages("car")
#   library(car)
# }


# Package information:
#  library(help = seqinr)       # basic information
#  browseVignettes("seqinr")    # available vignettes
#  data(package = "seqinr")     # available datasets


# ====  FUNCTIONS  =============================================================

# Define functions or source external files
devtools::load_all()

# Never use source() to load code from a file. source() modifies the current
# environment, inserting the results of executing the code. Instead, rely on
# devtools::load_all() which automatically sources all files in R/.

# source("<myUtilityFunctionsScript.R>")

# validateAndLoadData <- function(data, isFile = FALSE, scaled = TRUE) {
#   # Purpose:
#   #     Validate input type and load data into data frame.
#   #     Supported input types:
#   #     - R numerical matrix or data frame
#   #     - a wide range of file formats (refer to https://github.com/leeper/rio for a full
#   #                                     list of supported formats)
#   # Parameters:
#   #     data      a numerical R matrix or data frame.
#   #     isFile    if TRUE, data accepts a path to a file containing numeric multidimensional data.
#   #               FALSE by default.
#   #     scaled    if TRUE, use Z-score standardization so that outliers aren"t weighted less.
#   #               TRUE by default.
#   # Value:
#   #     R data frame with loaded data
#
#   # 1. load data into data frame
#
#   if (isFile == TRUE) {
#     # rio automated import chooses appropriate function based on file extension
#     df <- rio::import(data)
#     if (colnames(df)[1] == "V1") {
#       # remove additional row index column
#       df <- df[-1]
#     }
#   }
#
#   else if (is.matrix(data)) {
#     df <- as.data.frame(data)
#   }
#
#   else {
#     # data is already a data frame
#     df <- data
#   }
#
#   # 2. validate that data type is purely numeric
#
#   # if labels are included, separate from the numeric data
#   numCol <- sapply(df, is.numeric)
#   labelsIdx <- which(numCol == FALSE) # column index
#
#    if (length(labels) > 1) {
#     stop("Invalid dataset: contains more than 1 column of non-numeric data")
#    }
#
#   else if (length(labels) == 1) {
#     # labels <- data[labelsIdx]
#     df <- df[-labelsIdx]
#   }
#
#   valid <- all(sapply(df, is.numeric))
#   if (! valid) {
#     stop("Invalid dataset: input data must be purely numeric!")
#   }
#
#   if (scaled == TRUE) {
#     df <- scale(df)
#   }
#
#   return(df)
# }
#
# evaluateClusterQuality <- function(pcMatrix) {
#   # Purpose: compute Bayesian information criterion (BIC)
#   #          for Gaussian mixture models (GMM) and
#   #          associated log likelihood to quantify
#   #          cluster quality for a subset of PCs.
#   # Parameters:
#   #     pcMatrix    a matrix whose columns contain the principal components.
#   # Value:
#   #     list containing optimal model characteristics and classification
#
#   pcBIC <- mclust::mclustBIC(pcMatrix)
#   summaryBIC <- mclust::summary.mclustBIC(pcBIC, pcMatrix)
#
#   return(summaryBIC)
# }
#
# initBuffer <- function(n) {
#   # closure function
#   # following example from https://www.r-bloggers.com/closures-in-r-a-useful-abstraction/
#   subsets <- vector("list", length = n)
#   i <- 1
#
#   function(sel=NULL) {
#     if (is.null(sel)) {
#       return(subsets)
#     }
#     else {
#       subsets[[i]] <<- sel # scoping assignment
#       i <<- i + 1
#     }
#   }
# }
#
# # runPCA <- function(df) {
# #   # Purpose:
# #   #     Perform principle components analysis on data frame
# #   # Parameters:
# #   #     df          a validated R data frame containing numeric scaled data.
# #   # Value:
# #   #     matrix
# #
# #   return(prcomp(df)$x)
# #
# # }
#
# generatePCSubsets <- function(pcData, subsetSize=NULL) {
#   # Purpose:
#   #     Perform principle components analysis on matrix
#   #     and generate all possible subsets of principle components.
#   # Parameters:
#   #     pcData          a validated R matrix containing numeric scaled data.
#   #     subsetSize      number of principal components
#   # Value:
#   #     list of matrices, each a unique subset of principal components.
#
#   #pcObj <- prcomp(df)
#   #pcData <- pcObj$x
#   nPCs <- ncol(pcData)
#   pcSet <- seq(nPCs)
#
#   if (is.null(subsetSize)) {
#     subsetSize <- nPCs - 1
#   }
#
#   # iteratively remove PCs
#   # reduce the PC subset size by 1 in each iteration until a subset size of 2 is reached.
#   # use closure function to store iterations
#
#   pcSubsets <- combn(nPCs, subsetSize)
#
#   pcDataSubsets <- initBuffer(subsetSize)
#   for (i in seq(subsetSize)) {
#     pcDataSubsets(pcData[ ,pcSubsets[ ,i]])
#   }
#
#   # pcDataSubsets() contains a list of length subsetSize, wher each element in the list is a unique subset of PCs.
#
#   # evaluate cluster quality for each PC subset in buffer
#   # pcMatrix <- pcDataSubsets()[[1]]
#   # clusterQuality <- makeBuffer(subsetSize)
#
#   return(pcDataSubsets())
# }
#
# determineOptimalSubset <- function(subsets) {
#   # Purpose:
#   #     Determine the index of the subset with the optimal BIC and loglikelihood.
#   # Parameters:
#   #     subsets     list of summary.mclustBIC() lists where each sublist is a unique PC subset.
#   # Value:
#   #     integer (or vector of 2 integers)
#
#   size <- length(subsets)
#   # initialize closures
#   bic <- initBuffer(size)
#   loglikelihood <- initBuffer(size)
#
#   for (i in seq(size)) {
#     bic(subsets[[i]]$bic)
#     loglikelihood(subsets[[i]]$loglik)
#   }
#
#   idxOptimalBIC <- which(bic() == max(unlist(bic()))) # subset index
#   idxOptimalLoglik <- which(loglikelihood() == max(unlist(loglikelihood())))
#
#   if (all(optimalBIC == optimalLoglik)) {
#     # optimal subset is unambiguous
#     return(idxOptimalBIC)
#   }
#
#   else {
#     # retain both subsets for further downstream filtering
#     return(c(idxOptimalBIC, idxOptimalLoglik)) # vector of a pair of indices
#   }
# }
#
# checkSubsetAmbiguity <- function(subsets, idxOptimalSubset) {
#   # In the case of ambiguity, recursively execute PC filtering until there is an unambiguous
#   # optimal subset of 2 PCs (i.e. BIC and log likelihood criterions agree on the best model)
#
#   if (length(idxOptimalSubset) == 1) {
#     sel <- subsets[[idxOptimalSubset]]
#   }
#
#   else {
#     # (length(idxOptimalSubset) > 1)
#     # different models for maxBIC and maxLikelihood
#     # need to further evaluate both subsets to determine which
#     # criterion is a more stringent quantification of cluster quality.
#
#     # indirect recursion - returns when 2 PCs remain
#     sel1 <- executePCFiltering(subsets[[idxOptimalSubset[0]]])
#     sel2 <- executePCFiltering(subsets[[idxOptimalSubset[1]]])
#
#     # recursion complete. Compare the optimal PC sets from the final iteration.
#     result <- list(sel1, sel2)
#     subsetsClusterQuality <- lapply(result, evaluateClusterQuality)
#     idxOptimalSubset <- determineOptimalSubset(subsetsClusterQuality)
#     sel <- subsets[[idxOptimalSubset]]
#   }
#   return(sel)
# }
#
# # executeClusterAnalysis <- function(pcData) {
# #   subsets <- generatePCSubsets(pcData)
# #   subsetsClusterQuality <- lapply(subsets, evaluateClusterQuality)
# #   idxOptimalSubset <- determineOptimalSubset(subsetsClusterQuality)
# #
# #   sel <- checkSubsetAmbiguity(subsets, idxOptimalSubset)
# # }
#
# executePCFiltering <- function(pcData) {
#   # Purpose:
#   #     Filter raw principal component data automatically until only the top 2 PCs remain.
#   #     Indirect recursion in the case of optimal subset ambiguity (i.e. different models for maxBIC and maxLikelihood)
#   #     Use closure to store the resultant PC matrix from each filtering iteration in a list.
#   # Parameters:
#   #     pcData      a validated R matrix containing numeric scaled data.
#   # Value:
#   #     list of matrices ordered by where each item in the list contains a PC matrix,
#   #     where  each iteration has one less PC.
#
#   nIterations <- ncol(pcData) - 2
#
#   # base case for indirect recursion on ambiguous subsets: stop iterations once left with top 2 PCs
#   if (nIterations <= 0) {
#     return(pcData)
#   }
#
#   iterations <- initBuffer(nIterations)
#   selIn <- pcData
#
#   for (iter in 1:nIterations) {
#     subsets <- generatePCSubsets(selIn)
#     subsetsClusterQuality <- lapply(subsets, evaluateClusterQuality)
#     idxOptimalSubset <- determineOptimalSubset(subsetsClusterQuality)
#     # sel <- subsets[[idxOptimalSubset]]
#     sel <- checkSubsetAmbiguity(subsets, idxOptimalSubset) # conditional indirect recusion
#     iterations(sel)
#     selIn <- sel
#   }
#   return(iterations())
# }
#
# determineOptimalModel <- function(bestPCSet, models) {
#   modelInfo <- initBuffer(3)
#   loglikelihood <- initBuffer(3)
#
#   pcBIC <- mclust::mclustBIC(bestPCSet)
#
#   # top 3 models
#   # character vector of length 3, where each element has the form "<model>,<num components>"
#   models <- attributes(summary(pcBIC))$names
#
#   for (model in models) {
#     # parse model info
#     params <- unlist(strsplit(model,","))
#
#     # learn model using parameters
#     info <- summary(pcBIC, bestPCSet, G=as.numeric(params[2]), modelNames=params[1])
#     modelInfo(info)
#     loglikelihood(info$loglik)
#   }
#   idxOptimalLoglik <- which(loglikelihood() == max(unlist(loglikelihood())))
#   return(modelInfo()[[idxOptimalLoglik]])
# }
#
# findNearestNeighbor <- function(pcCol, queryPt) {
#   # data.table inherits from data.frame. It is faster and more memory efficient.
#   dt <- data.table::data.table(pcCol)
#
#   # sort xTable in ascending order and add an attribute "sorted"
#   data.table::setkey(dt, pcCol)
#
#   # search and "roll" to nearest neighbor
#   idxNearest <- dt[.(queryPt), roll="nearest", which=TRUE] # row index of nearest neighbor
#   valNearest <- dt$pcCol[idxNearest]
#
#   return(valNearest)
# }
#
# chooseClosestSample <- function(bestPCSet, x, y, nearestX, nearestY) {
#   # choose sample corresponding to the nearest neighbor for the PC
#   # that has the least combined absolute deviation from the query.
#
#   xPC <- colnames(bestPCSet)[1]
#   yPC <- colnames(bestPCSet)[2]
#
#   rowX <- which(bestPCSet[ ,xPC] == nearestX)
#   xNearestY <- bestPCSet[rowX, yPC]
#   errX <- abs(x - nearestX) + abs(y - xNearestY)
#
#   rowY <- which(bestPCSet[ ,yPC] == nearestY)
#   yNearestX <- bestPCSet[rowY, xPC]
#   errY <- abs(x - yNearestX) + abs(y - nearestY)
#
#   dimLeastErr <- which(c(errX, errY) == min(c(errX, errY)))
#
#   if (dimLeastErr == 1) {
#     # nearest neighbor for xPC is closest to the query (x, y)
#     nearestChoice <- nearestX
#   }
#
#   else {
#     # nearest neighbor for yPC is closest to the query (x, y)
#     nearestChoice <- nearestY
#   }
#
#   return(list(colPC = colnames(bestPCSet)[dimLeastErr], val = nearestChoice)) # [<PC name>, <value nearest neighbor>]
# }
#
# visualizePCA <- function(bestPCSet, clusters, outDir = ".") {
#   outPath <- file.path(outDir, "pcclust_visualization")
#   if (!dir.exists(outPath)) {
#     cmd <- sprintf("mkdir %s", outPath)
#     system(cmd)
#   }
#   setwd(outPath)
#
#   # output high quality .tiff PCA plots corresponding to the optimal set of PCs
#
#   dfBestPC <- data.frame(bestPCSet)
#   dfBestPC <- cbind(dfBestPC, clusters)
#
#   # encode clusters column as a factor
#   dfBestPC$clusters <- as.factor(dfBestPC$clusters)
#
#   dim1Name <- colnames(dfBestPC)[1]
#   dim2Name <- colnames(dfBestPC)[2]
#
#   # PCA plot of best 2 PCs
#   cat("Generating optimal PCA plot......\n")
#   tiff('optimalPC.tiff', units="in", width=10, height=10, res=300)
#   pc <- ggplot(dfBestPC, aes_string(x=dim1Name, y=dim2Name))
#
#   pcScatter <- pc +
#     aes(color=`clusters`, shape=`clusters`, alpha=0.8) +
#     geom_point(size=3) +
#     guides(alpha=FALSE) +
#     scale_color_brewer(palette="Dark2") +
#     geom_rug() +
#     stat_ellipse()
#   print(pcScatter)
#   dev.off()
#   cat(sprintf('Wrote "optimalPC.tiff" to %s\n', outPath))
#
#   # 2D density estimation
#   cat("Generating optimal PCA density plot......\n")
#   tiff('optimalPCDensity.tiff', units="in", width=10, height=10, res=300)
#   pcDensity <- pc +
#     aes(shape=`clusters`, alpha=0.8) +
#     geom_point() +
#     guides(alpha=FALSE) +
#     geom_density_2d() +
#     stat_density_2d(aes(fill = ..level..), geom="polygon") +
#     scale_fill_gradient(low="blue", high="red")
#   print(pcDensity)
#   dev.off()
#   cat(sprintf('Wrote "optimalPCDensity.tiff" to %s\n', outPath))
#
#   # Variance contribution from each PC in a pie chart
#   cat("Generating variance contribution pie chart......\n")
#   tiff('varianceContribution.tiff', units="in", width=10, height=10, res=300)
#   varianceContribution <- pcObj$sdev^2/sum(pcObj$sdev^2)
#   pie3D(varianceContribution,
#         labels=percent(varianceContribution),
#         explode=0.1,
#         main="Overall variance contribution from each PC ",
#         col=rainbow(length(pc)),
#         labelcex=0.7,
#         theta=0.9)
#   legend(1.1, 1.05,
#          legend=colnames(pcData),
#          fill = rainbow(ncol(pcData)),
#          cex = 0.6,
#          xjust = 1,
#          yjust = 1)
#   dev.off()
#   cat(sprintf('Wrote "varianceContribution.tiff" to %s\n', outPath))
#
#   setwd("..")
#   return(outPath)
# }
#
# zoomPC <- function(x, y, pcData, bestPCSet, clusters, iterationResults, outPath) {
#   currDir <- getwd()
#   # 1. Given a pair of x, y coordinates that query a region on the PCA plot of the bestPCSet,
#   #    find the associated ROW in the matrix (i.e. the sample) that is closest to this region.
#
#   # do a binary search on each of the columns (PCs)
#
#   xAxis <- bestPCSet[ ,1]
#   yAxis <- bestPCSet[ ,2]
#
#   # x <- 0.5
#   # y <- -0.3
#   # --------------------- test
#
#   # # data.table inherits from data.frame. It is faster and more memory efficient.
#   # xTable <- data.table::data.table(xAxis)
#   #
#   # # sort xTable in ascending order and add an attribute "sorted"
#   # data.table::setkey(xTable, xAxis)
#   # # attributes(xTable)$sorted
#   # # [1] "xAxis"
#   #
#   # x <- 0.5
#   # # search and "roll" to nearest neighbor
#   # idxNearestX <- xTable[.(x), roll="nearest", which=TRUE] # row index of nearest neighbor
#   #
#   # valNearestX <- xTable$xAxis[idxNearestNeighborX]
#
#   # ----------------------
#
#   nearestX <- findNearestNeighbor(xAxis, x)
#   nearestY <- findNearestNeighbor(yAxis, y)
#
#   # choose sample with the least combined absolute deviation
#   # rowX <- which(bestPCSet[ ,xPC] == nearestX)
#   # xNearestY <- bestPCSet[rowX, yPC]
#   # abs(x - nearestX) + abs(y - xNearestY)
#   #
#   # rowY <- which(bestPCSet[ ,yPC] == nearestY)
#   # yNearestX <- bestPCSet[rowY, xPC]
#   # abs(x - yNearestX) + abs(y - nearestY)
#
#
#   closestSample <- chooseClosestSample(bestPCSet, x, y, nearestX, nearestY)
#
#   # row index of sample that is nearest to query point
#   rowIdx <- which(pcData[ ,closestSample$colPC] == closestSample$val)
#
#   # pcData[rowIdx, ]
#   # PC1        PC2        PC3        PC4
#   # 0.6307451  0.4149974  0.2909216 -0.2733046
#
#   # sel <- bestPCSet[rowIdx, ]
#   # PC1        PC4
#   # 0.6307451 -0.2733046
#
#
#   # ---------------------- VISUALIZATION ----------------------
#
#   dfBestPC <- data.frame(bestPCSet)
#   dfBestPC <- cbind(dfBestPC, clusters)
#
#   # encode clusters column as a factor
#   dfBestPC$clusters <- as.factor(dfBestPC$clusters)
#
#   dim1Name <- colnames(dfBestPC)[1]
#   dim2Name <- colnames(dfBestPC)[2]
#
#   # # PCA plot of best 2 PCs
#   # pc <- ggplot(dfBestPC, aes_string(x=dim1Name, y=dim2Name)) +
#   #   aes(color=`clusters`, shape=`clusters`, alpha=0.8) +
#   #   geom_point() +
#   #   guides(alpha=FALSE) +
#   #   scale_color_brewer(palette="Dark2") +
#   #   geom_rug() +
#   #   stat_ellipse()
#   #
#   # # 2D density estimation
#   # pcDensity <- ggplot(dfBestPC, aes_string(x=dim1Name, y=dim2Name)) +
#   #   aes(shape=`clusters`, alpha=0.8) +
#   #   geom_point() +
#   #   guides(alpha=FALSE) +
#   #   geom_density_2d() +
#   #   stat_density_2d(aes(fill = ..level..), geom="polygon") +
#   #   scale_fill_gradient(low="blue", high="red")
#
#   # 2. Annotate the nearest neighbor to the selected query point on the PCA plot
#   qLabel <- sprintf("QUERY: (%.2f,%.2f)", dfBestPC[rowIdx, ][1], dfBestPC[rowIdx, ][2])
#
#   setwd(outPath)
#   cat(sprintf("Generating optimal PCA plot annotated with %s......\n", qLabel))
#   tiff('optimalPCQuery.tiff', units="in", width=10, height=10, res=300)
#   print(
#     ggplot(dfBestPC, aes_string(x=dim1Name, y=dim2Name)) +
#       aes(color=`clusters`, shape=`clusters`, alpha=0.8) +
#       geom_point(size=3) +
#       guides(alpha=FALSE) +
#       scale_color_brewer(palette="Dark2") +
#       geom_rug() +
#       stat_ellipse() +
#       geom_text(data=dfBestPC[rowIdx, ], color="red", size=3, label=qLabel, alpha=1))
#   dev.off()
#   cat(sprintf('Wrote "optimalPCQuery.tiff" to %s\n', outPath))
#
#   # 3. Output an additional graphic that shows the PC breakdown
#   #    for the query point over the complete set of PCs.
#
#   # Diverging lollipop chart
#   cat(sprintf("Generating diverging lollipop chart for %s......\n", qLabel))
#   tiff('divLollipopQuery.tiff', units="in", width=10, height=10, res=300)
#   pcDf <- data.frame(PC=colnames(pcData), sample_value=round(pcData[rowIdx, ], 2))
#   print(ggplot(pcDf, aes(x=`sample_value`, y=PC, label=`sample_value`)) +
#           geom_point(stat='identity', fill="blue", size=10)  +
#           geom_segment(aes(x = 0,
#                            y = `PC`,
#                            xend = `sample_value`,
#                            yend = `PC`),
#                        color = "blue") +
#           geom_text(color="white", size=3) +
#           labs(title=sprintf("Diverging lollipop chart of PC breakdown for %s", qLabel),
#                subtitle="Value of queried sample in terms of the PCs: Lollipop"))
#   dev.off()
#   cat(sprintf('Wrote "divLollipopQuery.tiff" to %s\n', outPath))
#   setwd(currDir)
#
#   # ouput interactive graphic that highlights query point from optimized 2D plot in 3D space
#   # i.e. plot the second-last iteration from executePCFiltering; adding one component back in.
#
#   sel <- colnames(iterationResults[[(length(iterationResults) - 1)]])
#   pcData[, sel]
#
#   qLabelCoord <- sprintf("QUERY: (%.2f,%.2f,%.3f)",
#                          pcData[rowIdx,sel[1]],
#                          pcData[rowIdx,sel[2]],
#                          pcData[rowIdx,sel[3]])
#
#   queryLabel <- character(nrow(pcData))
#   queryLabel[rowIdx] <- qLabelCoord
#
#   nClusters <- max(clusters)
#   clustersQuery <- clusters
#   clustersQuery[rowIdx] <- nClusters + 1 # add another color for query point
#
#   labelSpace <- 0.02 * sum(abs(range(pcData[ ,sel[3]])))
#
#   rgl::plot3d(pcData[, sel], col=as.factor(clustersQuery), size=10, type='p')
#   rgl::text3d(x=pcData[ ,sel[1]], y=pcData[ ,sel[2]], z=pcData[ ,sel[3]] + labelSpace,
#               texts=queryLabel,
#               cex=0.7,
#               pos=3)
# }

# ====  PROCESS  ===============================================================
# Enter the step-by-step process of your project here. Strive to write your
# code so that you can simply run this entire file and re-create all
# intermediate results.

# check validateAndLoadData()
irisCSV <- system.file("extdata", "iris.csv", package = "pcclust")
data <- validateAndLoadData(irisCSV, isFile = TRUE)

pcObj <- prcomp(data)
# pcData <- runPCA(data)
pcData <- pcObj$x

# ----------------------------- manual filtering process

# iteration 1
subsets1 <- generatePCSubsets(pcData)
subsets1ClusterQuality <- lapply(subsets1, evaluateClusterQuality)
idxOptimalSubset <- determineOptimalSubset(subsets1ClusterQuality)

# TODO: check for optimal subset ambiguity ---> DONE
if (length(idxOptimalSubset) == 1) {
  sel <- subsets1[[idxOptimalSubset]] # need to store "sel" @ each iteration as input for the next iteration
}

# TODO: RECURSION ----------------------------> DONE
if (length(idxOptimalSubset) > 1) {
  # different models for maxBIC and maxLikelihood
  # need to further evaluate both subsets to determine which
  # criterion is a more stringent quantification of cluster quality.
  sel1 <- subsets1[[idxOptimalSubset[0]]]
  sel2 <- subsets1[[idxOptimalSubset[1]]]

  subsets1 <- generatePCSubsets(sel1)
  subsets2 <- PCSubsets(sel2)

}

# iteration 2
subsets2 <- generatePCSubsets(sel)
subsets2ClusterQuality <- lapply(subsets2, evaluateClusterQuality)
idxOptimalSubset2 <- determineOptimalSubset(subsets2ClusterQuality)

if (length(idxOptimalSubset2) == 1) {
  sel2 <- subsets2[[idxOptimalSubset2]] # need to store "sel" @ each iteration as input for the next iteration
}

# iteration 3
# subsets3 <- generatePCSubsets(sel2, ncol(sel2) - 1)
# subsets3ClusterQuality <- lapply(subsets3, evaluateClusterQuality)
# idxOptimalSubset3 <- determineOptimalSubset(subsets3ClusterQuality)
#
# if (length(idxOptimalSubset3) == 1) {
#   sel3 <- subsets3[[idxOptimalSubset3]]
# }

colnames(sel2)

# ----------------------------- automated filtering process

# returns list of matrices where each item in the list corresponds to the iteration
# and contains a PC matrix where  each iteration has one less PC.
iterationResults <- executePCFiltering(pcData)

bestPCSet <- iterationResults[[length(iterationResults)]]

clusterResults <- evaluateClusterQuality(bestPCSet)

# use log likelihood criterion to select optimal model for best PC set

optimalModel <- determineOptimalModel(bestPCSet)
clusters <- optimalModel$classification

# ----------------------------- visualization

# clPairs(bestPCSet, clusters)

out <- visualizePCA(bestPCSet, clusters, pcObj)

x <- 0.5
y <- -0.3

x <- 0
y <- 0

x <- 100
y <- -100

zoomPC(x, y, pcData, bestPCSet, clusters, iterationResults, out)

# ====  TESTS  =================================================================
# Enter your function tests here...


# [END]
