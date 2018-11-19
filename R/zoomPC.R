#' Sample-specific PCA visualizations
#'
#' Supporting PCA visualizations and sample-specific metadata including standard 2-component PCA plot labeled with query, a diverging lollipop chart showing the PC breakdown for the query, and an interactive PCA plot showing the location of the query in 3D space.
#' @param bestPCSet a matrix whose columns contain the optimal principal components.
#' @param x approximate x coordinate value from optimal PCA plot. See details for more information.
#' @param y same as x.
#' @param pcData a validated R matrix containing numeric scaled PCA data.
#' @param clusters vector of predicted labels
#' @param iterationResults list of iteration results from PCA filtering, where each iteation has one less PC. List elements are PCA matrices. Last entry in the list corresponds to the top 2 PCs for clustering.
#' @param outPath path to ouput directory where pcclust_visualization folder was generated.
#' @return NULL. Outputs high quality .svg files in pcclust_visualization for each of the 2 plots.
#' @seealso \code{\link[ggplot2]{ggplot}}
#' @seealso \code{\link[grDevices]{svg}}
#' @seealso \code{\link[rgl]{plot3d}}
#' @seealso \code{\link[rgl]{text3d}}
#' @export
#' @examples
#' data <- validateAndLoadData(iris)
#' pcObj <- prcomp(data)
#' pcData <- pcObj$x
#' iterationResults <- executePCFiltering(pcData)
#' bestPCSet <- iterationResults[[length(iterationResults)]]
#' clusterResults <- evaluateClusterQuality(bestPCSet)
#' optimalModel <- determineOptimalModel(bestPCSet)
#' clusters <- optimalModel$classification
#' out <- visualizePCA(bestPCSet, clusters, pcObj)
#' x <- 0.5
#' y <- -0.3
#' zoomPC(x, y, pcData, bestPCSet, clusters, iterationResults, out)
zoomPC <- function(x, y, pcData, bestPCSet, clusters, iterationResults, outPath) {
  cat(sprintf("Your query point is (%.2f,%.2f)\n", x, y))

  currDir <- getwd()
  # 1. Given a pair of x, y coordinates that query a region on the PCA plot of the bestPCSet,
  #    find the associated ROW in the matrix (i.e. the sample) that is closest to this region.

  # do a binary search on each of the columns (PCs)

  xAxis <- bestPCSet[ ,1]
  yAxis <- bestPCSet[ ,2]

  nearestX <- findNearestNeighbor(xAxis, x)
  nearestY <- findNearestNeighbor(yAxis, y)

  # choose sample with the least combined absolute deviation from query

  closestSample <- chooseClosestSample(bestPCSet, x, y, nearestX, nearestY)

  # row index of sample that is nearest to query point
  rowIdx <- which(pcData[ ,closestSample$colPC] == closestSample$val)

  # ---------------------- VISUALIZATION ----------------------

  dfBestPC <- data.frame(bestPCSet)
  dfBestPC <- cbind(dfBestPC, clusters)

  # encode clusters column as a factor
  dfBestPC$clusters <- as.factor(dfBestPC$clusters)

  dim1Name <- colnames(dfBestPC)[1]
  dim2Name <- colnames(dfBestPC)[2]

  # 2. Annotate the nearest neighbor to the selected query point on the PCA plot
  qLabel <- sprintf("QUERY: (%.2f,%.2f)", dfBestPC[rowIdx, ][1], dfBestPC[rowIdx, ][2])

  cat(sprintf("The nearest neighbor to your query is %s\n", qLabel))

  setwd(outPath)
  cat(sprintf("Generating optimal PCA plot annotated with %s......\n", qLabel))
  grDevices::svg('optimalPCQuery.svg', width=10, height=10)
  print(
    ggplot(dfBestPC, aes_string(x=dim1Name, y=dim2Name)) +
      aes(color=`clusters`, shape=`clusters`, alpha=0.8) +
      geom_point(size=3) +
      guides(alpha=FALSE) +
      scale_color_brewer(palette="Dark2") +
      geom_rug() +
      stat_ellipse() +
      geom_text(data=dfBestPC[rowIdx, ], color="red", size=3, label=qLabel, alpha=1))
  grDevices::dev.off()
  cat(sprintf('Wrote "optimalPCQuery.svg" to %s\n', outPath))

  # 3. Output an additional graphic that shows the PC breakdown
  #    for the query point over the complete set of PCs.

  # Diverging lollipop chart
  cat(sprintf("Generating diverging lollipop chart for %s......\n", qLabel))
  grDevices::svg('divLollipopQuery.svg', width=10, height=10)
  pcDf <- data.frame(PC=colnames(pcData), sample_value=round(pcData[rowIdx, ], 2))
  print(ggplot(pcDf, aes(x=`sample_value`, y=PC, label=`sample_value`)) +
          geom_point(stat='identity', fill="blue", size=10)  +
          geom_segment(aes(x = 0,
                           y = `PC`,
                           xend = `sample_value`,
                           yend = `PC`),
                       color = "blue") +
          geom_text(color="white", size=3) +
          labs(title=sprintf("Diverging lollipop chart of PC breakdown for %s", qLabel),
               subtitle="Value of queried sample in terms of the PCs: Lollipop"))
  grDevices::dev.off()
  cat(sprintf('Wrote "divLollipopQuery.svg" to %s\n', outPath))
  setwd(currDir)

  # ouput interactive graphic that highlights query point from optimized 2D plot in 3D space
  # i.e. plot the second-last iteration from executePCFiltering; adding one component back in.

  cat("Generating interactive 3D PCA plot\n")

  sel <- colnames(iterationResults[[(length(iterationResults) - 1)]])
  pcData[, sel]

  qLabelCoord <- sprintf("QUERY: (%.2f,%.2f,%.3f)",
                         pcData[rowIdx,sel[1]],
                         pcData[rowIdx,sel[2]],
                         pcData[rowIdx,sel[3]])

  queryLabel <- character(nrow(pcData))
  queryLabel[rowIdx] <- qLabelCoord

  nClusters <- max(clusters)
  clustersQuery <- clusters
  clustersQuery[rowIdx] <- nClusters + 1 # add another color for query point

  labelSpace <- 0.02 * sum(abs(range(pcData[ ,sel[3]])))

  rgl::plot3d(pcData[, sel], col=as.factor(clustersQuery), size=10, type='p')
  rgl::text3d(x=pcData[ ,sel[1]], y=pcData[ ,sel[2]], z=pcData[ ,sel[3]] + labelSpace,
              texts=queryLabel,
              cex=0.7,
              pos=3)

  cat("Visualizations complete!\n")
}
