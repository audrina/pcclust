#' Generate PCA plots with optimized clustering
#'
#' Baseline PCA visualizations including standard 2-component PCA plot, density plot, and variance contribution pie chart.
#' @param bestPCSet a matrix whose columns contain the optimal principal components.
#' @param clusters vector of predicted labels
#' @param pcObj a PCA object
#' @param outDir path to ouput directory where pcclust_visualization folder will be generated. Defaults to current working directory.
#' @return path to pcclust_visualization output directory. Outputs high quality .tiff files for each of the 3 plots.
#' @seealso \code{\link[ggplot2]{ggplot}}
#' @seealso \code{\link[grDevices]{tiff}}
#' @importFrom grDevices dev.off rainbow tiff
#' @importFrom graphics legend
#' @import ggplot2
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
visualizePCA <- function(bestPCSet, clusters, pcObj, outDir = ".") {
  outPath <- file.path(outDir, "pcclust_visualization")
  if (!dir.exists(outPath)) {
    cmd <- sprintf("mkdir %s", outPath)
    system(cmd)
  }
  setwd(outPath)

  # output high quality .tiff PCA plots corresponding to the optimal set of PCs

  dfBestPC <- data.frame(bestPCSet)
  dfBestPC <- cbind(dfBestPC, clusters)

  # encode clusters column as a factor
  dfBestPC$clusters <- as.factor(dfBestPC$clusters)

  dim1Name <- colnames(dfBestPC)[1]
  dim2Name <- colnames(dfBestPC)[2]

  # PCA plot of best 2 PCs
  cat("Generating optimal PCA plot......\n")
  grDevices::tiff('optimalPC.tiff', units="in", width=10, height=10, res=300)
  pc <- ggplot(dfBestPC, aes_string(x=dim1Name, y=dim2Name))

  pcScatter <- pc +
    aes(color=`clusters`, shape=`clusters`, alpha=0.8) +
    geom_point(size=3) +
    guides(alpha=FALSE) +
    scale_color_brewer(palette="Dark2") +
    geom_rug() +
    stat_ellipse()
  print(pcScatter)
  grDevices::dev.off()
  cat(sprintf('Wrote "optimalPC.tiff" to %s\n', outPath))

  # 2D density estimation
  cat("Generating optimal PCA density plot......\n")
  grDevices::tiff('optimalPCDensity.tiff', units="in", width=10, height=10, res=300)
  pcDensity <- pc +
    aes(shape=`clusters`, alpha=0.8) +
    geom_point() +
    guides(alpha=FALSE) +
    geom_density_2d() +
    stat_density_2d(aes(fill = ..level..), geom="polygon") +
    scale_fill_gradient(low="blue", high="red")
  print(pcDensity)
  grDevices::dev.off()
  cat(sprintf('Wrote "optimalPCDensity.tiff" to %s\n', outPath))

  # Variance contribution from each PC in a pie chart
  cat("Generating variance contribution pie chart......\n")
  grDevices::tiff('varianceContribution.tiff', units="in", width=10, height=10, res=300)
  varianceContribution <- pcObj$sdev^2/sum(pcObj$sdev^2)
  plotrix::pie3D(varianceContribution,
        labels=scales::percent(varianceContribution),
        explode=0.1,
        main="Overall variance contribution from each PC ",
        col=rainbow(length(pc)),
        labelcex=0.7,
        theta=0.9)
  graphics::legend(1.1, 1.05,
         legend=colnames(pcObj$x),
         fill = rainbow(ncol(pcObj$x)),
         cex = 0.6,
         xjust = 1,
         yjust = 1)
  grDevices::dev.off()
  cat(sprintf('Wrote "varianceContribution.tiff" to %s\n', outPath))

  setwd("..")
  return(outPath)
}
