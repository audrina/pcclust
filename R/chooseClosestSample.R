chooseClosestSample <- function(bestPCSet, x, y, nearestX, nearestY) {
  # choose sample corresponding to the nearest neighbor for the PC
  # that has the least combined absolute deviation from the query.

  xPC <- colnames(bestPCSet)[1]
  yPC <- colnames(bestPCSet)[2]

  rowX <- which(bestPCSet[ ,xPC] == nearestX)
  xNearestY <- bestPCSet[rowX, yPC]
  errX <- abs(x - nearestX) + abs(y - xNearestY)

  rowY <- which(bestPCSet[ ,yPC] == nearestY)
  yNearestX <- bestPCSet[rowY, xPC]
  errY <- abs(x - yNearestX) + abs(y - nearestY)

  dimLeastErr <- which(c(errX, errY) == min(c(errX, errY)))

  if (dimLeastErr == 1) {
    # nearest neighbor for xPC is closest to the query (x, y)
    nearestChoice <- nearestX
  }

  else {
    # nearest neighbor for yPC is closest to the query (x, y)
    nearestChoice <- nearestY
  }

  return(list(colPC = colnames(bestPCSet)[dimLeastErr], val = nearestChoice)) # [<PC name>, <value nearest neighbor>]
}
