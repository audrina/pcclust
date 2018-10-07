#' findNearestNeighbor
#'
#' @param pcCol a column from a optimal PC subset
#' @param queryPt a number
#' @import data.table
#' @return value from complete PC matrix closest to query
findNearestNeighbor <- function(pcCol, queryPt) {
  # data.table inherits from data.frame. It is faster and more memory efficient.
  dt <- data.table::data.table(pcCol)

  # sort xTable in ascending order and add an attribute "sorted"
  data.table::setkey(dt, pcCol)

  # search and "roll" to nearest neighbor
  idxNearest <- dt[.(queryPt), roll="nearest", which=TRUE] # row index of nearest neighbor
  valNearest <- dt$pcCol[idxNearest]

  return(valNearest)
}
