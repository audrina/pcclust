#' Validate and load data
#'
#' Validate input type and load data into data frame.
#' @param data a numerical R matrix or data frame.
#' @param isFile if TRUE, data accepts a path to a file containing numeric multidimensional data. FALSE by default.
#' @param scaled if TRUE, use Z-score standardization so that outliers aren"t weighted less. TRUE by default.
#' @details Supported input types:
#' \itemize{
#'   \item R numerical matrix or data frame
#'   \item a wide range of file formats (refer to https://github.com/leeper/rio for a full list of supported formats)
#' }
#' @return Centered scaled R matrix with loaded data.
#' @seealso \code{\link[rio]{import}}
#' @export
#' @examples
#' irisCSV <- system.file("extdata", "iris.csv", package = "pcclust")
#' data <- validateAndLoadData(irisCSV, isFile = TRUE)
validateAndLoadData <- function(data, isFile = FALSE, scaled = TRUE) {
  # Purpose:
  #     Validate input type and load data into data frame.
  #     Supported input types:
  #     - R numerical matrix or data frame
  #     - a wide range of file formats (refer to https://github.com/leeper/rio for a full
  #                                     list of supported formats)
  # Parameters:
  #     data      a numerical R matrix or data frame.
  #     isFile    if TRUE, data accepts a path to a file containing numeric multidimensional data.
  #               FALSE by default.
  #     scaled    if TRUE, use Z-score standardization so that outliers aren"t weighted less.
  #               TRUE by default.
  # Value:
  #     Centered scaled R matrix with loaded data.

  # 1. load data into data frame

  if (isFile == TRUE) {
    # rio automated import chooses appropriate function based on file extension
    df <- rio::import(data)
    if (colnames(df)[1] == "V1") {
      # remove additional row index column
      df <- df[-1]
    }
  }

  else if (is.matrix(data)) {
    df <- as.data.frame(data)
  }

  else {
    # data is already a data frame
    df <- data
  }

  # 2. validate that data type is purely numeric

  # if labels are included, separate from the numeric data
  numCol <- sapply(df, is.numeric)
  labelsIdx <- which(numCol == FALSE) # column index

  if (length(labelsIdx) > 1) {
    stop("Invalid dataset: contains more than 1 column of non-numeric data")
  }

  else if (length(labelsIdx) == 1) {
    # labels <- data[labelsIdx]
    df <- df[-labelsIdx]
  }

  valid <- all(sapply(df, is.numeric))
  if (! valid) {
    stop("Invalid dataset: input data must be purely numeric!")
  }

  if (scaled == TRUE) {
    scaledMatrix <- scale(df)
  }

  cat(sprintf("Validation successful: loaded %s into a scaled matrix\n", data))
  return(scaledMatrix)
}
