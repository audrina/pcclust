#test_validateAndLoadData.R

context("Validate and load")

# ==== BEGIN SETUP AND PREPARE =================================================
#
# test assests created with:

# irisCSV <- system.file("extdata", "iris.csv", package = "pcclust") # regular csv file
# dfNumeric <- data.frame(x=1:20, y=30:49) # valid df
# dfNonNumeric <- data.frame(x=1:20, y=30:49, a=rep("a",20), b=rep("b",20)) # invalid df: more than 1 column labels
# matrixNumeric <- matrix(c(1:20), 10) # valid
# matrixString<- matrix(c(c(1:20), rep("a",10)), 10) # invalid
# matrixNonNumeric <- matrix(c(c(1:20), rep("a",10), rep("b",10)), 10) # invalid

# save(irisCSV, dfNumeric, dfNonNumeric,
#      matrixNumeric, matrixNonNumeric, matrixString,
#      file = "./inst/extdata/testdata/validateAndLoad.Rdata")
load(system.file("extdata/testdata", "validateAndLoad.Rdata", package = "pcclust"))
# gives you local location of file after it's been installed in the package --> so don't need relative addressing

# ==== END SETUP AND PREPARE ===================================================

test_that("invalid input type generates errors", {
  expect_error(validateAndLoadData(dfNonNumeric), "Invalid dataset: contains more than 1 column of non-numeric data")
  expect_error(validateAndLoadData(matrixNonNumeric), "Invalid dataset: contains more than 1 column of non-numeric data")
  expect_error(validateAndLoadData(matrixString), "Invalid dataset: contains more than 1 column of non-numeric data")
  expect_error(validateAndLoadData(), "argument \"data\" is missing, with no default")
})

test_that("output is a matrix", {
  expect_equal(is.matrix(validateAndLoadData(dfNumeric)), TRUE)
  expect_equal(is.matrix(validateAndLoadData(matrixNumeric)), TRUE)
})


# ==== BEGIN TEARDOWN AND RESTORE ==============================================
# Remove every persitent construct that the test has created, except for
# stuff in tempdir().

rm(irisCSV, dfNumeric, dfNonNumeric, matrixNumeric, matrixNonNumeric, matrixString)
#
# ==== END  TEARDOWN AND RESTORE ===============================================

# [END]
