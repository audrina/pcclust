#generatePCSubsets.R
#evaluateClusterQuality.R
#executePCFiltering

context("Generate PC Subsets and evaluate clusters")

# ==== BEGIN SETUP AND PREPARE =================================================
#
# test assests created with:

# irisCSV <- system.file("extdata", "iris.csv", package = "pcclust")
# data <- validateAndLoadData(irisCSV, isFile = TRUE)
# pcObj <- prcomp(data)
# pcData <- pcObj$x

# save(pcData, file = "./inst/extdata/testdata/generatePCSubsets.Rdata")
load(system.file("extdata/testdata", "generatePCSubsets.Rdata", package = "pcclust"))

# ==== END SETUP AND PREPARE ===================================================

test_that("number of subsets is correct", {
  expect_equal(length(generatePCSubsets(pcData)), 3)
  expect_equal(is.list(generatePCSubsets(pcData)), TRUE)
})

test_that("invalid input type generates errors", {
  expect_error(generatePCSubsets(), "argument \"pcData\" is missing, with no default")
  expect_error(evaluateClusterQuality(), "argument \"pcMatrix\" is missing, with no default")
})

test_that("cluster quality evaluation returns list", {
  expect_equal(is.list(evaluateClusterQuality(pcData)), TRUE)
})

test_that("PC filtering is correct", {
  expect_equal(is.list(executePCFiltering(pcData)), TRUE)
  expect_equal(length(executePCFiltering(pcData)), ncol(pcData) - 2)
  expect_error(executePCFiltering(), "argument \"pcData\" is missing, with no default")
})

test_that("optimal model evaluation returns list", {
  expect_equal(is.list(determineOptimalModel(pcData)), TRUE)
})

# ==== BEGIN TEARDOWN AND RESTORE ==============================================
# Remove every persitent construct that the test has created, except for
# stuff in tempdir().

rm(pcData)
#
# ==== END  TEARDOWN AND RESTORE ===============================================

# [END]
