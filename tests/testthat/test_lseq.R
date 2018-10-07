#test_lseq.R

context("lseq")

# ==== BEGIN SETUP AND PREPARE =================================================
#

# test assests created with:
tmp <- c(  1.0
         , 1.778279410038922980775
         , 3.162277660168379522787
         , 5.623413251903492060535
         , 10.0)

# load("testLseqFiveNumbers")
# load(system.file("extdata/testdata", "testAssets.Rdata", package = "pcclust"))
# gives you local location of file after it's been installed in the package --> so don't need relative addressing

# ==== END SETUP AND PREPARE ===================================================

test_that("corrupt input generates errors",  {
  # expect_error(lseq(), "argument \"x\" is missing, with no default")
  expect_error(lseq(0,10), "'from' must be a finite number")
  expect_error(lseq(10,0), "'to' must be a finite number")
})

test_that("a sample input prouces the expected output",  {
  expect_equal(lseq(1,10, length.out = 5), tmp)
})

# test_that("a sample input prouces the expected output",  {
#   expect_equal(lseq(1,10, length.out = 5), testLseqFiveNumbers)
# })



# ==== BEGIN TEARDOWN AND RESTORE ==============================================
# Remove every persitent construct that the test has created, except for
# stuff in tempdir().

# rm(testLseqFiveNumbers)
#
# ==== END  TEARDOWN AND RESTORE ===============================================

# [END]
