# Storing + using test artifacts for validation
# want to compare results @ run time w/ the results that you got from a confirmed "good" state

tmp <- c(1,0,
         0.000004543)

# maybe have a bunch of test numbers or data --> want to store in RData file
# so that you can compare output results later with what you get at test time.

# write + read reassigns
# save + load recreates


save(list = "tmp", file = "./inst/extdata/testdata/testAssets.Rdata")
rm(tmp)
load("./inst/extdata/testdata/testAssets.Rdata") # rewrites any existing previous state


# 1. create test artifacts
# 2. create inst/extdata/testdata
# 3. save("./......./<testname>.Rdata") --> put under version control
# 4. load(system.file(...)) to recreate objects in test and you can remove them later
