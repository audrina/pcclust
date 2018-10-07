initBuffer <- function(n) {
  # closure function
  # following example from https://www.r-bloggers.com/closures-in-r-a-useful-abstraction/
  subsets <- vector("list", length = n)
  i <- 1

  function(sel=NULL) {
    if (is.null(sel)) {
      return(subsets)
    }
    else {
      subsets[[i]] <<- sel # scoping assignment
      i <<- i + 1
    }
  }
}
