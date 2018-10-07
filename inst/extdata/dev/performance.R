# performance.R

# scenario:
# function to split a string into codons
# input e.g. "GCATTTCGGCTGAA"
# OUTPUT: "GCA", "ATT", "TCG", GCT", "GAA"

codons_1 <- function(x) {
  l <- nchar(x)
  first <- seq(l, l, by=3)
  last <- first + 2
  return(substring(x, first, last))
}

codons_2 <- function(x) {
  return(unlist(strsplit(x, "(?<=...)", perl = TRUE))) # look behind
}

library(stringi)

codons_3 <- function(x) {
  return(unlist(stri_extract_all_regex(x, "...")))
}


# speed, library loading, memory


# ================
x <- paste(sample(c("A","C","G","T"), 9999, replace = TRUE), collapse = "")

# ================
# sys.time() measures num seconds elapsed since unix epoch

system.time({output <- codons_1(x)})

install.packages("microbenchmark")
library(microbenchmark)
mb <- microbenchmark(output <- codons_3(x),
                     times = 1000)
autoplot(mb)

