library(devtools)
library(here)

merge_files <- function(file_list, output) {
  output_file <- NULL
  for(file in file_list) {
    output_file <- c(output_file, readLines(file))
    output_file <- c(output_file, "\n")
  }
  writeLines(output_file, output)
}

### BEGIN SCRIPT
file_list <- c(
  here("src", "estimate.R"),
  here("src", "hypothesis_test.R"),
  here("src", "regression.R"),
  here("src", "util.R")
)

merge_files(file_list, here("R", "thongke_dapan.R"))

build(
  pkg = ".",
  path = "./build",
  binary = FALSE,
  vignettes = TRUE,
  manual = FALSE,
  args = NULL,
  quiet = FALSE,
)

document(pkg = ".", roclets = NULL, quiet = FALSE)

### END SCRIPT