library(devtools)
library(here)
library(magrittr)

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