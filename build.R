library(devtools)
library(here)
library(magrittr)

merge_files <- function(file_list, output) {
  output_file <- NULL
  for(file in file_list) {
    output_file <- c(output_file, readLines(file))
    output_file <- c(output_file, "\n")
  }
  writeLines(output_file, output)
}

### BEGIN SCRIPT
# Thực hiện các bước: Tìm folder src -> lấy danh sách các
# file trong folder -> lấy path các file.
file_list <- here("src") %>% list.files() %>% here("src", .)

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