#' Hàm này dùng thư viện whisker để render file
#' @import whisker
#' @import here
render_template <- function (file_name, var_list) {
  template <- readLines(paste0(system.file(package = "thongke.dapan"), file_name))
  return(whisker.render(template, var_list))
}