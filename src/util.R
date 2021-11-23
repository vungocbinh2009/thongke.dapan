#' Hàm này dùng thư viện whisker để render file
#' @import whisker
#' @import here
render_template <- function (file_name, var_list) {
  template <- readLines(system.file(file_name, package="thongke.dapan"))
  return(whisker.render(template, var_list))
}