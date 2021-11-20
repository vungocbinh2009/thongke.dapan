#' Hàm này dùng thư viện whisker để render file
#' @import whisker
render_template <- function (file_name, var_list) {
  template <- readLines(file_name)
  return(whisker.render(template, var_list))
}