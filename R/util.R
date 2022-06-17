#' Hàm này dùng thư viện whisker để render file
#' @import whisker
render_template <- function (file_name, var_list) {
  template <- readLines(file_name)
  return(whisker.render(template, var_list))
}

#' Hàm này lấy đường dẫn các file trong package
get_file_path <- function (...) {
  return(file.path(system.file(package = "thongke.dapan"), ...))
}

#' Hàm này giúp việc trả về 1 list đơn giản hơn
return_list <- function (...) {
  return(list(...))
}
