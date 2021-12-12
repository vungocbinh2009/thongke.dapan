#' Hàm này in ra đáp án cho bài toán ước lượng giá trị trung bình (dùng phân bố chuẩn)
#' @import thongke
#' @export
answer_estimate_mean_norm <- function(data) {
  file_name <- get_file_path("template", "estimate", "estimate_mean_norm.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    mean = round(params$mean, 4),
    n = params$n,
    s = round(params$sigma, 4),
    alpha = params$alpha,
    z_alpha = round(result$z_alpha, 4),
    bottom = round(result$bottom, 4),
    top = round(result$top, 4)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán ước lượng giá trị trung bình (dùng phân bố Student)
#' @import thongke
#' @export
answer_estimate_mean_t <- function(data) {
  file_name <- get_file_path("template", "estimate", "estimate_mean_t.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    mean = round(params$mean, 4),
    n = params$n,
    s = round(params$s, 4),
    alpha = params$alpha,
    t_alpha = round(result$t_alpha, 4),
    bottom = round(result$bottom, 4),
    top = round(result$top, 4)
  )
  render_template(file_name, var_list)
}

#'Hàm này in ra đáp án cho bài toán ước lượng phương sai
#' @import thongke
#' @export
answer_estimate_var <- function(data) {
  file_name <- get_file_path("template", "estimate", "estimate_var.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    n = params$n,
    s = round(params$s, 4),
    alpha = params$alpha,
    chi_sq_1 = round(result$chi_sq_1, 4),
    chi_sq_2 = round(result$chi_sq_2, 4),
    n_1 = params$n - 1,
    s2 = round(params$s * params$s, 4),
    bottom = round(result$bottom, 4),
    top = round(result$top, 4)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán ước lượng cho tỷ lệ
#' @import thongke
#' @export
answer_estimate_prop <- function(data) {
  file_name <- get_file_path("template", "estimate", "estimate_prop.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    n = params$n,
    f = round(params$f, 4),
    alpha = params$alpha,
    z_alpha = round(result$z_alpha, 4),
    g = round(1 - params$f, 4),
    bottom = round(result$bottom, 4),
    top = round(result$top, 4)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán xác định kích thước mẫu (với giá trị trung bình)
#' @import thongke
#' @export
answer_sample_size_mean <- function(data) {
  file_name <- get_file_path("template", "estimate", "sample_size_mean.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    eps = params$eps,
    alpha = params$alpha,
    s = round(params$sigma, 4),
    z_alpha = round(result$z_alpha, 4),
    value = round(result$value, 4)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán xác định kích thước mẫu (với tỷ lệ, khi đã biết f)
#' @import thongke
#' @export
answer_sample_size_prop_1 <- function(data) {
  file_name <- get_file_path("template", "estimate", "sample_size_prop_1.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    f = round(params$f, 4),
    eps = params$eps,
    alpha = params$alpha,
    z_alpha = round(result$z_alpha, 4),
    g = round(1 - params$f, 4),
    value = round(result$value, 4)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán xác định kích thước mẫu (với tỷ lệ, khi đã biết f)
#' @import thongke
#' @export
answer_sample_size_prop_2 <- function(data) {
  file_name <- get_file_path("template", "estimate", "sample_size_prop_2.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    eps = params$eps,
    alpha = params$alpha,
    z_alpha = round(result$z_alpha, 4),
    value = round(result$value, 4)
  )
  render_template(file_name, var_list)
}
