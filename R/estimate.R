#' Hàm này in ra đáp án cho bài toán ước lượng giá trị trung bình (dùng phân bố chuẩn)
#' @import thongke
#' @export
answer_estimate_mean_norm <- function(data, sd_symbol, conclusion, score=c(0.5, 1.5), round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "estimate_mean_norm.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    two_side = params$mode == "two.side",
    min = params$mode == "min",
    max = params$mode == "max",
    mean = round(params$mean, round_digits),
    n = params$n,
    s = round(params$sigma, round_digits),
    sd_symbol = sd_symbol,
    conclusion = conclusion,
    alpha = params$alpha,
    z_alpha = round(result$z_alpha, round_digits),
    z_alpha_2 = round(result$z_alpha_2, round_digits),
    bottom = round(result$bottom, round_digits),
    top = round(result$top, round_digits),
    min_value = round(result$min, round_digits),
    max_value = round(result$max, round_digits),
    score_1 = score[1],
    score_2 = score[2]
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán ước lượng giá trị trung bình (dùng phân bố Student)
#' @import thongke
#' @export
answer_estimate_mean_t <- function(data, conclusion, score=c(0.5, 1.5), round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "estimate_mean_t.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    two_side = params$mode == "two.side",
    min = params$mode == "min",
    max = params$mode == "max",
    mean = round(params$mean, round_digits),
    n = params$n,
    conclusion = conclusion,
    s = round(params$s, round_digits),
    alpha = params$alpha,
    t_alpha = round(result$t_alpha, round_digits),
    t_alpha_2 = round(result$t_alpha_2, round_digits),
    bottom = round(result$bottom, round_digits),
    top = round(result$top, round_digits),
    min_value = round(result$min, round_digits),
    max_value = round(result$max, round_digits),
    score_1 = score[1],
    score_2 = score[2]
  )
  render_template(file_name, var_list)
}

#'Hàm này in ra đáp án cho bài toán ước lượng phương sai
#' @import thongke
#' @export
answer_estimate_var <- function(data, conclusion, score=c(0.5, 1.5), round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "estimate_var.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    two_side = params$mode == "two.side",
    min = params$mode == "min",
    max = params$mode == "max",
    n = params$n,
    s = round(params$s, round_digits),
    alpha = params$alpha,
    conclusion = conclusion,
    chi_sq_1 = round(result$chi_sq_1, round_digits),
    chi_sq_2 = round(result$chi_sq_2, round_digits),
    chi_sq_1_2 = round(result$chi_sq_1_2, round_digits),
    chi_sq_2_2 = round(result$chi_sq_2_2, round_digits),
    n_1 = params$n - 1,
    s2 = round(params$s * params$s, round_digits),
    bottom = round(result$bottom, round_digits),
    top = round(result$top, round_digits),
    min_value = round(result$min, round_digits),
    max_value = round(result$max, round_digits),
    score_1 = score[1],
    score_2 = score[2]
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán ước lượng cho tỷ lệ
#' @import thongke
#' @export
answer_estimate_prop <- function(data, conclusion, score=c(0.5, 1.5), round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "estimate_prop.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    two_side = params$mode == "two.side",
    min = params$mode == "min",
    max = params$mode == "max",
    n = params$n,
    f = round(params$f, round_digits),
    alpha = params$alpha,
    z_alpha = round(result$z_alpha, round_digits),
    z_alpha_2 = round(result$z_alpha_2, round_digits),
    conclusion = conclusion,
    g = round(1 - params$f, round_digits),
    bottom = round(result$bottom, round_digits),
    top = round(result$top, round_digits),
    min_value = round(result$min, round_digits),
    max_value = round(result$max, round_digits),
    score_1 = score[1],
    score_2 = score[2]
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán xác định kích thước mẫu (với giá trị trung bình)
#' @import thongke
#' @export
answer_sample_size_mean <- function(data, sd_symbol, score=c(0.5, 1, 0.5), round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "sample_size_mean.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    sd_symbol = sd_symbol,
    eps = params$eps,
    alpha = params$alpha,
    s = round(params$sigma, round_digits),
    z_alpha = round(result$z_alpha, round_digits),
    value = round(result$value, round_digits),
    score_1 = score[1],
    score_2 = score[2],
    score_3 = score[3]
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán xác định kích thước mẫu (với tỷ lệ, khi đã biết f)
#' @import thongke
#' @export
answer_sample_size_prop_1 <- function(data, score=c(0.5, 1, 0.5), round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "sample_size_prop_1.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    f = round(params$f, round_digits),
    eps = params$eps,
    alpha = params$alpha,
    z_alpha = round(result$z_alpha, round_digits),
    g = round(1 - params$f, round_digits),
    value = round(result$value, round_digits),
    score_1 = score[1],
    score_2 = score[2],
    score_3 = score[3]
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán xác định kích thước mẫu (với tỷ lệ, khi đã biết f)
#' @import thongke
#' @export
answer_sample_size_prop_2 <- function(data, score=c(0.5, 1, 0.5), round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "sample_size_prop_2.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    eps = params$eps,
    alpha = params$alpha,
    z_alpha = round(result$z_alpha, round_digits),
    value = round(result$value, round_digits),
    score_1 = score[1],
    score_2 = score[2],
    score_3 = score[3]
  )
  render_template(file_name, var_list)
}
