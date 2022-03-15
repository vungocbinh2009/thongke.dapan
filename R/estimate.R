#' Trình bày bài toán ước lượng khoảng cho trung bình (phân bố chuẩn)
#'
#' Hàm này in ra đáp án cho bài toán ước lượng giá trị trung bình (dùng phân bố chuẩn)
#' answer là lời giải của bài toán.
#' @import thongke
#' @export
answer_estimate_mean_norm <- function(data, sd_symbol, answer, score=c(0.5, 1.5), round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "estimate_mean_norm.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    two_side = params$mode == "two.side" | params$mode == "two_side",
    min = params$mode == "min",
    max = params$mode == "max",
    mean = round(params$mean, round_digits),
    n = params$n,
    s = round(params$sigma, round_digits),
    sd_symbol = sd_symbol,
    answer = answer,
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

#' Trình bày bài toán ước lượng khoảng cho trung bình (phân bố Student)
#'
#' Hàm này in ra đáp án cho bài toán ước lượng giá trị trung bình (dùng phân bố Student)
#' answer là lời giải của bài toán.
#' @import thongke
#' @export
answer_estimate_mean_t <- function(data, answer, score=c(0.5, 1.5), round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "estimate_mean_t.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    two_side = params$mode == "two.side" | params$mode == "two_side",
    min = params$mode == "min",
    max = params$mode == "max",
    mean = round(params$mean, round_digits),
    n = params$n,
    answer = answer,
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

#' Trình bày bài toán ước lượng khoảng cho phương sai.
#'
#' Hàm này in ra đáp án cho bài toán ước lượng phương sai
#' answer là lời giải của bài toán.
#' @import thongke
#' @export
answer_estimate_var <- function(data, answer, score=c(0.5, 1.5), round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "estimate_var.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    two_side = params$mode == "two.side" | params$mode == "two_side",
    min = params$mode == "min",
    max = params$mode == "max",
    n = params$n,
    s = round(params$s, round_digits),
    alpha = params$alpha,
    answer = answer,
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

#' Trình bày bài toán ước lượng cho tỷ lệ.
#'
#' Hàm này in ra đáp án cho bài toán ước lượng cho tỷ lệ
#' answer là lời giải của bài toán.
#' @import thongke
#' @export
answer_estimate_prop <- function(data, answer, score=c(0.5, 1.5), round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "estimate_prop.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    two_side = params$mode == "two.side" | params$mode == "two_side",
    min = params$mode == "min",
    max = params$mode == "max",
    n = params$n,
    f = round(params$f, round_digits),
    alpha = params$alpha,
    z_alpha = round(result$z_alpha, round_digits),
    z_alpha_2 = round(result$z_alpha_2, round_digits),
    answer = answer,
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

#' Trình bày bài toán xác định kích thước mẫu (TH cho giá trị trung bình)
#'
#' Hàm này in ra đáp án cho bài toán xác định kích thước mẫu (với giá trị trung bình)
#' conclusion là một hàm in ra kết quả cuối cùng của bài toán, với 1 tham số chính là kết quả của phép tính.
#' @import thongke
#' @export
answer_sample_size_mean <- function(data, sd_symbol, conclusion = function (value) {return(sprintf("%d", value))}, score=c(0.5, 1, 0.5), round_digits = 4) {
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
    conclusion = conclusion(result$value),
    score_1 = score[1],
    score_2 = score[2],
    score_3 = score[3]
  )
  render_template(file_name, var_list)
}

#' Trình bày bài toán xác định kích thước mẫu (cho tỷ lệ, đã biết f)
#'
#' Hàm này in ra đáp án cho bài toán xác định kích thước mẫu (với tỷ lệ, khi đã biết f)
#' conclusion là một hàm in ra kết quả cuối cùng của bài toán, với 1 tham số chính là kết quả của phép tính.
#' @import thongke
#' @export
answer_sample_size_prop_1 <- function(data, score=c(0.5, 1, 0.5), conclusion = function (value) {return(sprintf("%d", value))}, round_digits = 4) {
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
    conclusion = conclusion(result$value),
    score_1 = score[1],
    score_2 = score[2],
    score_3 = score[3]
  )
  render_template(file_name, var_list)
}

#' Trình bày bài toán xác định kích thước mẫu (cho tỷ lệ, không biết f)
#'
#' Hàm này in ra đáp án cho bài toán xác định kích thước mẫu (với tỷ lệ, khi chưa biết f)
#' conclusion là một hàm in ra kết quả cuối cùng của bài toán, với 1 tham số chính là kết quả của phép tính.
#' @import thongke
#' @export
answer_sample_size_prop_2 <- function(data, score=c(0.5, 1, 0.5), conclusion = function (value) {return(sprintf("%d", value))}, round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "sample_size_prop_2.mustache")
  params <- data$params
  result <- data$result
  var_list <- list(
    eps = params$eps,
    alpha = params$alpha,
    z_alpha = round(result$z_alpha, round_digits),
    value = round(result$value, round_digits),
    conclusion = conclusion(result$value),
    score_1 = score[1],
    score_2 = score[2],
    score_3 = score[3]
  )
  render_template(file_name, var_list)
}
