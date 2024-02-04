#' Trình bày bài toán ước lượng khoảng cho trung bình (phân bố chuẩn)
#'
#' Hàm này in ra đáp án cho bài toán ước lượng giá trị trung bình (dùng phân bố chuẩn)
#' answer là lời giải của bài toán.
#' @import thongke
#' @export
answer_estimate_mean_norm <- function(data, sd_symbol = "\\sigma", answer = "Khoảng tin cậy cần tìm là:", score=c(0.5, 1.5), round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "estimate_mean_norm.mustache")
  input_data <- data$input_data
  output_data <- data$output_data
  var_list <- list(
    two_side = input_data$alternative == "two_sided",
    min = input_data$alternative == "min",
    max = input_data$alternative == "max",
    mean = round(input_data$mean, round_digits),
    n = input_data$n,
    s = round(input_data$sigma, round_digits),
    sd_symbol = sd_symbol,
    answer = answer,
    alpha = input_data$alpha,
    z_alpha = round(output_data$z_alpha, round_digits),
    z_alpha_div_2 = round(output_data$z_alpha_div_2, round_digits),
    bottom = round(output_data$bottom, round_digits),
    top = round(output_data$top, round_digits),
    min_value = round(output_data$min, round_digits),
    max_value = round(output_data$max, round_digits),
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
answer_estimate_mean_t <- function(data, answer = "Khoảng tin cậy cần tìm là:", score=c(0.5, 1.5), round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "estimate_mean_t.mustache")
  input_data <- data$input_data
  output_data <- data$output_data
  var_list <- list(
    two_side = input_data$alternative == "two_sided",
    min = input_data$alternative == "min",
    max = input_data$alternative == "max",
    mean = round(input_data$mean, round_digits),
    n = input_data$n,
    answer = answer,
    s = round(input_data$s, round_digits),
    alpha = input_data$alpha,
    t_alpha = round(output_data$t_alpha, round_digits),
    t_alpha_2 = round(output_data$t_alpha_div_2, round_digits),
    bottom = round(output_data$bottom, round_digits),
    top = round(output_data$top, round_digits),
    min_value = round(output_data$min, round_digits),
    max_value = round(output_data$max, round_digits),
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
answer_estimate_var <- function(data, answer = "Khoảng tin cậy cần tìm là:", score=c(0.5, 1.5), round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "estimate_var.mustache")
  input_data <- data$input_data
  output_data <- data$output_data
  var_list <- list(
    two_side = input_data$alternative == "two_sided",
    min = input_data$alternative == "min",
    max = input_data$alternative == "max",
    n = input_data$n,
    s = round(input_data$s, round_digits),
    alpha = input_data$alpha,
    answer = answer,
    chi_sq_1 = round(output_data$chi_sq_2, round_digits),
    chi_sq_2 = round(output_data$chi_sq_1, round_digits),
    chi_sq_1_2 = round(output_data$chi_sq_1_div_2, round_digits),
    chi_sq_2_2 = round(output_data$chi_sq_2_div_2, round_digits),
    n_1 = input_data$n - 1,
    s2 = round(input_data$s^2, round_digits),
    bottom = round(output_data$bottom, round_digits),
    top = round(output_data$top, round_digits),
    min_value = round(output_data$min, round_digits),
    max_value = round(output_data$max, round_digits),
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
answer_estimate_prop <- function(data, answer = "Khoảng tin cậy cần tìm là:", score=c(0.5, 1.5), round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "estimate_prop.mustache")
  input_data <- data$input_data
  output_data <- data$output_data
  var_list <- list(
    two_side = input_data$alternative == "two_sided",
    min = input_data$alternative == "min",
    max = input_data$alternative == "max",
    n = input_data$n,
    f = round(input_data$f, round_digits),
    alpha = input_data$alpha,
    z_alpha = round(output_data$z_alpha, round_digits),
    z_alpha_div_2 = round(output_data$z_alpha_div_2, round_digits),
    answer = answer,
    g = round(1 - input_data$f, round_digits),
    bottom = round(output_data$bottom, round_digits),
    top = round(output_data$top, round_digits),
    min_value = round(output_data$min, round_digits),
    max_value = round(output_data$max, round_digits),
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
answer_sample_size_mean <- function(data, sd_symbol = "\\sigma", conclusion = function (value) {return(sprintf("%d", value))}, score=c(0.5, 1, 0.5), round_digits = 4) {
  file_name <- get_file_path("template", "estimate", "sample_size_mean.mustache")
  input_data <- data$input_data
  output_data <- data$output_data
  var_list <- list(
    sd_symbol = sd_symbol,
    eps = input_data$eps,
    alpha = input_data$alpha,
    s = round(input_data$sigma, round_digits),
    z_alpha = round(output_data$z_alpha, round_digits),
    value = round(output_data$value, round_digits),
    conclusion = conclusion(output_data$value),
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
  input_data <- data$input_data
  output_data <- data$output_data
  var_list <- list(
    f = round(input_data$f, round_digits),
    eps = input_data$eps,
    alpha = input_data$alpha,
    z_alpha = round(output_data$z_alpha, round_digits),
    g = round(1 - input_data$f, round_digits),
    value = round(output_data$value, round_digits),
    conclusion = conclusion(output_data$value),
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
  input_data <- data$input_data
  output_data <- data$output_data
  var_list <- list(
    eps = input_data$eps,
    alpha = input_data$alpha,
    z_alpha = round(output_data$z_alpha, round_digits),
    value = round(output_data$value, round_digits),
    conclusion = conclusion(output_data$value),
    score_1 = score[1],
    score_2 = score[2],
    score_3 = score[3]
  )
  render_template(file_name, var_list)
}
