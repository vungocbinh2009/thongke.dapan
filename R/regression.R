#' Trình bày bài toán tính hệ số tương quan.
#'
#' Hàm này in ra đáp án cho bài toán tính hệ số tương quan
#' @import thongke
#' @export
answer_correlation <- function(data, score = 1, round_digits = 4) {
  file_name <- get_file_path("template", "regression", "correlation.mustache")
  input_data <- data$input_data
  output_data <- data$output_data
  sum_data <- calculate_sum(input_data$x, input_data$y)$output_data
  var_list <- list(
    sum_xy = sum_data$sum_xy,
    sum_x = sum_data$sum_x,
    sum_y = sum_data$sum_y,
    sum_x2 = sum_data$sum_x2,
    sum_y2 = sum_data$sum_y2,
    n = length(input_data$x),
    cor = round(output_data$cor, round_digits),
    score = score
  )
  render_template(file_name, var_list)
}

#' Trình bày bài toán hồi quy tuyến tính đơn.
#'
#' Hàm này in ra đáp án cho bài toán hồi quy tuyến tính đơn
#' @import thongke
#' @export
answer_linear_regression <- function(data, intro = "", score=c(1, 0.5), round_digits = 4) {
  file_name <- get_file_path("template", "regression", "linear_regression.mustache")
  input_data <- data$input_data
  sum_data <- calculate_sum(input_data$x, input_data$y)$output_data
  output_data <- data$output_data
  var_list <- list(
    intro = intro,
    sum_xy = sum_data$sum_xy,
    sum_x = sum_data$sum_x,
    sum_y = sum_data$sum_y,
    sum_x2 = sum_data$sum_x2,
    n = length(input_data$x),
    a = round(output_data$a, round_digits),
    b = round(output_data$b, round_digits),
    score_1 = score[1],
    score_2 = score[2]
  )
  render_template(file_name, var_list)
}

#' Trình bày bài toán dự đoán dựa trên mô hình hồi quy.
#'
#' Hàm này in ra đáp án cho bài toán tìm giá trị dự báo của Y dựa trên mô hình hồi quy tuyến tính.
#' @import thongke
#' @export
answer_linear_regression_predict <- function(data, answer = "Giá trị cần dự đoán là:", value_unit = "", score = 0.5, round_digits = 4) {
  file_name <- get_file_path("template", "regression", "linear_regression_predict.mustache")
  input_data <- data$input_data
  output_data <- data$output_data
  df <- data.frame(X = input_data$x, Y = input_data$y)
  model <- lm(Y ~ X, data = df)
  var_list <- list(
    a = round(model$coefficients[2], round_digits),
    b = round(model$coefficients[1], round_digits),
    answer = answer,
    value = input_data$value,
    predict_value = round(output_data$predict_value, round_digits),
    value_unit = ifelse(value_unit == "", "", sprintf("(%s)", value_unit)),
    score = score
  )
  render_template(file_name, var_list)
}

#' Tính các giá trị tổng và tích giữa X và Y.
#'
#' Hàm này tính tất cả các giá trị tổng giữa x và y
#' @export
answer_calculate_sum <- function(data, intro = "", score = 1) {
  file_name <- get_file_path("template", "regression", "calculate_sum.mustache")
  #input_data <- data$input_data
  output_data <- data$output_data
  var_list <- list(
    intro = intro,
    sum_xy = output_data$sum_xy,
    sum_x = output_data$sum_x,
    sum_y = output_data$sum_y,
    sum_x2 = output_data$sum_x2,
    sum_y2 = output_data$sum_y2,
    score = score
  )
  render_template(file_name, var_list)
}
