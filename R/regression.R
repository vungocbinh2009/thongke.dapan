#' Trình bày bài toán tính hệ số tương quan.
#'
#' Hàm này in ra đáp án cho bài toán tính hệ số tương quan
#' @import thongke
#' @export
answer_correlation <- function(data, score = 1, round_digits = 4) {
  file_name <- get_file_path("template", "regression", "correlation.mustache")
  params <- data$params
  cor <- data$result
  sum <- calculate_sum(params$x, params$y)
  var_list <- list(
    sum_xy = sum$sum_xy,
    sum_x = sum$sum_x,
    sum_y = sum$sum_y,
    sum_x2 = sum$sum_x2,
    sum_y2 = sum$sum_y2,
    n = length(params$x),
    cor = round(cor, round_digits),
    score = score
  )
  render_template(file_name, var_list)
}

#' Trình bày bài toán hồi quy tuyến tính đơn.
#'
#' Hàm này in ra đáp án cho bài toán hồi quy tuyến tính đơn
#' @import thongke
#' @export
answer_linear_regression <- function(data, score=c(1, 0.5), round_digits = 4) {
  file_name <- get_file_path("template", "regression", "linear_regression.mustache")
  params <- data$params
  sum <- calculate_sum(params$x, params$y)
  result <- data$result
  var_list <- list(
    sum_xy = sum$sum_xy,
    sum_x = sum$sum_x,
    sum_y = sum$sum_y,
    sum_x2 = sum$sum_x2,
    n = length(params$x),
    a = round(result$a, round_digits),
    b = round(result$b, round_digits),
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
answer_linear_regression_predict <- function(data, conclusion, value_unit, score = 0.5, round_digits = 4) {
  file_name <- get_file_path("template", "regression", "linear_regression_predict.mustache")
  params <- data$params
  result <- data$result
  df <- data.frame(X = params$x, Y = params$y)
  model <- lm(Y ~ X, data = df)
  var_list <- list(
    a = round(model$coefficients[2], round_digits),
    b = round(model$coefficients[1], round_digits),
    conclusion = conclusion,
    value = params$value,
    predict_value = round(result, round_digits),
    value_unit = value_unit,
    score = score
  )
  render_template(file_name, var_list)
}

#' Tính các giá trị tổng và tích giữa X và Y.
#'
#' Hàm này tính tất cả các giá trị tổng giữa x và y
#' @export
answer_calculate_sum <- function(data, score = 1) {
  file_name <- get_file_path("template", "regression", "calculate_sum.mustache")
  #params <- data$params
  result <- data$result
  var_list <- list(
    sum_xy = result$sum_xy,
    sum_x = result$sum_x,
    sum_y = result$sum_y,
    sum_x2 = result$sum_x2,
    sum_y2 = result$sum_y2,
    score = score
  )
  render_template(file_name, var_list)
}
