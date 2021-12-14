#' Hàm này in ra đáp án cho bài toán tính hệ số tương quan
#' @import thongke
#' @export
answer_correlation <- function(data) {
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
    cor = round(cor, 4)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán hồi quy tuyến tính đơn và tìm giá trị dự báo của Y
#' @import thongke
#' @export
answer_linear_regression <- function(data) {
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
    a = round(result$a, 4),
    b = round(result$b, 4)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán hồi quy tuyến tính đơn và tìm giá trị dự báo của Y
#' @import thongke
#' @export
answer_linear_regression_predict <- function(data) {
  file_name <- get_file_path("template", "regression", "linear_regression_predict.mustache")
  params <- data$params
  result <- data$result
  df <- data.frame(X = params$x, Y = params$y)
  model <- lm(Y ~ X, data = df)
  var_list <- list(
    a = round(model$coefficients[2], 4),
    b = round(model$coefficients[1], 4),
    value = params$value,
    predict_value = result
  )
  render_template(file_name, var_list)
}

#' Hàm này tính tất cả các giá trị tổng giữa x và y
#' @export
answer_calculate_sum <- function(data) {
  file_name <- get_file_path("template", "regression", "calculate_sum.mustache")
  #params <- data$params
  result <- data$result
  var_list <- list(
    sum_xy = result$sum_xy,
    sum_x = result$sum_x,
    sum_y = result$sum_y,
    sum_x2 = result$sum_x2,
    sum_y2 = result$sum_y2
  )
  render_template(file_name, var_list)
}

#' Hàm này tính tất cả các giá trị tổng giữa x và y
#' @export
calculate_sum <- function (x, y) {
  sum_x <- sum(x)
  sum_y <- sum(y)
  sum_x2 <- sum(x*x)
  sum_y2 <- sum(y*y)
  sum_xy <- sum(x*y)
  return(list(
    sum_x = sum_x,
    sum_y = sum_y,
    sum_xy = sum_xy,
    sum_x2 = sum_x2,
    sum_y2 = sum_y2
  ))
}
