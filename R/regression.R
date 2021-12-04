#' Hàm này in ra đáp án cho bài toán tính hệ số tương quan
#' @import thongke
#' @export
answer_correlation <- function (x, y) {
  file_name <- get_file_path("template", "regression", "correlation.mustache")
  data1 <- calculate_sum(x, y)
  cor <- correlation(x, y, silent = TRUE)
  var_list <- list(
    sum_xy = data1$sum_xy,
    sum_x = data1$sum_x,
    sum_y = data1$sum_y,
    sum_x2 = data1$sum_x2,
    sum_y2 = data1$sum_y2,
    n = length(x),
    cor = round(cor, 4)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán hồi quy tuyến tính đơn và tìm giá trị dự báo của Y
#' @import thongke
#' @export
answer_linear_regression <- function (x, y, value) {
  file_name <- get_file_path("template", "regression", "linear_regression.mustache")
  data1 <- calculate_sum(x, y)
  data2 <- linear_regression(x, y, silent = TRUE)
  predict_value <- linear_regression_predict(x, y, value, silent = TRUE)
  var_list <- list(
    sum_xy = data1$sum_xy,
    sum_x = data1$sum_x,
    sum_y = data1$sum_y,
    sum_x2 = data1$sum_x2,
    n = length(x),
    a = round(data2$a, 4),
    b = round(data2$b, 4),
    value = value,
    predict_value = round(predict_value, 4)
  )
  render_template(file_name, var_list)
}

#' Hàm này tính tất cả các giá trị tổng giữa x và y
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