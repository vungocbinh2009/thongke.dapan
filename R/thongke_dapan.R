#' Hàm này in ra đáp án cho bài toán ước lượng giá trị trung bình (dùng phân bố chuẩn)
#' @import thongke
#' @export
answer_estimate_mean_norm <- function (n, mean, sigma, alpha) {
  file_name <- file.path("template", "estimate", "estimate_mean_t.mustache")
  data <- estimate_mean_norm(n, mean, sigma, alpha, silent = TRUE)
  var_list <- list(
    mean = round(mean, 4),
    n = n,
    s = round(sigma, 4),
    alpha = alpha,
    z_alpha = round(data$z_alpha, 4),
    bottom = round(data$bottom, 4),
    top = round(data$top, 4)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán ước lượng giá trị trung bình (dùng phân bố Student)
#' @import thongke
#' @export
answer_estimate_mean_t <- function (n, mean, s, alpha) {
  file_name <- "/R/template/estimate/estimate_mean_t.mustache"
  data <- estimate_mean_t(n, mean, s, alpha, silent = TRUE)
  var_list <- list(
    mean = round(mean, 4),
    n = n,
    s = round(s, 4),
    alpha = alpha,
    t_alpha = round(data$t_alpha, 4),
    bottom = round(data$bottom, 4),
    top = round(data$top, 4)
  )
  render_template(file_name, var_list)
}

#'Hàm này in ra đáp án cho bài toán ước lượng phương sai
#' @import thongke
#' @export
answer_estimate_var <- function (n, s, alpha) {
  file_name <- "/R/template/estimate/estimate_var.mustache"
  data <- estimate_var(n, s, alpha, silent = TRUE)
  var_list <- list(
    n = n,
    s = round(s, 4),
    alpha = alpha,
    chi_sq_1 = round(data$chi_sq_1, 4),
    chi_sq_2 = round(data$chi_sq_2, 4),
    n_1 = n - 1,
    s2 = round(s*s, 4),
    bottom = round(data$bottom, 4),
    top = round(data$bottom, 4)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán ước lượng cho tỷ lệ
#' @import thongke
#' @export
answer_estimate_prop <- function (n, f, alpha) {
  file_name <- "/R/template/estimate/estimate_prop.mustache"
  data <- estimate_prop(n, f, alpha, silent = TRUE)
  var_list <- list(
    n = n,
    f = round(f, 4),
    alpha = alpha,
    z_alpha = round(data$z_alpha, 4),
    g = round(1 - f, 4),
    bottom = round(data$bottom, 4),
    top = round(data$top, 4)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán xác định kích thước mẫu (với giá trị trung bình)
#' @import thongke
#' @export
answer_sample_size_mean <- function (sigma, eps, alpha) {
  file_name <- "/R/template/estimate/sample_size_mean.mustache"
  data <- sample_size_mean(sigma, eps, alpha, silent = TRUE)
  var_list <- list(
    eps = eps,
    alpha = alpha,
    s = round(sigma, 4),
    z_alpha = round(data$z_alpha, 4),
    value = round(data$value, 4)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán xác định kích thước mẫu (với tỷ lệ, khi đã biết f)
#' @import thongke
#' @export
answer_sample_size_prop_1 <- function (f, eps, alpha) {
  file_name <- "/R/template/estimate/sample_size_prop_1.mustache"
  data <- sample_size_prop_1(f, eps, alpha, silent = TRUE)
  var_list <- list(
     f = round(f, 4),
     eps = eps,
     alpha = alpha,
     z_alpha = round(data$z_alpha, 4),
     g = round(1-f, 4),
     value = round(data$value, 4)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán xác định kích thước mẫu (với tỷ lệ, khi đã biết f)
#' @import thongke
#' @export
answer_sample_size_prop_2 <- function (eps, alpha) {
  file_name <- "/R/template/estimate/sample_size_prop_2.mustache"
  data <- sample_size_prop_2(eps, alpha, silent = TRUE)
  var_list <- list(
    eps = eps,
    alpha = alpha,
    z_alpha = round(data$z_alpha, 4),
    value = round(data$value, 4)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @export
answer_test_mean_norm <- function (n, mean, mean_0, sigma, alpha, mode="neq") {
  file_name <- "/R/template/hypothesis_test/test_mean_norm.mustache"
  data <- test_mean_norm(n, mean, mean_0, sigma, alpha, mode, silent = TRUE)
  var_list <- list(
    mean_0 = mean_0,
    mean = round(mean, 4),
    n = n,
    sigma = round(sigma, 4),
    test = round(data$test, 4),
    alpha = alpha,
    c = round(data$c, 4),
    conclusion = get_conclusion(data$test, data$c)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @export
answer_test_mean_t <- function (n, mean, mean_0, s, alpha, mode="neq") {
  file_name <- "/R/template/hypothesis_test/test_mean_t.mustache"
  data <- test_mean_t(n, mean, mean_0, s, alpha, mode, silent = TRUE)
  var_list <- list(
    mean_0 = mean_0,
    mean = round(mean, 4),
    n = n,
    s = round(s, 4),
    test = round(data$test, 4),
    alpha = alpha,
    c = round(data$c, 4),
    conclusion = get_conclusion(data$test, data$c)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @export
answer_test_prop <- function (n, f, p_0, alpha, mode="neq") {
  file_name <- "/R/template/hypothesis_test/test_prop.mustache"
  data <- test_prop(n, f, p_0, alpha, mode, silent = TRUE)
  var_list <- list(
    k = round(n*f, 0),
    n = n,
    f = round(f, 4),
    p_0 = p_0,
    g_0 = 1 - p_0,
    test = round(data$test, 4),
    alpha = alpha,
    c = round(data$c, 4),
    conclusion = get_conclusion(data$test, data$c)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @import xtable
#' @export
answer_test_goodness_of_fit <- function (statement, actual, expected, alpha) {
  file_name <- "/R/template/hypothesis_test/test_goodness_of_fit.mustache"
  data <- test_chi_squared(actual, expected, alpha, silent = TRUE)
  col_names <- seq_along(actual)
  row_names <- c("Tần số quan sát", "Tần số lý thuyết")
  matrix <- matrix(c(actual, expected), nrow = 2,
                   byrow = TRUE, dimnames = list(row_names, col_names))
  var_list <- list(
    statement = statement,
    # Khi truyền vào làm tham số, ta không in gì ra màn hình, nội dung bảng sẽ chỉ được
    # in ra khi gọi hàm render_template.
    table = print(xtable(matrix), print.results = FALSE),
    a_1 = actual[1],
    a_2 = actual[2],
    a_k = actual[length[actual]],
    e_1 = expected[1],
    e_2 = expected[2],
    e_k = expected[length[expected]],
    test = round(data$test, 4),
    alpha = alpha,
    c = round(data$c, 4),
    conclusion = get_conclusion(data$test, data$c)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @export
answer_test_2_mean_norm <- function (n1, n2, mean1, mean2, sigma1, sigma2, alpha, mode="neq") {
  file_name <- "/R/template/hypothesis_test/test_2_mean_norm.mustache"
  data <- test_2_mean_norm(n1, n2, mean1, mean2, sigma1, sigma2, alpha, mode, silent = TRUE)
  var_list <- list(
    mean1 = round(mean1, 4),
    n1 = n1,
    sigma1 = round(sigma1, 4),
    mean2 = round(mean2, 4),
    n2 = n2,
    sigma2 = round(sigma2, 4),
    test = round(data$test, 4),
    alpha = alpha,
    c = round(data$c, 4),
    conclusion = get_conclusion(data$test, data$c)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @export
answer_test_2_mean_t <- function (n1, n2, mean1, mean2, s1, s2, alpha, mode="neq") {
  file_name <- "/R/template/hypothesis_test/test_2_mean_t.mustache"
  data <- test_2_mean_t(n1, n2, mean1, mean2, s1, s2, alpha, mode, silent = TRUE)
  var_list <- list(
    mean1 = round(mean1, 4),
    n1 = n1,
    s1 = round(s1, 4),
    mean2 = round(mean2, 4),
    n2 = n2,
    s2 = round(s2, 4),
    s = round(sqrt(data$s), 4),
    test = round(data$test, 4),
    alpha = alpha,
    c = round(data$c, 4),
    conclusion = get_conclusion(data$test, data$c)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @export
answer_test_2_prop <- function (n1, n2, f1, f2, alpha, mode="neq") {
  file_name <- "/R/template/hypothesis_test/test_2_prop.mustache"
  data <- test_2_prop(n1, n2, f1, f2, alpha, mode, silent = TRUE)
  var_list <- list(
    k1 = round(n1*f1, 0),
    n1 = n1,
    f1 = round(f1, 4),
    k2 = round(n2*f2, 0),
    n2 = n2,
    f2 = round(f2, 4),
    f = round(data$f, 4),
    g = round(1 - data$f, 4),
    test = round(data$test, 4),
    alpha = alpha,
    c = round(data$c, 4),
    conclusion = get_conclusion(data$test, data$c)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @import xtable
#' @export
answer_test_k_prop <- function (statement, m_i, n_i, alpha) {
  file_name <- "/R/template/hypothesis_test/test_k_prop.mustache"
  data <- test_n_prop(m_i, n_i, alpha, silent = TRUE)
  row_names <- c("Có A", "Không A", "Tổng")
  col_names <- c(seq_along(m_i), "Tổng")
  l_i <- n_i - m_i
  matrix <- matrix(c(m_i, data$sum_m_i, l_i, data$sum_l_i, n_i, data$sum_n_i), nrow = 3,
                   byrow = TRUE, dimnames = list(row_names, col_names))
  var_list <- list(
    statement = statement,
    # Khi truyền vào làm tham số, ta không in gì ra màn hình, nội dung bảng sẽ chỉ được
    # in ra khi gọi hàm render_template
    table = print(xtable(matrix), print.results = FALSE),
    sum_n_i = data$sum_n_i,
    sum_m_i = data$sum_m_i,
    sum_l_i = data$sum_l_i,
    m_1 = m_i[1],
    m_2 = m_i[2],
    n_1 = n_i[1],
    n_2 = n_i[2],
    test = round(data$test, 4),
    alpha = alpha,
    c = round(data$c, 4),
    conclusion = get_conclusion(data$test, data$c)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @import xtable
#' @export
answer_test_independent <- function (statement, matrix, alpha) {
  file_name <- "/R/template/hypothesis_test/test_independent.mustache"
  data <- test_independent(matrix, alpha, silent = TRUE)
  row_names <- c(seq_len(nrow(matrix)), "Tổng")
  col_names <- c(seq_len(ncol(matrix)), "Tổng")
  matrix_2 <- rbind(matrix, data$col_sums)
  matrix_2 <- cbind(matrix_2, c(data$row_sums, data$n))
  colnames(matrix_2) <- col_names
  rownames(matrix_2) <- row_names
  var_list <- list(
    statement = statement,
    # Khi truyền vào làm tham số, ta không in gì ra màn hình, nội dung bảng sẽ chỉ được
    # in ra khi gọi hàm render_template
    table = print(xtable(matrix_2), print.results = FALSE),
    n = data$n,
    n_11 = matrix[1, 1],
    n_10 = data$row_sums[1],
    n_01 = data$col_sums[1],
    n_12 = matrix[1, 2],
    n_02 = data$col_sums[2],
    n_rk = matrix[length[data$row_sums], length[data$col_sums]],
    n_r0 = data$row_sums[length[data$row_sums]],
    n_0k = data$col_sums[length[data$col_sums]],
    test = round(data$test, 4),
    alpha = alpha,
    c = round(data$c, 4),
    conclusion = get_conclusion(data$test, data$c)
  )
  render_template(file_name, var_list)
}

#' Hàm này đưa ra kết luận cho bài toán kiểm định giả thiết
get_conclusion <- function (test, c) {
  if(abs(test) > c) {
    conclusion <- "Vì |T| > c nên ta bác bỏ $H_0$, chấp nhận $H_1$"
  } else {
    conclusion <- "Vì |T| < c nên ta chưa đủ cơ sở để bác bỏ $H_0$"
  }
  return(conclusion)
}

#' Hàm này in ra đáp án cho bài toán tính hệ số tương quan
#' @import thongke
#' @export
answer_correlation <- function (x, y) {
  file_name <- "/R/template/regression/correlation.mustache"
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
  file_name <- "/R/template/regression/linear_regression.mustache"
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

#' Hàm này dùng thư viện whisker để render file
#' @import whisker
#' @import here
render_template <- function (file_name, var_list) {
  template <- readLines(paste0(system.file(package = "thongke.dapan"), file_name))
  return(whisker.render(template, var_list))
}