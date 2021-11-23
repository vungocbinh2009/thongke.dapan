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

