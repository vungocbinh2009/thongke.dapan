#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @export
answer_test_mean_norm <- function(data, sd_symbol) {
  file_name <- get_file_path("template", "hypothesis_test", "test_mean_norm.mustache")
  params <- data$params
  result <- data$result
  operator <- get_operator(params$mode)
  var_list <- list(
    operator = operator,
    sd_symbol = sd_symbol,
    mean_0 = params$mean_0,
    mean = round(params$mean, 4),
    n = params$n,
    sigma = round(params$sigma, 4),
    test = round(result$test, 4),
    alpha = params$alpha,
    c = round(result$c, 4),
    conclusion = get_conclusion(result$rejected)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @export
answer_test_mean_t <- function(data) {
  file_name <- get_file_path("template", "hypothesis_test", "test_mean_t.mustache")
  params <- data$params
  result <- data$result
  operator <- get_operator(params$mode)
  var_list <- list(
    operator = operator,
    mean_0 = params$mean_0,
    mean = round(params$mean, 4),
    n = params$n,
    s = round(params$s, 4),
    test = round(result$test, 4),
    alpha = params$alpha,
    c = round(result$c, 4),
    conclusion = get_conclusion(result$rejected)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @export
answer_test_prop <- function(data) {
  file_name <- get_file_path("template", "hypothesis_test", "test_prop.mustache")
  params <- data$params
  result <- data$result
  operator <- get_operator(params$mode)
  var_list <- list(
    operator = operator,
    k = round(params$n * params$f, 0),
    n = params$n,
    f = round(params$f, 4),
    p_0 = params$p_0,
    g_0 = 1 - params$p_0,
    test = round(result$test, 4),
    alpha = params$alpha,
    c = round(result$c, 4),
    conclusion = get_conclusion(result$rejected)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @import xtable
#' @export
answer_test_goodness_of_fit <- function(data, h0, col_names) {
  file_name <- get_file_path("template", "hypothesis_test", "test_goodness_of_fit.mustache")
  params <- data$params
  result <- data$result
  col_names <- col_names
  row_names <- c("Tần số quan sát", "Tần số lý thuyết")
  matrix <- matrix(c(params$actual, params$expected), nrow = 2,
                   byrow = TRUE, dimnames = list(row_names, col_names))
  var_list <- list(
    h0 = h0,
    # Khi truyền vào làm tham số, ta không in gì ra màn hình, nội dung bảng sẽ chỉ được
    # in ra khi gọi hàm render_template, digits loại bỏ phần thập phân khi in bảng.
    table = print(xtable(matrix, digits = 0), print.results = FALSE, floating = FALSE),
    a_1 = params$actual[1],
    a_2 = params$actual[2],
    a_k = params$actual[length(params$actual)],
    e_1 = params$expected[1],
    e_2 = params$expected[2],
    e_k = params$expected[length(params$expected)],
    test = round(result$test, 4),
    alpha = params$alpha,
    c = round(result$c, 4),
    conclusion = get_conclusion(result$rejected)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @export
answer_test_2_mean_norm <- function(data, sd_symbol) {
  file_name <- get_file_path("template", "hypothesis_test", "test_2_mean_norm.mustache")
  params <- data$params
  result <- data$result
  operator <- get_operator(params$mode)
  var_list <- list(
    operator = operator,
    sd_symbol = sd_symbol,
    mean1 = round(params$mean1, 4),
    n1 = params$n1,
    sigma1 = round(params$sigma1, 4),
    mean2 = round(params$mean2, 4),
    n2 = params$n2,
    sigma2 = round(params$sigma2, 4),
    test = round(result$test, 4),
    alpha = params$alpha,
    c = round(result$c, 4),
    conclusion = get_conclusion(result$rejected)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @export
answer_test_2_mean_t <- function(data) {
  file_name <- get_file_path("template", "hypothesis_test", "test_2_mean_t.mustache")
  params <- data$params
  result <- data$result
  operator <- get_operator(params$mode)
  var_list <- list(
    operator = operator,
    mean1 = round(params$mean1, 4),
    n1 = params$n1,
    s1 = round(params$s1, 4),
    mean2 = round(params$mean2, 4),
    n2 = params$n2,
    s2 = round(params$s2, 4),
    s = round(sqrt(result$s), 4),
    test = round(result$test, 4),
    alpha = params$alpha,
    c = round(result$c, 4),
    conclusion = get_conclusion(result$rejected)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @export
answer_test_2_prop <- function(data) {
  file_name <- get_file_path("template", "hypothesis_test", "test_2_prop.mustache")
  params <- data$params
  result <- data$result
  operator <- get_operator(params$mode)
  var_list <- list(
    operator = operator,
    k1 = round(params$n1 * params$f1, 0),
    n1 = params$n1,
    f1 = round(params$f1, 4),
    k2 = round(params$n2 * params$f2, 0),
    n2 = params$n2,
    f2 = round(params$f2, 4),
    f = round(result$f, 4),
    g = round(1 - result$f, 4),
    test = round(result$test, 4),
    alpha = params$alpha,
    c = round(result$c, 4),
    conclusion = get_conclusion(result$rejected)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @import xtable
#' @export
answer_test_k_prop <- function(data, h0, row_names, col_names) {
  file_name <- get_file_path("template", "hypothesis_test", "test_k_prop.mustache")
  params <- data$params
  result <- data$result
  row_names_full <- c(row_names, "Tổng")
  col_names_full <- c(col_names, "Tổng")
  l_i <- params$n_i - params$m_i
  matrix <- matrix(c(params$m_i, result$sum_m_i, l_i, result$sum_l_i, params$n_i, result$sum_n_i), nrow = 3,
                   byrow = TRUE, dimnames = list(row_names_full, col_names_full))
  var_list <- list(
    h0 = h0,
    # Khi truyền vào làm tham số, ta không in gì ra màn hình, nội dung bảng sẽ chỉ được
    # in ra khi gọi hàm render_template, digits loại bỏ phần thập phân khi in bảng.
    table = print(xtable(matrix, digits = 0), print.results = FALSE, floating = FALSE),
    sum_n_i = result$sum_n_i,
    sum_m_i = result$sum_m_i,
    sum_l_i = result$sum_l_i,
    m_1 = params$m_i[1],
    m_2 = params$m_i[2],
    n_1 = params$n_i[1],
    n_2 = params$n_i[2],
    test = round(result$test, 4),
    alpha = params$alpha,
    c = round(result$c, 4),
    conclusion = get_conclusion(result$rejected)
  )
  render_template(file_name, var_list)
}

#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @import xtable
#' @export
answer_test_independent <- function(data, h0, row_names, col_names) {
  file_name <- get_file_path("template", "hypothesis_test", "test_independent.mustache")
  params <- data$params
  result <- data$result
  row_names_full <- c(row_names, "Tổng")
  col_names_full <- c(col_names, "Tổng")
  matrix_2 <- rbind(params$matrix, result$col_sums)
  matrix_2 <- cbind(matrix_2, c(result$row_sums, result$n))
  colnames(matrix_2) <- col_names_full
  rownames(matrix_2) <- row_names_full
  var_list <- list(
    h0 = h0,
    # Khi truyền vào làm tham số, ta không in gì ra màn hình, nội dung bảng sẽ chỉ được
    # in ra khi gọi hàm render_template, digits loại bỏ phần thập phân khi in bảng.
    table = print(xtable(matrix_2, digits = 0), print.results = FALSE, floating = FALSE),
    n = result$n,
    n_11 = params$matrix[1, 1],
    n_10 = result$row_sums[1],
    n_01 = result$col_sums[1],
    n_12 = params$matrix[1, 2],
    n_02 = result$col_sums[2],
    n_rk = params$matrix[length(result$row_sums), length(result$col_sums)],
    n_r0 = result$row_sums[length(result$row_sums)],
    n_0k = result$col_sums[length(result$col_sums)],
    test = round(result$test, 4),
    alpha = params$alpha,
    c = round(result$c, 4),
    conclusion = get_conclusion(result$rejected)
  )
  render_template(file_name, var_list)
}

get_operator <- function (mode) {
  return(switch(
    mode,
    "neq" = "\\neq",
    "less" = "\\leqslant",
    "greater" = "\\geqslant"
  ))
}

#' Hàm này đưa ra kết luận cho bài toán kiểm định giả thiết
get_conclusion <- function(rejected) {
  if (rejected) {
    conclusion <- "Vì |T| > c nên ta bác bỏ $H_0$, chấp nhận $H_1$"
  } else {
    conclusion <- "Vì |T| < c nên ta chưa đủ cơ sở để bác bỏ $H_0$"
  }
  return(conclusion)
}
