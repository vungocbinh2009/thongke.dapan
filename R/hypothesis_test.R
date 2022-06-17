#' Trình bày bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#'
#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @export
answer_test_mean_norm <- function(data, sd_symbol, conclusion_h0, conclusion_h1, intro = "", score=c(0.5, 1, 0.5), round_digits = 4) {
  file_name <- get_file_path("template", "hypothesis_test", "test_mean_norm.mustache")
  input_data <- data$input_data
  output_data <- data$output_data
  operator <- get_operator(input_data$alternative)
  var_list <- list(
    intro = intro,
    operator = operator,
    sd_symbol = sd_symbol,
    mean_0 = input_data$mean_0,
    mean = round(input_data$mean, round_digits),
    n = input_data$n,
    sigma = round(input_data$sigma, round_digits),
    test = round(output_data$test, round_digits),
    alpha = input_data$alpha,
    c = round(output_data$c, round_digits),
    stat_conclusion = get_conclusion(output_data$rejected, input_data$alternative),
    conclusion = ifelse(output_data$rejected, conclusion_h1, conclusion_h0),
    score_1 = score[1],
    score_2 = score[2],
    score_3 = score[3]
  )
  render_template(file_name, var_list)
}

#' Trình bày bài toán KĐGT về giá trị trung bình (phân bố Student)
#'
#' Hàm này in ra đáp án cho bài toán KĐGT về giá trị trung bình (phân bố Student)
#' @import thongke
#' @export
answer_test_mean_t <- function(data, conclusion_h0, conclusion_h1, intro = "", score=c(0.5, 1, 0.5), round_digits = 4) {
  file_name <- get_file_path("template", "hypothesis_test", "test_mean_t.mustache")
  input_data <- data$input_data
  output_data <- data$output_data
  operator <- get_operator(input_data$alternative)
  var_list <- list(
    intro = intro,
    operator = operator,
    mean_0 = input_data$mean_0,
    mean = round(input_data$mean, round_digits),
    n = input_data$n,
    s = round(input_data$s, round_digits),
    test = round(output_data$test, round_digits),
    alpha = input_data$alpha,
    c = round(output_data$c, round_digits),
    stat_conclusion = get_conclusion(output_data$rejected, input_data$alternative),
    conclusion = ifelse(output_data$rejected, conclusion_h1, conclusion_h0),
    score_1 = score[1],
    score_2 = score[2],
    score_3 = score[3]
  )
  render_template(file_name, var_list)
}

#' Trình bày bài toán KĐGT về tỷ lệ
#'
#' Hàm này in ra đáp án cho bài toán KĐGT về tỷ lệ
#' @import thongke
#' @export
answer_test_prop <- function(data, conclusion_h0, conclusion_h1, intro = "", score=c(0.5, 1, 0.5), round_digits = 4) {
  file_name <- get_file_path("template", "hypothesis_test", "test_prop.mustache")
  input_data <- data$input_data
  output_data <- data$output_data
  operator <- get_operator(input_data$alternative)
  var_list <- list(
    intro = intro,
    operator = operator,
    k = round(input_data$n * input_data$f, 0),
    n = input_data$n,
    f = round(input_data$f, round_digits),
    p_0 = input_data$p_0,
    g_0 = 1 - input_data$p_0,
    test = round(output_data$test, round_digits),
    alpha = input_data$alpha,
    c = round(output_data$c, round_digits),
    stat_conclusion = get_conclusion(output_data$rejected, input_data$alternative),
    conclusion = ifelse(output_data$rejected, conclusion_h1, conclusion_h0),
    score_1 = score[1],
    score_2 = score[2],
    score_3 = score[3]
  )
  render_template(file_name, var_list)
}

#' Trình bày bài toán kiểm định khi bình phương
#'
#' Hàm này in ra đáp án cho bài toán kiểm định sự phù hợp của k tỷ lệ.
#' @import thongke
#' @import xtable
#' @export
answer_test_goodness_of_fit <- function(data, h0, col_names, conclusion_h0, conclusion_h1, score=c(0.5, 1, 0.5), round_digits = 4) {
  file_name <- get_file_path("template", "hypothesis_test", "test_goodness_of_fit.mustache")
  input_data <- data$input_data
  output_data <- data$output_data
  col_names <- col_names
  row_names <- c("Tần số quan sát", "Tần số lý thuyết")
  matrix <- matrix(c(input_data$actual, input_data$expected), nrow = 2,
                   byrow = TRUE, dimnames = list(row_names, col_names))
  var_list <- list(
    h0 = h0,
    # Khi truyền vào làm tham số, ta không in gì ra màn hình, nội dung bảng sẽ chỉ được
    # in ra khi gọi hàm render_template, digits loại bỏ phần thập phân khi in bảng.
    table = print(xtable(matrix, digits = 0), print.output_datas = FALSE, floating = FALSE),
    a_1 = input_data$actual[1],
    a_2 = input_data$actual[2],
    a_k = input_data$actual[length(input_data$actual)],
    e_1 = input_data$expected[1],
    e_2 = input_data$expected[2],
    e_k = input_data$expected[length(input_data$expected)],
    test = round(output_data$test, round_digits),
    alpha = input_data$alpha,
    c = round(output_data$c, round_digits),
    stat_conclusion = get_conclusion(output_data$rejected, "greater"),
    conclusion = ifelse(output_data$rejected, conclusion_h1, conclusion_h0),
    score_1 = score[1],
    score_2 = score[2],
    score_3 = score[3]
  )
  render_template(file_name, var_list)
}

#' Trình bày bài toán KĐGT về so sánh 2 giá trị trung bình (phân bố chuẩn)
#'
#' Hàm này in ra đáp án cho bài toán KĐGT về so sánh 2 giá trị trung bình (phân bố chuẩn)
#' @import thongke
#' @export
answer_test_2_mean_norm <- function(data, sd_symbol, conclusion_h0, conclusion_h1, intro = "", score=c(0.5, 1, 0.5), round_digits = 4) {
  file_name <- get_file_path("template", "hypothesis_test", "test_2_mean_norm.mustache")
  input_data <- data$input_data
  output_data <- data$output_data
  operator <- get_operator(input_data$alternative)
  var_list <- list(
    intro = intro,
    operator = operator,
    sd_symbol = sd_symbol,
    mean1 = round(input_data$mean[1], round_digits),
    n1 = input_data$n1,
    sigma1 = round(input_data$sigma[1], round_digits),
    mean2 = round(input_data$mean[2], round_digits),
    n2 = input_data$n2,
    sigma2 = round(input_data$sigma[2], round_digits),
    test = round(output_data$test, round_digits),
    alpha = input_data$alpha,
    c = round(output_data$c, round_digits),
    stat_conclusion = get_conclusion(output_data$rejected, input_data$alternative),
    conclusion = ifelse(output_data$rejected, conclusion_h1, conclusion_h0),
    score_1 = score[1],
    score_2 = score[2],
    score_3 = score[3]
  )
  render_template(file_name, var_list)
}

#' Trình bày bài toán KĐGT về so sánh 2 giá trị trung bình (phân bố Student)
#'
#' Hàm này in ra đáp án cho bài toán KĐGT về so sánh 2 giá trị trung bình (phân bố Student)
#' @import thongke
#' @export
answer_test_2_mean_t <- function(data, conclusion_h0, conclusion_h1, intro = "", score=c(0.5, 1, 0.5), round_digits = 4) {
  file_name <- get_file_path("template", "hypothesis_test", "test_2_mean_t.mustache")
  input_data <- data$input_data
  output_data <- data$output_data
  operator <- get_operator(input_data$alternative)
  var_list <- list(
    intro = intro,
    operator = operator,
    mean1 = round(input_data$mean[1], round_digits),
    n1 = input_data$n[1],
    s1 = round(input_data$s[1], round_digits),
    mean2 = round(input_data$mean[2], round_digits),
    n2 = input_data$n[2],
    s2 = round(input_data$s[2], round_digits),
    s = round(sqrt(output_data$s), round_digits),
    test = round(output_data$test, round_digits),
    alpha = input_data$alpha,
    c = round(output_data$c, round_digits),
    stat_conclusion = get_conclusion(output_data$rejected, input_data$alternative),
    conclusion = ifelse(output_data$rejected, conclusion_h1, conclusion_h0),
    score_1 = score[1],
    score_2 = score[2],
    score_3 = score[3]
  )
  render_template(file_name, var_list)
}

#' Trình bày bài toán KĐGT về so sánh 2 tỷ lệ.
#'
#' Hàm này in ra đáp án cho bài toán KĐGT về so sánh 2 tỷ lệ.
#' @import thongke
#' @export
answer_test_2_prop <- function(data, conclusion_h0, conclusion_h1, intro = "", score=c(0.5, 1, 0.5), round_digits = 4) {
  file_name <- get_file_path("template", "hypothesis_test", "test_2_prop.mustache")
  input_data <- data$input_data
  output_data <- data$output_data
  operator <- get_operator(input_data$alternative)
  var_list <- list(
    intro = intro,
    operator = operator,
    k1 = round(input_data$n[1] * input_data$f[1], 0),
    n1 = input_data$n[1],
    f1 = round(input_data$f[1], round_digits),
    k2 = round(input_data$n[2] * input_data$f[2], 0),
    n2 = input_data$n[2],
    f2 = round(input_data$f[2], round_digits),
    f = round(output_data$f, round_digits),
    g = round(1 - output_data$f, round_digits),
    test = round(output_data$test, round_digits),
    alpha = input_data$alpha,
    c = round(output_data$c, round_digits),
    stat_conclusion = get_conclusion(output_data$rejected, input_data$alternative),
    conclusion = ifelse(output_data$rejected, conclusion_h1, conclusion_h0),
    score_1 = score[1],
    score_2 = score[2],
    score_3 = score[3]
  )
  render_template(file_name, var_list)
}

#' Trình bày bài toán KĐGT về so sánh k tỷ lệ.
#'
#' Hàm này in ra đáp án cho bài toán KĐGT về so sánh k tỷ lệ.
#' @import thongke
#' @import xtable
#' @export
answer_test_k_prop <- function(data, h0, row_names, col_names, conclusion_h0, conclusion_h1, score=c(0.5, 1, 0.5), round_digits = 4) {
  file_name <- get_file_path("template", "hypothesis_test", "test_k_prop.mustache")
  input_data <- data$input_data
  output_data <- data$output_data
  row_names_full <- c(row_names, "Tổng")
  col_names_full <- c(col_names, "Tổng")
  l_i <- input_data$n_i - input_data$m_i
  matrix <- matrix(c(input_data$m_i, output_data$sum_m_i, l_i, output_data$sum_l_i, input_data$n_i, output_data$sum_n_i), nrow = 3,
                   byrow = TRUE, dimnames = list(row_names_full, col_names_full))
  var_list <- list(
    h0 = h0,
    # Khi truyền vào làm tham số, ta không in gì ra màn hình, nội dung bảng sẽ chỉ được
    # in ra khi gọi hàm render_template, digits loại bỏ phần thập phân khi in bảng.
    table = print(xtable(matrix, digits = 0), print.output_datas = FALSE, floating = FALSE),
    sum_n_i = output_data$sum_n_i,
    sum_m_i = output_data$sum_m_i,
    sum_l_i = output_data$sum_l_i,
    m_1 = input_data$m_i[1],
    m_2 = input_data$m_i[2],
    n_1 = input_data$n_i[1],
    n_2 = input_data$n_i[2],
    test = round(output_data$test, round_digits),
    alpha = input_data$alpha,
    c = round(output_data$c, round_digits),
    stat_conclusion = get_conclusion(output_data$rejected, "greater"),
    conclusion = ifelse(output_data$rejected, conclusion_h1, conclusion_h0),
    score_1 = score[1],
    score_2 = score[2],
    score_3 = score[3]
  )
  render_template(file_name, var_list)
}

#' Trình bày bài toán kiểm định tính độc lập.
#'
#' Hàm này in ra đáp án cho bài toán kiểm định tính độc lập.
#' @import thongke
#' @import xtable
#' @export
answer_test_independent <- function(data, h0, row_names, col_names, conclusion_h0, conclusion_h1, score=c(0.5, 1, 0.5), round_digits = 4) {
  file_name <- get_file_path("template", "hypothesis_test", "test_independent.mustache")
  input_data <- data$input_data
  output_data <- data$output_data
  row_names_full <- c(row_names, "Tổng")
  col_names_full <- c(col_names, "Tổng")
  matrix_2 <- rbind(input_data$matrix, output_data$col_sums)
  matrix_2 <- cbind(matrix_2, c(output_data$row_sums, output_data$n))
  colnames(matrix_2) <- col_names_full
  rownames(matrix_2) <- row_names_full
  var_list <- list(
    h0 = h0,
    # Khi truyền vào làm tham số, ta không in gì ra màn hình, nội dung bảng sẽ chỉ được
    # in ra khi gọi hàm render_template, digits loại bỏ phần thập phân khi in bảng.
    table = print(xtable(matrix_2, digits = 0), print.output_datas = FALSE, floating = FALSE),
    n = output_data$n,
    n_11 = input_data$matrix[1, 1],
    n_10 = output_data$row_sums[1],
    n_01 = output_data$col_sums[1],
    n_12 = input_data$matrix[1, 2],
    n_02 = output_data$col_sums[2],
    n_rk = input_data$matrix[length(output_data$row_sums), length(output_data$col_sums)],
    n_r0 = output_data$row_sums[length(output_data$row_sums)],
    n_0k = output_data$col_sums[length(output_data$col_sums)],
    test = round(output_data$test, round_digits),
    alpha = input_data$alpha,
    c = round(output_data$c, round_digits),
    stat_conclusion = get_conclusion(output_data$rejected, "greater"),
    conclusion = ifelse(output_data$rejected, conclusion_h1, conclusion_h0),
    score_1 = score[1],
    score_2 = score[2],
    score_3 = score[3]
  )
  render_template(file_name, var_list)
}

#' Hàm này dựa vào alternative để chọn dấu thích hợp cho H1.
get_operator <- function (alternative) {
  return(switch(
    alternative,
    "neq" = "\\neq",
    "less" = "<",
    "greater" = ">"
  ))
}

#' Hàm này đưa ra kết luận cho bài toán kiểm định giả thiết
get_conclusion <- function(rejected, operator) {
  if (rejected) {
    if (operator == "neq") {
      conclusion <- "Vì $|T| > c$ nên ta bác bỏ $H_0$, chấp nhận $H_1$"
    } else if (operator == "less") {
      conclusion <- "Vì $T < -c$ nên ta bác bỏ $H_0$, chấp nhận $H_1$"
    } else {
      conclusion <- "Vì $T > c$ nên ta bác bỏ $H_0$, chấp nhận $H_1$"
    }
  } else {
    if (operator == "neq") {
      conclusion <- "Vì $|T| < c$ nên ta chưa đủ cơ sở để bác bỏ $H_0$"
    } else if (operator == "less") {
      conclusion <- "Vì $T > -c$ nên ta chưa đủ cơ sở để bác bỏ $H_0$"
    } else {
      conclusion <- "Vì $T < c$ nên ta chưa đủ cơ sở để bác bỏ $H_0$"
    }
  }
  return(conclusion)
}
