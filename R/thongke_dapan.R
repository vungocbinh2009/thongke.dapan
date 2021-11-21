#' @import thongke
answer_estimate_mean_norm <- function (n, mean, sigma, alpha) {
  file_name <- "./R/template/estimate/estimate_mean_norm.mustache"
  data <- estimate_mean_norm(n, mean, sigma, alpha)
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

#' @import thongke
answer_estimate_mean_t <- function (n, mean, s, alpha) {
  file_name <- "./R/template/estimate/estimate_mean_t.mustache"
  data <- estimate_mean_t(n, mean, s, alpha)
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

answer_estimate_var <- function (n, s, alpha) {
  file_name <- "./R/template/estimate/estimate_var.mustache"
  data <- estimate_var(n, s, alpha)
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

answer_estimate_prop <- function (n, f, alpha) {
  file_name <- "./R/template/estimate/estimate_prop.mustache"
  data <- estimate_prop(n, f, alpha)
  var_list <- list(
    k = round(n*f, 0),
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

answer_sample_size_mean <- function (sigma, eps, alpha) {
  file_name <- "./R/template/estimate/sample_size_mean.mustache"
  data <- sample_size_mean(sigma, eps, alpha)
  var_list <- list(
    eps = eps,
    alpha = alpha,
    s = round(sigma, 4),
    z_alpha = round(data$z_alpha, 4),
    value = round(data$value, 4)
  )
  render_template(file_name, var_list)
}

answer_sample_size_prop_1 <- function (n, f, eps, alpha) {
  file_name <- "./R/template/estimate/sample_size_prop_1.mustache"
  data <- sample_size_prop_1(f, eps, alpha)
  var_list <- list(
     k = round(n*f, 0),
     n = n,
     f = round(f, 4),
     eps = eps,
     alpha = alpha,
     z_alpha = round(data$z_alpha, 4),
     g = round(1-f, 4),
     value = round(data$value, 4)
  )
  render_template(file_name, var_list)
}

answer_sample_size_prop_2 <- function (eps, alpha) {
  file_name <- "./R/template/estimate/sample_size_prop_2.mustache"
  data <- sample_size_prop_2(eps, alpha)
  var_list <- list(
    eps = eps,
    alpha = alpha,
    z_alpha = round(data$z_alpha, 4),
    value = round(data$value, 4)
  )
  render_template(file_name, var_list)
}

answer_test_mean_norm <- function (n, mean, mean_0, sigma, alpha, mode="neq") {
  file_name <- "./R/template/hypothesis_test/test_mean_norm.mustache"
  data <- test_mean_norm(n, mean, mean_0, sigma, alpha, mode)
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

answer_test_mean_t <- function (n, mean, mean_0, s, alpha, mode="neq") {
  file_name <- "./R/template/hypothesis_test/test_mean_t.mustache"
  data <- test_mean_t(n, mean, mean_0, s, alpha, mode)
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

answer_test_prop <- function (n, f, p_0, alpha, mode="neq") {
  file_name <- "./R/template/hypothesis_test/test_prop.mustache"
  data <- test_prop(n, f, p_0, alpha, mode)
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

answer_test_goodness_of_fit <- function (statement, actual, expected, alpha) {
  file_name <- "./R/template/hypothesis_test/test_goodness_of_fit.mustache"
  data <- test_chi_squared(actual, expected, alpha)
  var_list <- list(
    statement = statement,
    test = round(data$test, 4),
    alpha = alpha,
    c = round(data$c, 4),
    conclusion = get_conclusion(data$test, data$c)
  )
  render_template(file_name, var_list)
}

answer_test_2_mean_norm <- function (n1, n2, mean1, mean2, sigma1, sigma2, alpha, mode="neq") {
  file_name <- "./R/template/hypothesis_test/test_2_mean_norm.mustache"
  data <- test_2_mean_norm(n1, n2, mean1, mean2, sigma1, sigma2, alpha, mode)
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

answer_test_2_mean_t <- function (n1, n2, mean1, mean2, s1, s2, alpha, mode="neq") {
  file_name <- "./R/template/hypothesis_test/test_2_mean_t.mustache"
  data <- test_2_mean_t(n1, n2, mean1, mean2, s1, s2, alpha, mode)
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

answer_test_2_prop <- function (n1, n2, f1, f2, alpha, mode="neq") {
  file_name <- "./R/template/hypothesis_test/test_2_prop.mustache"
  data <- test_2_prop(n1, n2, f1, f2, alpha, mode)
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

answer_test_k_prop <- function (statement, m_i, n_i, alpha) {
  file_name <- "./R/template/hypothesis_test/test_k_prop.mustache"
  data <- test_n_prop(m_i, n_i, alpha)
  var_list <- list(
    statement = statement,
    test = round(data$test, 4),
    alpha = alpha,
    c = round(data$c, 4),
    conclusion = get_conclusion(data$test, data$c)
  )
  render_template(file_name, var_list)
}

answer_test_independent <- function (statement, matrix, alpha) {
  file_name <- "./R/template/hypothesis_test/test_independent.mustache"
  data <- test_independent(matrix, alpha)
  var_list <- list(
    statement = statement,
    test = round(data$test, 4),
    alpha = alpha,
    c = round(data$c, 4),
    conclusion = get_conclusion(data$test, data$c)
  )
  render_template(file_name, var_list)
}

get_conclusion <- function (test, c) {
  if(abs(test) > c) {
    conclusion <- "Vì |T| > c nên ta bác bỏ $H_0$, chấp nhận $H_1$"
  } else {
    conclusion <- "Vì |T| < c nên ta chưa đủ cơ sở để bác bỏ $H_0$"
  }
  return(conclusion)
}

answer_correlation <- function (x, y) {
  file_name <- "./R/template/regression/correlation.mustache"
  data1 <- calculate_sum(x, y)
  cor <- correlation(x, y)
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

answer_linear_regression <- function (x, y, value) {
  file_name <- "./R/template/regression/linear_regression.mustache"
  data1 <- calculate_sum(x, y)
  data2 <- linear_regression(x, y)
  predict_value <- linear_regression_predict(x, y, value)
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
  template <- readLines(here(file_name))
  return(whisker.render(template, var_list))
}