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

