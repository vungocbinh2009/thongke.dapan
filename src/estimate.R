#' @import thongke
answer_estimate_mean_norm <- function (n, mean, sigma, alpha) {
  file_name <- "./R/template/estimate/estimate_mean_norm.mustache"
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

#' @import thongke
answer_estimate_mean_t <- function (n, mean, s, alpha) {
  file_name <- "./R/template/estimate/estimate_mean_t.mustache"
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

answer_estimate_var <- function (n, s, alpha) {
  file_name <- "./R/template/estimate/estimate_var.mustache"
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

answer_estimate_prop <- function (n, f, alpha) {
  file_name <- "./R/template/estimate/estimate_prop.mustache"
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

answer_sample_size_mean <- function (sigma, eps, alpha) {
  file_name <- "./R/template/estimate/sample_size_mean.mustache"
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

answer_sample_size_prop_1 <- function (f, eps, alpha) {
  file_name <- "./R/template/estimate/sample_size_prop_1.mustache"
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

answer_sample_size_prop_2 <- function (eps, alpha) {
  file_name <- "./R/template/estimate/sample_size_prop_2.mustache"
  data <- sample_size_prop_2(eps, alpha, silent = TRUE)
  var_list <- list(
    eps = eps,
    alpha = alpha,
    z_alpha = round(data$z_alpha, 4),
    value = round(data$value, 4)
  )
  render_template(file_name, var_list)
}

