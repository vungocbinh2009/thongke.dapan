#' @import thongke
answer_estimate_mean_norm <- function (n, mean, sigma, alpha) {
  file_name <- "./R/template/estimate/estimate_mean_norm.mustache"
  data <- estimate_mean_norm(n, mean, sigma, alpha)
  var_list <- list(
    mean = mean,
    n = n,
    s = sigma,
    alpha = alpha,
    z_alpha = data$z_alpha,
    bottom = data$bottom,
    top = data$top
  )
  render_template(file_name, var_list)
}

#' @import thongke
answer_estimate_mean_t <- function (n, mean, s, alpha) {
  file_name <- "./R/template/estimate/estimate_mean_t.mustache"
  data <- estimate_mean_t(n, mean, s, alpha)
  var_list <- list(
    mean = mean,
    n = n,
    s = s,
    alpha = alpha,
    t_alpha = data$t_alpha,
    bottom = data$bottom,
    top = data$top
  )
  render_template(file_name, var_list)
}

answer_estimate_var <- function (n, s, alpha) {
  file_name <- "./R/template/estimate/estimate_var.mustache"
  data <- estimate_var(n, s, alpha)
  var_list <- list(
    n = n,
    s = s,
    alpha = alpha,
    chi_sq_1 = data$chi_sq_1,
    chi_sq_2 = data$chi_sq_2,
    n_1 = n - 1,
    s2 = s * s,
    bottom = data$bottom,
    top = data$top
  )
  render_template(file_name, var_list)
}

answer_estimate_prop <- function (n, f, alpha) {
  file_name <- "./R/template/estimate/estimate_prop.mustache"
  data <- estimate_prop(n, f, alpha)
  var_list <- list(
    k = n*f,
    n = n,
    f = f,
    alpha = alpha,
    z_alpha = data$z_alpha,
    g = 1 - f,
    bottom = data$bottom,
    top = data$top
  )
  render_template(file_name, var_list)
}

answer_sample_size_mean <- function (sigma, eps, alpha) {
  file_name <- "./R/template/estimate/sample_size_mean.mustache"
  data <- sample_size_mean(sigma, eps, alpha)
  var_list <- list(
    eps = eps,
    alpha = alpha,
    s = sigma,
    z_alpha = data$z_alpha,
    value = data$value
  )
  render_template(file_name, var_list)
}

answer_sample_size_prop_1 <- function (n, f, eps, alpha) {
  file_name <- "./R/template/estimate/sample_size_prop_1.mustache"
  data <- sample_size_prop_1(f, eps, alpha)
  var_list <- list(
     k = n*f,
     n = n,
     f = f,
     eps = eps,
     alpha = alpha,
     z_alpha = data$z_alpha,
     g = 1 - f,
     value = data$value
  )
  render_template(file_name, var_list)
}

answer_sample_size_prop_2 <- function (eps, alpha) {
  file_name <- "./R/template/estimate/sample_size_prop_2.mustache"
  data <- sample_size_prop_2(eps, alpha)
  var_list <- list(
    eps = eps,
    alpha = alpha,
    z_alpha = data$z_alpha,
    value = data$value
  )
  render_template(file_name, var_list)
}

