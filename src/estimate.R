answer_estimate_mean_norm <- function (n, mean, sigma, alpha) {
  file_name <- "./template/estimate/estimate_mean_norm.mustache"
  data <- estimate_mean_norm(n, mean, sigma, alpha)
  var_list <- list(

  )
  render_template(file_name, var_list)
}

answer_estimate_mean_t <- function (n, mean, s, alpha) {
  file_name <- "./template/estimate/estimate_mean_t.mustache"
  data <- estimate_mean_t(n, mean, s, alpha)
  var_list <- list(

  )
  render_template(file_name, var_list)
}

answer_estimate_var <- function (n, s, alpha) {
  file_name <- "./template/estimate/estimate_var.mustache"
  data <- estimate_var(n, s, alpha)
  var_list <- list(

  )
  render_template(file_name, var_list)
}

answer_estimate_prop <- function (n, f, alpha) {
  file_name <- "./template/estimate/estimate_prop.mustache"
  data <- estimate_prop(n, f, alpha)
  var_list <- list(

  )
  render_template(file_name, var_list)
}

answer_sample_size_mean <- function (sigma, eps, alpha) {
  file_name <- "./template/estimate/sample_size_mean.mustache"
  data <- sample_size_mean(sigma, eps, alpha)
  var_list <- list(

  )
  render_template(file_name, var_list)
}

answer_sample_size_prop_1 <- function (f, eps, alpha) {
  file_name <- "./template/estimate/sample_size_prop_1.mustache"
  data <- sample_size_prop_1(f, eps, alpha)
  var_list <- list(
  )
  render_template(file_name, var_list)
}

answer_sample_size_prop_2 <- function (eps, alpha) {
  file_name <- "./template/estimate/sample_size_prop_2.mustache"
  data <- sample_size_prop_2(eps, alpha)
  var_list <- list(

  )
  render_template(file_name, var_list)
}

