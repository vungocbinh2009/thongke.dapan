#' Tính giá trị trung bình
#'
#' Hàm in đáp án bài toán tính giá trị trung bình
#' @export
answer_mean <- function(data, answer, score = 1, round_digits = 4) {
  file_name <- get_file_path("template", "descriptive", "mean.mustache")
  freq_table <- table(data)
  value <- as.numeric(names(freq_table))
  freq <- as.numeric(freq_table)
  var_list <- list(
    answer = answer,
    n = length(data),
    x_1 = value[1],
    r_1 = freq[1],
    x_2 = value[2],
    r_2 = freq[2],
    x_n = value[length(value)],
    r_n = freq[length(freq)],
    mean = round(mean(data), digits = round_digits),
    score = score
  )
  render_template(file_name, var_list)
}

#' Tính giá trị phương sai
#'
#' Hàm này in đáp án cho bài toán tính phương sai
#' @export
answer_var <- function(data, answer, with_mean = TRUE, score = 1, round_digits = 4) {
  file_name <- get_file_path("template", "descriptive", "var.mustache")
  freq_table <- table(data)
  value <- as.numeric(names(freq_table))
  freq <- as.numeric(freq_table)
  if (with_mean) {
    var_list <- list(
      with_mean = TRUE,
      answer = answer,
      n = length(data),
      x_1 = value[1],
      r_1 = freq[1],
      x_2 = value[2],
      r_2 = freq[2],
      x_n = value[length(value)],
      r_n = freq[length(freq)],
      mean = round(mean(data), round_digits),
      var = round(var(data), round_digits),
      score = score
    )
  } else {
    var_list <- list(
      with_mean = FALSE,
      sum_x2r = sum(data^2),
      sum_xr = sum(data),
      n = length(data),
      var = round(var(data), round_digits),
      score = score
    )
  }
  render_template(file_name, var_list)
}

#' Tính giá trị độ lệch chuẩn
#'
#' Hàm này in đáp án cho bài toán tính giá trị độ lệch chuẩn
#' @import whisker
#' @export
answer_sd <- function(data, answer, with_mean = TRUE, score = 1, round_digits = 4) {
  file_name <- get_file_path("template", "descriptive", "sd.mustache")
  freq_table <- table(data)
  value <- as.numeric(names(freq_table))
  freq <- as.numeric(freq_table)
  if (with_mean) {
    var_list <- list(
      with_mean = TRUE,
      answer = answer,
      n = length(data),
      x_1 = value[1],
      r_1 = freq[1],
      x_2 = value[2],
      r_2 = freq[2],
      x_n = value[length(value)],
      r_n = freq[length(freq)],
      mean = round(mean(data), round_digits),
      var = round(var(data), round_digits),
      sd = round(sd(data), round_digits),
      score = score
    )
  } else {
    var_list <- list(
      with_mean = FALSE,
      sum_x2r = sum(data^2),
      sum_xr = sum(data),
      n = length(data),
      var = round(var(data), round_digits),
      sd = round(sd(data), round_digits),
      score = score
    )
  }
  render_template(file_name, var_list)
}
