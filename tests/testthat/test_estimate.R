library(thongke)
library(thongke.dapan)
library(testthat)
library(magrittr)

test_that("Test các hàm answer_estimate_*, answer_sample_*", {
  generate_data(
    estimate_mean_norm,
    list(sigma = 3, n = 36, alpha = 0.05, mean = 66, mode="two.side")
  ) %>%
    answer_estimate_mean_norm(
      sd_symbol = "\\sigma",
      answer = "Khoảng tin cậy 95\\% là:",
    ) %>%
    cat() %>%
    print()
  # Đáp số: 65,02 - 66,98
  print("===================================================")
  generate_data(
    estimate_mean_norm,
    list(sigma = 3, n = 36, alpha = 0.05, mean = 66, mode="min")
  ) %>%
    answer_estimate_mean_norm(
      sd_symbol = "\\sigma",
      answer = "Khoảng tin cậy 95\\% là:"
    ) %>%
    cat() %>%
    print()
  print("===================================================")
  generate_data(
    estimate_mean_norm,
    list(sigma = 3, n = 36, alpha = 0.05, mean = 66, mode="max")
  ) %>%
    answer_estimate_mean_norm(
      sd_symbol = "\\sigma",
      answer = "Khoảng tin cậy 95\\% là:"
    ) %>%
    cat() %>%
    print()
  print("===================================================")
  generate_data(
    estimate_mean_t,
    list(mean = 39.8, alpha = 0.01, n = 15, s = sqrt(0.144), mode="two.side")
  ) %>%
    answer_estimate_mean_t(
      answer = "Khoảng tin cậy 99\\% là"
    ) %>%
    cat() %>%
    print()
  # Đáp số: 39,5023 - 40,0977
  print("===================================================")
  generate_data(
    estimate_mean_t,
    list(mean = 39.8, alpha = 0.01, n = 15, s = sqrt(0.144), mode="min")
  ) %>%
    answer_estimate_mean_t(
      answer = "Khoảng tin cậy 99\\% là"
    ) %>%
    cat() %>%
    print()
  print("===================================================")
  generate_data(
    estimate_mean_t,
    list(mean = 39.8, alpha = 0.01, n = 15, s = sqrt(0.144), mode="max")
  ) %>%
    answer_estimate_mean_t(
      answer = "Khoảng tin cậy 99\\% là"
    ) %>%
    cat() %>%
    print()
  print("===================================================")
  generate_data(
    estimate_var,
    list(n = 30, s = 0.032, alpha = 0.025, mode="two.side")
  ) %>%
    answer_estimate_var(
      answer = "Khoảng tin cậy 97,5\\% là:"
    ) %>%
    cat() %>%
    print()
  # Đáp số: 0.000649 - 0.001851
  print("===================================================")
  generate_data(
    estimate_var,
    list(n = 30, s = 0.032, alpha = 0.025, mode="min")
  ) %>%
    answer_estimate_var(
      answer = "Khoảng tin cậy 97,5\\% là:"
    ) %>%
    cat() %>%
    print()
  print("===================================================")
  generate_data(
    estimate_var,
    list(n = 30, s = 0.032, alpha = 0.025, mode="max")
  ) %>%
    answer_estimate_var(
      answer = "Khoảng tin cậy 97,5\\% là:"
    ) %>%
    cat() %>%
    print()
  print("===================================================")
  generate_data(
    estimate_prop,
    list(n = 100, f = 0.6, alpha = 0.1, mode="two.side")
  ) %>%
    answer_estimate_prop(
      answer = "Khoảng tin cậy 99\\% là:"
    ) %>%
    cat() %>%
    print()
  # Đáp số: 0.52 - 0.68
  print("===================================================")
  generate_data(
    estimate_prop,
    list(n = 100, f = 0.6, alpha = 0.1, mode="min")
  ) %>%
    answer_estimate_prop(
      answer = "Khoảng tin cậy 99\\% là:"
    ) %>%
    cat() %>%
    print()
  print("===================================================")
  generate_data(
    estimate_prop,
    list(n = 100, f = 0.6, alpha = 0.1, mode="max")
  ) %>%
    answer_estimate_prop(
      answer = "Khoảng tin cậy 99\\% là:"
    ) %>%
    cat() %>%
    print()
  print("===================================================")
  generate_data(
    sample_size_mean,
    list(sigma = 3, alpha = get_alpha(1.64), eps = 0.5)
  ) %>%
    answer_sample_size_mean(
      sd_symbol = "\\sigma",
      conclusion = "Vậy kích thước mẫu tối thiểu để thỏa mãn yêu cầu đề bài là %d"
    ) %>%
    cat() %>%
    print()
  # Đáp số: 96.826
  print("===================================================")
  generate_data(
    sample_size_prop_1,
    list(f = 0.64, alpha = get_alpha(1.64), eps = 0.02)
  ) %>%
    answer_sample_size_prop_1(
      conclusion = "Vậy kích thước mẫu tối thiểu để thỏa mãn yêu cầu đề bài là %d"
    ) %>%
    cat() %>%
    print()
  # Đáp số: 1549.2
  print("===================================================")
  generate_data(
    sample_size_prop_2,
    list(eps = 0.02, alpha = get_alpha(1.64))
  ) %>%
    answer_sample_size_prop_2(
      conclusion = "Vậy kích thước mẫu tối thiểu để thỏa mãn yêu cầu đề bài là %d"
    ) %>%
    cat() %>%
    print()
  # Đáp số: 1681
  expect_equal(1, 1)
})
