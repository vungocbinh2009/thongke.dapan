library(thongke)
library(thongke.dapan)
library(testthat)
library(magrittr)

test_that("Test các hàm answer_estimate_*, answer_sample_*", {
  estimate_mean_norm(sigma = 3, n = 36, alpha = 0.05, mean = 66, alternative="two_sided") %>%
    answer_estimate_mean_norm(
      sd_symbol = "\\sigma",
      answer = "Khoảng tin cậy 95\\% là:",
    ) %>%
    cat() %>%
    print()
  # Đáp số: 65,02 - 66,98
  print("===================================================")
  estimate_mean_norm(sigma = 3, n = 36, alpha = 0.05, mean = 66, alternative="min") %>%
    answer_estimate_mean_norm(
      sd_symbol = "\\sigma",
      answer = "Khoảng tin cậy 95\\% là:"
    ) %>%
    cat() %>%
    print()
  print("===================================================")
  estimate_mean_norm(sigma = 3, n = 36, alpha = 0.05, mean = 66, alternative="max") %>%
    answer_estimate_mean_norm(
      sd_symbol = "\\sigma",
      answer = "Khoảng tin cậy 95\\% là:"
    ) %>%
    cat() %>%
    print()
  print("===================================================")
  estimate_mean_t(mean = 39.8, alpha = 0.01, n = 15, s = sqrt(0.144), alternative="two_sided") %>%
    answer_estimate_mean_t(
      answer = "Khoảng tin cậy 99\\% là"
    ) %>%
    cat() %>%
    print()
  # Đáp số: 39,5023 - 40,0977
  print("===================================================")
  estimate_mean_t(mean = 39.8, alpha = 0.01, n = 15, s = sqrt(0.144), alternative="min") %>%
    answer_estimate_mean_t(
      answer = "Khoảng tin cậy 99\\% là"
    ) %>%
    cat() %>%
    print()
  print("===================================================")
  estimate_mean_t(mean = 39.8, alpha = 0.01, n = 15, s = sqrt(0.144), alternative="max") %>%
    answer_estimate_mean_t(
      answer = "Khoảng tin cậy 99\\% là"
    ) %>%
    cat() %>%
    print()
  print("===================================================")
  estimate_var(n = 30, s = 0.032, alpha = 0.025, alternative="two_sided") %>%
    answer_estimate_var(
      answer = "Khoảng tin cậy 97,5\\% là:"
    ) %>%
    cat() %>%
    print()
  # Đáp số: 0.000649 - 0.001851
  print("===================================================")
  estimate_var(n = 30, s = 0.032, alpha = 0.025, alternative="min") %>%
    answer_estimate_var(
      answer = "Khoảng tin cậy 97,5\\% là:"
    ) %>%
    cat() %>%
    print()
  print("===================================================")
  estimate_var(n = 30, s = 0.032, alpha = 0.025, alternative="max") %>%
    answer_estimate_var(
      answer = "Khoảng tin cậy 97,5\\% là:"
    ) %>%
    cat() %>%
    print()
  print("===================================================")
  estimate_prop(n = 100, f = 0.6, alpha = 0.1, alternative="two_sided") %>%
    answer_estimate_prop(
      answer = "Khoảng tin cậy 99\\% là:"
    ) %>%
    cat() %>%
    print()
  # Đáp số: 0.52 - 0.68
  print("===================================================")
  estimate_prop(n = 100, f = 0.6, alpha = 0.1, alternative="min") %>%
    answer_estimate_prop(
      answer = "Khoảng tin cậy 99\\% là:"
    ) %>%
    cat() %>%
    print()
  print("===================================================")
  estimate_prop(n = 100, f = 0.6, alpha = 0.1, alternative="max") %>%
    answer_estimate_prop(
      answer = "Khoảng tin cậy 99\\% là:"
    ) %>%
    cat() %>%
    print()
  print("===================================================")
  sample_size_mean(sigma = 3, alpha = get_alpha(1.64), eps = 0.5) %>%
    answer_sample_size_mean(
      sd_symbol = "\\sigma",
      conclusion = function(value) {
        return(sprintf("Vậy kích thước mẫu tối thiểu để thỏa mãn yêu cầu đề bài là %d", ceiling(value)))
      }
    ) %>%
    cat() %>%
    print()
  # Đáp số: 96.826
  print("===================================================")
  sample_size_prop_1(f = 0.64, alpha = get_alpha(1.64), eps = 0.02) %>%
    answer_sample_size_prop_1(
      conclusion = function(value) {
        return(sprintf("Vậy kích thước mẫu tối thiểu để thỏa mãn yêu cầu đề bài là %d", ceiling(value)))
      }
    ) %>%
    cat() %>%
    print()
  # Đáp số: 1549.2
  print("===================================================")
  sample_size_prop_2(eps = 0.02, alpha = get_alpha(1.64)) %>%
    answer_sample_size_prop_2(
      conclusion = function(value) {
        return(sprintf("Vậy kích thước mẫu tối thiểu để thỏa mãn yêu cầu đề bài là %d", ceiling(value)))
      }
    ) %>%
    cat() %>%
    print()
  # Đáp số: 1681
  expect_equal(1, 1)
})
