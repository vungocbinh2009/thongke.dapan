library(thongke)
library(thongke.dapan)
library(testthat)
library(magrittr)

test_that("Test các hàm answer_estimate_*, answer_sample_*", {
  generate_data(estimate_mean_norm,
                list(sigma = 3, n = 36, alpha = 0.05, mean = 66)) %>%
    answer_estimate_mean_norm() %>%
    cat() %>%
    print()
  # Đáp số: 65,02 - 66,98
  print("===================================================")
  generate_data(estimate_mean_t,
                list(mean = 39.8, alpha = 0.01, n = 15, s = sqrt(0.144))) %>%
    answer_estimate_mean_t() %>%
    cat() %>%
    print()
  # Đáp số: 39,5023 - 40,0977
  print("===================================================")
  generate_data(estimate_var,
                list(n = 30, s = 0.032, alpha = 0.025)) %>%
    answer_estimate_var() %>%
    cat() %>%
    print()
  # Đáp số: 0.000649 - 0.001851
  print("===================================================")
  generate_data(estimate_prop,
                list(n = 100, f = 0.6, alpha = 0.1)) %>%
    answer_estimate_prop() %>%
    cat() %>%
    print()
  # Đáp số: 0.52 - 0.68
  print("===================================================")
  generate_data(sample_size_mean,
                list(sigma = 3, alpha = get_alpha(1.64), eps = 0.5)) %>%
    answer_sample_size_mean() %>%
    cat() %>%
    print()
  # Đáp số: 96.826
  print("===================================================")
  generate_data(sample_size_prop_1,
                list(f = 0.64, alpha = get_alpha(1.64), eps = 0.02)) %>%
    answer_sample_size_prop_1() %>%
    cat() %>%
    print()
  # Đáp số: 1549.2
  print("===================================================")
  generate_data(sample_size_prop_2,
                list(eps = 0.02, alpha = get_alpha(1.64))) %>%
    answer_sample_size_prop_2() %>%
    cat() %>%
    print()
  # Đáp số: 1681
  expect_equal(1, 1)
})
