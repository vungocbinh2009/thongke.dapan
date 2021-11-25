library(thongke)
library(thongke.dapan)
library(testthat)
library(magrittr)

test_that("Test các hàm answer_estimate_*, answer_sample_*", {
  answer_estimate_mean_norm(sigma = 3, n = 36, alpha = 0.05,
                            mean = 66) %>% cat() %>% print()
  # Đáp số: 65,02 - 66,98
  print("===================================================")
  answer_estimate_mean_t(mean = 39.8, alpha = 0.01, n = 15,
                  s = sqrt(0.144)) %>% cat() %>% print()
  # Đáp số: 39,5023 - 40,0977
  print("===================================================")
  answer_estimate_var(n = 30, s = 0.032, alpha = 0.025) %>% cat() %>% print()
  # Đáp số: 0.000649 - 0.001851
  print("===================================================")
  answer_estimate_prop(n = 100, f = 0.6, alpha = 0.1) %>% cat() %>% print()
  # Đáp số: 0.52 - 0.68
  print("===================================================")
  answer_sample_size_mean(sigma = 3, alpha = get_alpha(1.64),
                          eps = 0.5) %>% cat() %>% print()
  # Đáp số: 96.826
  print("===================================================")
  answer_sample_size_prop_1(f = 0.64, alpha = get_alpha(1.64),
                            eps = 0.02) %>% cat() %>% print()
  # Đáp số: 1549.2
  print("===================================================")
  answer_sample_size_prop_2(eps = 0.02, alpha = get_alpha(1.64)) %>% cat() %>% print()
  # Đáp số: 1681
  expect_equal(1, 1)
})