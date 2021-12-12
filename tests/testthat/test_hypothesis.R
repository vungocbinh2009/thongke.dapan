library(thongke)
library(thongke.dapan)
library(testthat)
library(magrittr)

test_that("Test các hàm answer_test_*", {
  generate_data(test_mean_norm,
                list(sigma = 5.2, alpha = 0.05, n = 100,
                 mean = 27.56, mean_0 = 26, mode = "neq")) %>%
    answer_test_mean_norm() %>%
    cat() %>%
    print()
  # Đáp số: T=3 - c=1.96 - Bác bỏ
  print("===================================================")
  data <- c(19, 18, 22, 20, 16, 25)
  generate_data(test_mean_t,
                list(mean = mean(data), mean_0 = 21.5, mode = "neq",
                  n = 6, alpha = 0.05, s = sqrt(var(data)))) %>%
    answer_test_mean_t() %>%
    cat() %>%
    print()
  # Đáp số: T=-1.16 - c=2.571 - Chấp nhận
  print("===================================================")
  generate_data(test_prop,
                list(f = 0.4, p_0 = 0.45, alpha = 0.05, n = 200, mode = "neq")) %>%
    answer_test_prop() %>%
    cat() %>%
    print()
  # Đáp số: T=-1.43, c=1.96, Chấp nhận
  print("===================================================")
  generate_data(test_goodness_of_fit,
                list(expected = c(100, 100, 100, 100, 100, 100),
                  actual = c(106, 92, 97, 105, 88, 112),
                  alpha = 0.05)) %>%
    answer_test_goodness_of_fit(statement = "Nội dung của $H_0$") %>%
    cat() %>%
    print()
  # Đáp số: T=4.22, c=11.070, Chấp nhận
  print("===================================================")
  generate_data(test_2_mean_norm,
                list(n1 = 40, n2 = 50, mean1 = 130, mean2 = 140,
                   sigma1 = sqrt(80), sigma2 = sqrt(100),
                   alpha = 0.01, mode = "neq")) %>%
    answer_test_2_mean_norm() %>%
    cat() %>%
    print()
  # Đáp số: T=5, c=2.58, Bác bỏ
  print("===================================================")
  generate_data(test_2_mean_t,
                list(mode = "neq", alpha = 0.01, mean1 = 4.8, n1 = 10,
                s1 = 1.1, mean2 = 4.3, n2 = 12, s2 = 0.9)) %>%
    answer_test_2_mean_t() %>%
    cat() %>%
    print()
  # Đáp số: T=1.174, c=2.845, Chấp nhận
  print("===================================================")
  generate_data(test_2_prop,
                list(f1 = 0.42, f2 = 0.46, n1 = 100, n2 = 200,
                  alpha = 0.05, mode = "neq")) %>%
    answer_test_2_prop() %>%
    cat() %>%
    print()
  # Đáp số: T=-0.66, c=1.96, Chấp nhận
  print("===================================================")
  generate_data(test_k_prop,
                list(m_i = c(79, 82, 77, 83, 76, 81),
                  n_i = c(100, 100, 100, 100, 100, 100),
                  alpha = 0.05)) %>%
    answer_test_k_prop(statement = "Nội dung của $H_0$") %>%
    cat() %>%
    print()
  # Đáp số: T=2.42. c=11.07. Chấp nhận
  print("===================================================")
  generate_data(test_independent,
                list(matrix = matrix(data = c(328, 122, 77, 33), ncol = 2, nrow = 2),
                  alpha = 0.05)) %>%
    answer_test_independent(statement = "Nội dung của $H_0$") %>%
    cat() %>%
    print()
  # Đáp số: T=0.368, c=3.841, Chấp nhận
  expect_equal(1, 1)
})
