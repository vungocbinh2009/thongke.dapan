library(thongke)
library(thongke.dapan)
library(testthat)
library(magrittr)

test_that("Test các hàm answer_test_*", {
  test_mean_norm(sigma = 5.2, alpha = 0.05, n = 100, mean = 27.56, mean_0 = 26, alternative = "neq") %>%
    answer_test_mean_norm(
      intro = "Gọi $\\mu$ là ...",
      sd_symbol = "\\sigma",
      conclusion_h0 = "Chưa đủ cơ sở để cho rằng ...",
      conclusion_h1 = "Có thể cho rằng ..."
    ) %>%
    cat() %>%
    print()
  # Đáp số: T=3 - c=1.96 - Bác bỏ
  print("===================================================")
  data <- c(19, 18, 22, 20, 16, 25)
  test_mean_t(mean = mean(data), mean_0 = 21.5, alternative = "neq", n = 6, alpha = 0.05, s = sqrt(var(data))) %>%
    answer_test_mean_t(
      intro = "Gọi $\\mu$ là ...",
      conclusion_h0 = "Chưa đủ cơ sở để cho rằng ...",
      conclusion_h1 = "Có thể cho rằng ..."
    ) %>%
    cat() %>%
    print()
  # Đáp số: T=-1.16 - c=2.571 - Chấp nhận
  print("===================================================")
  test_prop(f = 0.4, p_0 = 0.45, alpha = 0.05, n = 200, alternative = "neq") %>%
    answer_test_prop(
      intro = "Gọi $p$ là ...",
      conclusion_h0 = "Chưa đủ cơ sở để cho rằng ...",
      conclusion_h1 = "Có thể cho rằng ..."
    ) %>%
    cat() %>%
    print()
  # Đáp số: T=-1.43, c=1.96, Chấp nhận
  print("===================================================")
  test_goodness_of_fit(
    expected = c(100, 100, 100, 100, 100, 100),
    actual = c(106, 92, 97, 105, 88, 112),
    alpha = 0.05
  ) %>%
    answer_test_goodness_of_fit(
      h0 = "Nội dung của $H_0$",
      col_names = c("Mặt 1", "Mặt 2", "Mặt 3", "Mặt 4", "Mặt 5", "Mặt 6"),
      conclusion_h0 = "Chưa đủ cơ sở để cho rằng ...",
      conclusion_h1 = "Có thể cho rằng ..."
    ) %>%
    cat() %>%
    print()
  # Đáp số: T=4.22, c=11.070, Chấp nhận
  print("===================================================")
  test_2_mean_norm(
    n1 = 40, n2 = 50, mean1 = 130, mean2 = 140,
    sigma1 = sqrt(80), sigma2 = sqrt(100),
    alpha = 0.01, alternative = "neq"
  ) %>%
    answer_test_2_mean_norm(
      intro = "Gọi $\\mu_1$ và $\\mu_2$ lần lượt là ...",
      sd_symbol = "\\sigma",
      conclusion_h0 = "Chưa đủ cơ sở để cho rằng ...",
      conclusion_h1 = "Có thể cho rằng ..."
    ) %>%
    cat() %>%
    print()
  # Đáp số: T=5, c=2.58, Bác bỏ
  print("===================================================")
  test_2_mean_t(
    alternative = "neq", alpha = 0.01, mean1 = 4.8, n1 = 10,
    s1 = 1.1, mean2 = 4.3, n2 = 12, s2 = 0.9
  ) %>%
    answer_test_2_mean_t(
      intro = "Gọi $\\mu_1$ và $\\mu_2$ lần lượt là ...",
      conclusion_h0 = "Chưa đủ cơ sở để cho rằng ...",
      conclusion_h1 = "Có thể cho rằng ..."
    ) %>%
    cat() %>%
    print()
  # Đáp số: T=1.174, c=2.845, Chấp nhận
  print("===================================================")
  test_2_prop(
    f1 = 0.42, f2 = 0.46, n1 = 100, n2 = 200,
    alpha = 0.05, alternative = "neq"
  ) %>%
    answer_test_2_prop(
      intro = "Gọi $p_1$ và $p_2$ lần lượt là ...",
      conclusion_h0 = "Chưa đủ cơ sở để cho rằng ...",
      conclusion_h1 = "Có thể cho rằng ..."
    ) %>%
    cat() %>%
    print()
  # Đáp số: T=-0.66, c=1.96, Chấp nhận
  print("===================================================")
  test_k_prop(
    m_i = c(79, 82, 77, 83, 76, 81),
    n_i = c(100, 100, 100, 100, 100, 100),
    alpha = 0.05
  ) %>%
    answer_test_k_prop(
      h0 = "Nội dung của $H_0$",
      col_names = c("A", "B", "C", "D", "E", "F"),
      row_names = c("Có A", "Không A"),
      conclusion_h0 = "Chưa đủ cơ sở để cho rằng ...",
      conclusion_h1 = "Có thể cho rằng ..."
    ) %>%
    cat() %>%
    print()
  # Đáp số: T=2.42. c=11.07. Chấp nhận
  print("===================================================")
  test_independent(
    matrix = matrix(data = c(328, 122, 77, 33), ncol = 2, nrow = 2),
    alpha = 0.05
  ) %>%
    answer_test_independent(
      h0 = "Nội dung của $H_0$",
      col_names = c("Loại 1", "Loại 2"),
      row_names = c("Loại A", "Loại B"),
      conclusion_h0 = "Chưa đủ cơ sở để cho rằng ...",
      conclusion_h1 = "Có thể cho rằng ..."
    ) %>%
    cat() %>%
    print()
  # Đáp số: T=0.368, c=3.841, Chấp nhận
  expect_equal(1, 1)
})
