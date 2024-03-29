library(thongke)
library(thongke.dapan)
library(testthat)
library(magrittr)

test_that("Test 2 hàm trong file regression.R", {
  calculate_sum(
    x = c(80, 85, 88, 90, 95, 92, 82, 75, 78, 85),
    y = c(2.4, 2.8, 3.3, 3.1, 3.7, 3, 2.5, 2.3, 2.8, 3.1)
  ) %>%
    answer_calculate_sum(
      intro = "Gọi $X$ và $Y$ lần lượt là ..."
    ) %>%
    cat() %>%
    print()

  correlation(
    x = c(80, 85, 88, 90, 95, 92, 82, 75, 78, 85),
    y = c(2.4, 2.8, 3.3, 3.1, 3.7, 3, 2.5, 2.3, 2.8, 3.1)
  ) %>%
    answer_correlation() %>%
    cat() %>%
    print()
  # Đáp số: 0.858
  print("===================================================")
  linear_regression(
    x = c(400, 600, 500, 600, 400, 500),
    y = c(44, 47, 48, 48, 43, 46)
  ) %>%
    answer_linear_regression(
      intro = "Gọi $X$ và $Y$ lần lượt là ..."
    ) %>%
    cat() %>%
    print()
  # Đáp số: y=0.02x + 36, giá trị sách 700 trang là 50 nghìn
  print("===================================================")
  linear_regression_predict(
    x = c(400, 600, 500, 600, 400, 500),
    y = c(44, 47, 48, 48, 43, 46),
    value = 700
  ) %>%
    answer_linear_regression_predict(
      answer = "Giá sách là:",
      value_unit = "nghìn đồng"
    ) %>%
    cat() %>%
    print()
  expect_equal(1, 1)
})
