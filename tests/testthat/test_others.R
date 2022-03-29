library(thongke)
library(thongke.dapan)
library(testthat)
library(magrittr)

test_that("Test một số hàm trong file others.R", {
  # You can create custom answer
  answer_custom(
    templates = c(
      "Đáp án 1 {{data1}}",
      "Đáp án 2 {{data2}}",
      "Đáp án 3 {{data3}}"),
    scores = c(1.25, 0.5, 1),
    data = list(
      data1 = "Haha",
      data2 = "hihi",
      data3 = "huhu"
    )
  ) %>%
    cat() %>%
    print()

  answer_1 <- generate_data(
    linear_regression,
    list(
      x = c(400, 600, 500, 600, 400, 500),
      y = c(44, 47, 48, 48, 43, 46)
    )
  ) %>%
    answer_linear_regression(
      intro = "Gọi $X$ và $Y$ lần lượt là ..."
    )
  # Đáp số: y=0.02x + 36, giá trị sách 700 trang là 50 nghìn
  answer_2 <- generate_data(
    linear_regression_predict,
    list(
      x = c(400, 600, 500, 600, 400, 500),
      y = c(44, 47, 48, 48, 43, 46),
      value = 700
    )
  ) %>%
    answer_linear_regression_predict(
      answer = "Giá sách là:",
      value_unit = "nghìn đồng"
    )

  # You can also merge answers too.
  answer_merge(c(answer_1, answer_2)) %>%
    cat() %>%
    print()
  expect_equal(1, 1)
})
