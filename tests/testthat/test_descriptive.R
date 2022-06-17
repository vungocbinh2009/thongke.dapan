library(thongke)
library(thongke.dapan)
library(testthat)
library(magrittr)

test_that("Test một số hàm trong file descriptive.R", {
  data <- rep(x = c(1, 2, 3, 4, 5), times = c(1, 2, 3, 4, 5))
  data %>% answer_mean(answer = "Giá trị trung bình là: ") %>% cat() %>% print()
  data %>% answer_var(answer = "Giá trị phương sai là: ") %>% cat() %>% print()
  data %>% answer_var(answer = "Giá trị phương sai là: ", with_mean = FALSE) %>% cat() %>% print()
  data %>% answer_sd(answer = "Giá trị độ lệch chuẩn là: ") %>% cat() %>% print()
  expect_equal(1, 1)
})
