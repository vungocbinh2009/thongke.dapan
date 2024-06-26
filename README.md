# Table of Contents

  - [thongke.dapan](#thongke.dapan)
  - [Install](#install)
  - [How to use](#how-to-use)
      - [Descriptive statistics](#descriptive-statistics)
      - [Parameter estimation](#parameter-estimation)
      - [Hypothesis testing](#hypothesis-testing)
      - [Correlation and simple linear
        regression](#correlation-and-simple-linear-regression)
      - [Other features](#other-features)
  - [Note](#note)
  - [License](#license)

# thongke.dapan

![r-universe](https://vungocbinh2009.r-universe.dev/badges/thongke.dapan)

My R package, generate latex code to add to my exam template :)

# Install

Using r-universe:
```{r, eval=FALSE}
install.packages("thongke.dapan", repos = "https://vungocbinh2009.r-universe.dev")
```

Using devtools
``` {r, eval=FALSE}
# Install devtools
install.packages("devtools")
# Install thongke before thongke_dapan
devtools::install_github("vungocbinh2009/thongke")
# Install thongke.dapan
devtools::install_github("vungocbinh2009/thongke.dapan")
# Others package dependency: whisker, xtable
```

# How to use

## Descriptive statistics

``` r
library(thongke)
library(thongke.dapan)
library(testthat)
library(magrittr)
```

    ## 
    ## Attaching package: 'magrittr'

    ## The following objects are masked from 'package:testthat':
    ## 
    ##     equals, is_less_than, not

``` r
test_that("Test một số hàm trong file descriptive.R", {
  data <- rep(x = c(1, 2, 3, 4, 5), times = c(1, 2, 3, 4, 5))
  data %>% answer_mean(answer = "Giá trị trung bình là: ") %>% cat() %>% print()
  data %>% answer_var(answer = "Giá trị phương sai là: ") %>% cat() %>% print()
  data %>% answer_var(answer = "Giá trị phương sai là: ", with_mean = FALSE) %>% cat() %>% print()
  data %>% answer_sd(answer = "Giá trị độ lệch chuẩn là: ") %>% cat() %>% print()
  expect_equal(1, 1)
})
```

## Parameter estimation

``` r
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
```

## Hypothesis testing

``` r
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
```

## Correlation and simple linear regression

``` r
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
```

## Other features

``` r
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

  answer_1 <- linear_regression(
    x = c(400, 600, 500, 600, 400, 500),
    y = c(44, 47, 48, 48, 43, 46)
  ) %>%
    answer_linear_regression(
      intro = "Gọi $X$ và $Y$ lần lượt là ..."
    )
  # Đáp số: y=0.02x + 36, giá trị sách 700 trang là 50 nghìn
  answer_2 <- linear_regression_predict(
    x = c(400, 600, 500, 600, 400, 500),
    y = c(44, 47, 48, 48, 43, 46),
    value = 700
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
```

# Note

  - If you want to set comma as decimal seperator, add this code

<!-- end list -->

``` r
options(OutDec=",")
```

  - In latex, use `icomma` package to display number with comma as
    decimal seperator

# License

MIT License

Copyright (c) 2021-2022 vungocbinh2009

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
“Software”), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
