# thongke.dapan

My R package, generate latex code to add to my exam template :) 

# Install
```r
# Install devtools
install.packages("devtools")
# Install thongke before thongke_dapan
devtools::install_github("vungocbinh2009/thongke")
# Install thongke.dapan
devtools::install_github("vungocbinh2009/thongke_dapan")
# Others package dependency: whisker, here, xtable
```
# How to use
```r
answer_estimate_mean_norm(sigma = 3, n = 36, alpha = 0.05,
                            mean = 66) %>% cat() %>% print()
  # Đáp số: 65,02 - 66,98
  answer_estimate_mean_t(mean = 39.8, alpha = 0.01, n = 15,
                  s = sqrt(0.144)) %>% cat() %>% print()
  # Đáp số: 39,5023 - 40,0977
  answer_estimate_var(n = 30, s = 0.032, alpha = 0.025) %>% cat() %>% print()
  # Đáp số: 0.000649 - 0.001851
  answer_estimate_prop(n = 100, f = 0.6, alpha = 0.1) %>% cat() %>% print()
  # Đáp số: 0.52 - 0.68
  answer_sample_size_mean(sigma = 3, alpha = get_alpha(1.64),
                          eps = 0.5) %>% cat() %>% print()
  # Đáp số: 96.826
  answer_sample_size_prop_1(f = 0.64, alpha = get_alpha(1.64),
                            eps = 0.02) %>% cat() %>% print()
  # Đáp số: 1549.2
  answer_sample_size_prop_2(eps = 0.02, alpha = get_alpha(1.64)) %>% cat() %>% print()
  # Đáp số: 1681
  answer_test_mean_norm(sigma = 5.2, alpha = 0.05, n = 100,
                 mean = 27.56, mean_0 = 26, mode = "neq") %>% cat() %>% print()
  # Đáp số: T=3 - c=1.96 - Bác bỏ
  data <- c(19, 18, 22, 20, 16, 25)
  answer_test_mean_t(mean = mean(data), mean_0 = 21.5, mode = "neq",
    n = 6, alpha = 0.05, s = sqrt(var(data))) %>% cat() %>% print()
  # Đáp số: T=-1.16 - c=2.571 - Chấp nhận
  answer_test_prop(f = 0.4, p_0 = 0.45, alpha = 0.05, n = 200,
                   mode = "neq") %>% cat() %>% print()
  # Đáp số: T=-1.43, c=1.96, Chấp nhận
  answer_test_goodness_of_fit(statement = "Nội dung của $H_0$",
                              expected = c(100, 100, 100, 100, 100, 100),
                              actual = c(106, 92, 97, 105, 88, 112),
                              alpha = 0.05) %>% cat() %>% print()
  # Đáp số: T=4.22, c=11.070, Chấp nhận
  answer_test_2_mean_norm(n1 = 40, n2 = 50, mean1 = 130, mean2 = 140,
                   sigma1 = sqrt(80), sigma2 = sqrt(100),
                   alpha = 0.01, mode = "neq") %>% cat() %>% print()
  # Đáp số: T=5, c=2.58, Bác bỏ
  answer_test_2_mean_t(mode = "neq", alpha = 0.01, mean1 = 4.8, n1 = 10,
                s1 = 1.1, mean2 = 4.3, n2 = 12, s2 = 0.9) %>% cat() %>% print()
  # Đáp số: T=1.174, c=2.845, Chấp nhận
  answer_test_2_prop(f1 = 0.42, f2 = 0.46, n1 = 100, n2 = 200,
              alpha = 0.05, mode = "neq") %>% cat() %>% print()
  # Đáp số: T=-0.66, c=1.96, Chấp nhận
  answer_test_k_prop(statement = "Nội dung của $H_0$",
                     m_i = c(79, 82, 77, 83, 76, 81),
                     n_i = c(100, 100, 100, 100, 100, 100),
                     alpha = 0.05) %>% cat() %>% print()
  # Đáp số: T=2.42. c=11.07. Chấp nhận
  answer_test_independent(statement = "Nội dung của $H_0$",
                           matrix = matrix(data = c(328, 122, 77, 33), ncol = 2, nrow = 2),
                          alpha = 0.05) %>% cat() %>% print()
  # Đáp số: T=0.368, c=3.841, Chấp nhận
  answer_correlation(x = c(80, 85, 88, 90, 95, 92, 82, 75, 78, 85),
              y = c(2.4, 2.8, 3.3, 3.1, 3.7, 3, 2.5, 2.3, 2.8, 3.1)) %>% cat() %>% print()
  # Đáp số: 0.858
  answer_linear_regression(x = c(400, 600, 500, 600, 400, 500),
                    y = c(44, 47, 48, 48, 43, 46), value = 700) %>% cat() %>% print()
  # Đáp số: y=0.02x + 36, giá trị sách 700 trang là 50 nghìn
```
# Note
+ If you want to set comma as decimal seperator, add this code
```r
options(OutDec=",")
```
+ In latex, use ``icomma`` package to display number with comma as decimal seperator


License: MIT License
