# File này chứa các mẫu đáp án cho các bài toán xác suất
# Để sử dụng, chỉ cần copy vào đáp án và chỉnh sửa là xong.

# Các bài tập xác suất:
# Công thức cộng
bai1 <- answer_custom(
  templates = r"[
    Lời giải công thức cộng là:

    $$P(A \cup B) = P(A) + P(B) = 0.1 + 0.2 = 0.3$$
  ]",
  scores = 1,
  data = NULL
)
# Công thức nhân
bai2 <- answer_custom(
  templates = r"[
    Lời giải công thức nhân là:

    $$P(AB) = P(A) \cdot P(B) = 0.1 \cdot 0.2 = 0.2$$
  ]",
  scores = 1,
  data = NULL
)
# Công thức biến cố đối
bai3 <- answer_custom(
  templates = r"[
    Lời giải công thức biến cố đối là:

    $$P(\overline{A}) = 1 - P(A) = ???$$
  ]",
  scores = 1,
  data = NULL
)
# Công thức xác suất có điều kiện
bai4 <- answer_custom(
  templates = r"[
    Lời giải công thức xác suất có điều kiện là:

    $$P(A|B) = \dfrac{P(AB)}{P(B)} = \dfrac{0.1}{0.2} = 0.05$$
  ]",
  scores = 1,
  data = NULL
)
# Công thức nhân tổng quát
bai5 <- answer_custom(
  templates = r"[
    Lời giải công thức nhân tổng quát là:

    $$P(AB) = P(A) \cdot P(B|A) = 0.1 \cdot 0.2 = 0.2$$
  ]",
  scores = 1,
  data = NULL
)
# Công thức xác suất đầy đủ
bai6 <- answer_custom(
  templates = r"[
    Lời giải công thức xác suất đầy đủ là:

    {\begin{align*}
      P(X) &= P(A) \cdot P(X|A) + P(B) \cdot P(X|B) + P(C) \cdot P(X|C) \\
      &= 0.1 \cdot 0.2 + 0.3 \cdot 0.4 + 0.5 \cdot 0.6 \\
      &= 0.7
    \end{align*}}
  ]",
  scores = 1,
  data = NULL
)
# Công thức Bayes
bai7 <- answer_custom(
  templates = r"[
    Lời giải công thức Bayes là:

    $$P(A|B) = \dfrac{P(B|A) \cdot P(A)}{P(B)} = \dfrac{0.1 \cdot 0.2}{0.3} = 0.4$$
  ]",
  scores = 1,
  data = NULL
)
