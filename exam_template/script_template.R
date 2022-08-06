#' Đây là script dùng để xây dựng song song nhiều bộ đề thi, sử dụng thongke.dapan
library(thongke.dapan)
library(thongke)
library(whisker)
library(stringr)

#' Hàm này dùng để tạo các đáp án câu hỏi trong 1 đề thi
generate_answer <- function(seed) {
  #' Đặt seed
  set.seed(seed)

  #' Đoạn code này dùng để tạo đáp án. Bạn có thể sử dụng thư viện thongke và thongke.dapan để xây dựng đáp án.
  questions <- sprintf("\\textbf{%s}", c("Câu 1", "Câu 2"))
  answer_1 <- "Đáp án câu hỏi 1"
  answer_2 <- "Đáp án câu hỏi 2"
  answers <- c(answer_1, answer_2)

  #' Hàm này dùng để chọn ngẫu nhiên các câu hỏi từ tập hợp các câu hỏi cho trước. Bạn cần chỉnh sửa:
  #' - size để giới hạn số câu hỏi trong đề thi.
  index <- sample(seq_along(answers_full), size=length(answer_full), replace = FALSE)
  index_sorted <- sort(index, decreasing = FALSE)

  #' Xây dựng các bộ đề thi và trả về kết quả.
  answers <- answers[index_sorted]
  questions <- questions[index_sorted]
  return(list(
    questions = questions,
    answers = answers
  ))
}

#' Hàm này dùng để xây dựng một đề thi - bạn không cần chỉnh sửa gì ở hàm này.
generate_one_exam <- function(questions, answers, output_file, answer_title) {
  answer_list_template <- readLines("https://raw.githubusercontent.com/vungocbinh2009/thongke.dapan/master/exam_template/answer_list_template.mustache")
  answer_template <- readLines("https://raw.githubusercontent.com/vungocbinh2009/thongke.dapan/master/exam_template/answer_template.mustache")
  rendered_answer_list <- NULL
  for (i in seq_along(questions)) {
    data <- list(
      question = questions[i],
      answer = answers[i]
    )
    rendered_answer <- whisker.render(answer_template, data)
    rendered_answer <- str_replace_all(rendered_answer, "\n\t\t", "\n")
    rendered_answer <- str_replace_all(rendered_answer, "\n", "\n\t\t")
    rendered_answer_list <- c(rendered_answer_list, rendered_answer)
  }
  data <- list(
    answer_list = iteratelist(rendered_answer_list),
    answer_title = answer_title
  )
  rendered_exam <- whisker.render(answer_list_template, data)
  writeLines(rendered_exam, output_file)

  # Compile latex file to pdf
  tools::texi2dvi(output_file, pdf = TRUE, clean = TRUE)
}

# Hàm này dùng để tạo file lưu lại các seed đã sử dụng.
generate_seed_table <- function(seed_list) {
  data.frame(index = seq_along(seed_list), seed = seed_list) |> write.csv("seed_list.csv")
}

#' Hàm này dùng để tạo nhiều đề thi, là hàm sẽ được gọi để tạo đề thi.
generate_multi_exam <- function(numbers_of_exam = 2) {
  #' Output file - Bạn có thể sửa lại tên file cho phù hợp.
  output_file <-  sprintf("Đáp án đề %d.tex", 1:numbers_of_exam)

  #' Answer title - Bạn có thể chỉnh sửa lại cho phù hợp
  answer_title <- r"[
  \begin{center}
      \textbf{\large Đáp án đề thi một môn học nào đó}

      Môn học: Một môn học nào đó - Mã lớp học: 123456

      Thời gian: Ngày tận thế - Mã đề thi: %d
  \end{center}
  ]" |> sprintf(1:numbers_of_exam)

  #' Câu lệnh này tạo seed để phục vụ cho việc xây dựng dữ liệu ngẫu nhiên.
  seed_list <- sample(x = 2147483647, size = numbers_of_exam)

  #' Hàm này tạo các đề thi, dựa vào các hàm con.
  sapply(1:numbers_of_exam, function(index) {
    qa <- generate_answer(seed_list[index])
    generate_one_exam(qa$questions, qa$answers, output_file[index], answer_title[index])
    generate_seed_table(seed_list)
  })
}

# Lệnh gọi hàm chính - gọi hàm generate_multi_exam - Bạn có thể set.seed để cố định kết quả, nếu muốn.
# Lời khuyên: Nên generate số lượng đề gấp rưỡi hoặc gấp đôi số bạn cần, để có thể loại bỏ một số đề không hợp lệ.
generate_multi_exam(numbers_of_exam = 4)
