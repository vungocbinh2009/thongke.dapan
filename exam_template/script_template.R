#' Đây là script dùng để xây dựng song song nhiều bộ đề thi, sử dụng thongke.dapan
library(thongke.dapan)
library(thongke)
library(whisker)
library(stringr)

generate_answer <- function(seed) {
  set.seed(seed)

  questions <- sprintf("\\textbf{%s}", c("Câu 1", "Câu 2"))
  answer_1 <- "Đáp án câu hỏi 1"
  answer_2 <- "Đáp án câu hỏi 2"
  # Thêm câu trả lời vào biến answer, theo đúng thứ tự câu hỏi trong biến question.
  answers <- c(answer_1, answer_2)
  return(list(
    questions = questions,
    answers = answers
  ))
}

generate_multi_exam <- function(numbers_of_exam = 2) {
  output_file <-  sprintf("output%d.tex", 1:numbers_of_exam)
  answer_title <- r"[
  \begin{center}
      \textbf{\large Đáp án đề thi một môn học nào đó}

      Môn học: Một môn học nào đó - Mã lớp học: 123456

      Thời gian: Ngày tận thế - Mã đề thi: %d
  \end{center}
  ]" |> sprintf(1:numbers_of_exam)
  seed_list <- sample(x = 2147483647, size = numbers_of_exam)

  sapply(1:numbers_of_exam, function(index) {
    qa <- generate_answer(seed_list[index])
    generate_one_exam(qa$questions, qa$answers, output_file[index], answer_title[index])
    generate_seed_table(seed_list)
  })
}

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

generate_seed_table <- function(seed_list) {
  data.frame(index = seq_along(seed_list), seed = seed_list) |> write.csv("seed_list.csv")
}

# Lệnh gọi hàm chính - gọi hàm generate_multi_exam.
generate_multi_exam(numbers_of_exam = 4)
