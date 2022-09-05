#' Hàm này dùng để tạo template cho việc tạo đề thi bằng thongke.dapan
#' @export
init_template <- function() {
  print("Init single template")
  exam_name <- readline(prompt = "Exam name: ")
  dir.create(exam_name)
  setwd(exam_name)
  print("Create exam answer")
  file.create("exam_answer.Rmd")
  exam_answer_link <- "https://raw.githubusercontent.com/vungocbinh2009/thongke.dapan/master/exam_template/test_answer_template.rmd"
  exam_answer_content <- readLines(exam_answer_link, )
  writeLines(exam_answer_content, "exam_answer.Rmd")
  print("Create exam file")
  exam_word_link <- "https://github.com/vungocbinh2009/thongke.dapan/raw/master/exam_template/empty_file.docx"
  download.file(exam_word_link, "exam.docx", quiet = TRUE)
  setwd("../")
  print("Done")
}

init_multiple_template <- function() {
  print("Init multiple template")
  exam_name <- readline(prompt = "Exam name: ")
  dir.create(exam_name)
  setwd(exam_name)
  print("Create multiple exam answer")
  file.create("multi_answer_template.R")
  exam_answer_link <- "https://raw.githubusercontent.com/vungocbinh2009/thongke.dapan/master/exam_template/multi_answer_template.R"
  exam_answer_content <- readLines(exam_answer_link, )
  writeLines(exam_answer_content, "exam_answer.Rmd")
  print("Create exam file")
  exam_word_link <- "https://github.com/vungocbinh2009/thongke.dapan/raw/master/exam_template/empty_file.docx"
  download.file(exam_word_link, "exam.docx", quiet = TRUE)
  setwd("../")
  print("Done")
}