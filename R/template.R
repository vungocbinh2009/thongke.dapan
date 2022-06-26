#' Hàm này dùng để tạo template cho việc tạo đề thi bằng thongke.dapan
#' @export
init_template <- function() {
  file.create(c("exam.docx", "exam_answer.Rmd"))
  link <- "https://raw.githubusercontent.com/vungocbinh2009/thongke.dapan/master/exam_template/test_answer_template.rmd"
  exam_answer_content <- readLines(link)
  writeLines(exam_answer_content, "exam_answer.Rmd")
}