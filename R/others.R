#' Tạo đáp án thủ công
#'
#' Hàm này dùng để tạo đáp án tùy ý, theo nhu cầu của người dùng.
#' @param templates: Một vector chứa mẫu đáp án
#' @param scores: Một vector chứa điểm số cho từng mục.
#' @param data: Một list chứa dữ liệu để thêm vào template.
#' @import whisker
#' @export
answer_custom <- function (templates, scores, data) {
  scores_str <- paste(scores, "đ", sep = " ")
  answer_list <- NULL
  answer_list <- c(answer_list, paste(templates, "&", scores_str, sep = "\n"))
  template_full <- paste(answer_list, collapse = "\n\\\\\n")
  return(whisker.render(template_full, data))
}

#' Ghép đáp án
#'
#' Hàm này dùng để ghép đáp án từ nhiều hàm có sẵn trở thành 1 đáp án duy nhất
#' @param templates Một vector chứa các template cần ghép.
#' @export
answer_merge <- function(templates) {
  return(paste(templates, collapse = "\n\\\\\n"))
}