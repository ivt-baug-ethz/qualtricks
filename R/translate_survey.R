#' Translate the survey
#'
#' @param survey survey data.frame
#' @param type which labels should be used?
#' @param ...
#'
#' @return
#' @export
translate_survey <- function(survey, type = c("AnswerTranslation", "AnswerValue"), ...)
{
  type <- match.arg(type)
  labelr::label_df(survey, type = type, ...)
}
