#' Translate one answer vector
#'
#' @param v answer vector unquoted expression (usually a column from the survey data.frame)
#' @param from hint what v contains (if numeric -> AnswerValue, if main language to foreign -> Answer)
#' @param codebook see `get_codebook`
#'
#' @return
#' @export
translate_answer <- function(v, from = c("Answer", "AnswerValue"), codebook = survey$codebook)
{
  w <- rlang::enquo(v)
  if(is.null(name)) name <- rlang::quo_name(w)

  cw <-
    codebook %>%
    filter(DataExportTag == name) %>%
    select(to = factor_levels_t, from = factor_value)

  vt <- labelr:::mapvalues(v, from = cw$from, to = cw$to)
  vt
}
