#' Get labels from data.frame
#'
#' Usually the df is retrieved via `qualtRics::fetch_survey()`
#'
#' @param df data.frame
#' @param ... passed to `Hmisc::label()`
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{qualtRics::all_surveys()$id[4] %>% qualtRics::fetch_survey() %>% get_label()}
get_label <- function(df, ...) {
  Hmisc::label(df, ...)
}
