#' Strips html tags
#'
#' @param x a string with html tags
#'
#' @references Taken from `qualtRics:::remove_html`
#'
#' @return
#' @export
strip_html <- function(x)
{
  stringr::str_remove_all(x, "<[^>]+>")
}




#' Make ugly names less ugly
#'
#' Usually applied to survey or col_map or something the like...
#'
#' @param x character vector
#'
#' @return
#' @export
name_parser <- function(x)
{
  x %>%
  tolower() %>%
  stringr::str_remove_all("\\(|\\)") %>%
  stringr::str_replace_all("[[:space:]]", "_")
}




#' Drop columns only containing NAs
#'
#' @param df data.frame
#'
#' @return
#' @export
drop_all_na <- function(df)
{
  df[colSums(is.na(df)) < nrow(df)]
}
