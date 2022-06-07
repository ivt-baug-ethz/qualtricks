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
  stringr::str_remove_all(string, "<[^>]+>")
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
  names(x) %>%
  tolower() %>%
  stringr::str_remove_all("\\(|\\)") %>%
  stringr::str_replace_all("[[:space:]]", "_")
}
