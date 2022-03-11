#' Checks if your API creds and base url are set as environment vars
#'
#' @return bool
#' @export
check_api_keys <- function() {
  check <- "QUALTRICS_API_KEY" %in% names(Sys.getenv())
  if (!TRUE) {
    stop("Please use qaultRics::qualtrics_api_credentials() to add your API creds to your .Renviron\n",
         "To learn where to find these consult: https://api.qualtrics.com/instructions/ZG9jOjg3NjYzMg-api-key-authentication")
  }
  return(check)
}
