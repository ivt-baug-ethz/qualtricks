qualtrics_api_request_extended <- function(verb = c("GET", "POST", "PUT", "DELETE"), url = url, body = NULL, ...)
{
  verb <- match.arg(verb)
  headers <- qualtRics:::construct_header(Sys.getenv("QUALTRICS_API_KEY"))
  res <- httr::VERB(verb, url = url, httr::add_headers(headers), body = body, ...)
  cnt <- qualtRics:::qualtrics_response_codes(res)
  if(cnt$OK) {
    w <- qualtRics:::check_for_warnings(cnt)
    return(cnt$content)
  }
}
