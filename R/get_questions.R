#' @export
get_questions <- function(survey_id, ...)
{
  base_url <- Sys.getenv("QUALTRICS_BASE_URL")
  key <- Sys.getenv("QUALTRICS_API_KEY")

  url <- glue::glue("{base_url}/API/v3/survey-definitions/{survey_id}/questions")

  response <- httr::VERB("GET", url, httr::add_headers("x-api-token" = key), httr::content_type("application/octet-stream"))

  out <- httr::content(response, "text", ...)
  jsonlite::prettify(out)
}
