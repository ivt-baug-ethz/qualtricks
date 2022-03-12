#' Create a new contact in a mailing list
#'
#' @param mailinglistID check `qualtRics::all_mailinglists()`
#' @param body named list (careful with names, e.g. externalDataRef -> compare api documentation)
#'
#' @seealso <https://api.qualtrics.com/api-reference/b3A6NjEwMzA-create-contact>
#'
#' @return content of request as returned by `httr::content()`
#' @export
#'
#' @examples
#' \dontrun{
#' mailinglistID <- qualtRics::all_mailinglists()$id[1]
#' body <- list(firstName = "hello", lastName = "world", email = "hello@world.ch", externalDataRef = "E", language = "de", unsubscribed = FALSE)
#' create_contact(mailinglistID = mailinglistID, body = body)
#' }
create_contact <- function(mailinglistID, body)
{
  qualtRics:::assert_base_url()
  qualtRics:::assert_api_key()
  post_url <- generate_url_extended("createcontact", mailinglistID = mailinglistID)
  qualtrics_api_request_extended(verb = "POST", url = post_url, body = body, encode = "json")
}
