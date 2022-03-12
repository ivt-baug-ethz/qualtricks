#' Update a contact in your mailing list
#'
#' @param mailinglistID check `qualtRics::all_mailinglists()`
#' @param contactID check `qualtRics::fetch_mailinglist()`
#' @param body named list (careful with names, e.g. externalDataReference -> does not match api documenation... internal strangeness)

#' @seealso <https://api.qualtrics.com/api-reference/b3A6NjEwMzA-create-contact>
#'
#' @return content of request as returned by `httr::content()`
#' @export
#'
#' @examples
#' \dontrun{
#' mailinglist <- qualtRics::all_mailinglists()
#' mailinglistID <- mailinglist$id[1]
#' m <- fetch_mailinglist(mailinglistID)
#' contactID <- m$id[1]
#' code <- generate_codes(n = 1)$code
#' body <- list(firstName = "hello", lastName = "world", externalDataReference = code)
#' update_contact(mailinglistID = mailinglistID, contactID = contactID, body = body)
#' fetch_mailinglist(mailinglistID)
#' }
update_contact <- function(mailinglistID, contactID, body)
{
  qualtRics:::assert_base_url()
  qualtRics:::assert_api_key()
  put_url <- generate_url_extended("updatecontact", mailinglistID = mailinglistID, contactID = contactID)
  qualtrics_api_request_extended(verb = "PUT", url = put_url, body = body, encode = "json")
}


## content(res)
