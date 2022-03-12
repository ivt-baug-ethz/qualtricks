generate_url_extended <- function(query, ...)
{
  args <- list(...)
  list2env(args, envir = environment())
  base_url <- Sys.getenv("QUALTRICS_BASE_URL")
  base_url <- ifelse(substring(base_url, nchar(base_url)) ==
                       "/", substr(base_url, 1, nchar(base_url) - 1), base_url)
  root_url <- glue::glue("https://{base_url}/API/v3")
  endpoint_template <-
    switch(query,
           createcontact = "{rooturl}/mailinglists/{mailinglistID}/contacts",
           updatecontact = "{rooturl}/mailinglists/{mailinglistID}/contacts/{contactID}")

  glue::glue(endpoint_template, rooturl = root_url, ...)
}
