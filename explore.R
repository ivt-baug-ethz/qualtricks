
library(qualtRics)

help(package = "qualtRics")

id <- all_surveys()$id[4]
test <- fetch_survey(id)

# recode values

survey_questions(id)
extract_colmap(test)
md <- metadata(id)
q <- md$questions
names(q)
q$QID1

column_map(id)

fetch_description(id)
metadata(id)
survey_questions(id)

fetch_survey(id, add_column_map = T, add_var_labels = T) %>% View()
fetch_survey(id, label = F, convert = F) %>% View()








# mailing list

all_mailinglists()
fetch_mailinglist("ML_cROpOLdqoh22Tyu")
debugonce(fetch_mailinglist) # leverages list contacts api
?qualtRics::generate_url
?qualtRics::qualtrics_api_request # httr request

# https://api.qualtrics.com/api-reference/YXBpOjYwOTE3-contacts
# create contact api -> post request

# update contact api -> in conjunction with generate_codes...
qualtRics::generate_url()

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
           createdirectorycontact = "{rooturl}/mailinglists/{mailinglistID}/contacts")

  glue::glue(endpoint_template, rooturl = root_url, ...)
}


create_contact <- function(mailinglistID, body)
{
  qualtRics::assert_base_url()
  qualtRics::assert_api_key()
  post_url <- generate_url_extended("createdirectorycontact", mailinglistID = mailinglistID)
  qualtRics::qualtrics_api_request(verb = "POST", url = post_url, body = body)
}


body <- list(firstName = "Blobb", lastName = "Blubb", email = "blobb@blubb.ch", externalDataReference = "E", language = "de", unsubscribed = FALSE)
create_contact("ML_cROpOLdqoh22Tyu", body = body)
debugonce(create_contact)
