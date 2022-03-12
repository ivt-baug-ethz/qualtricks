
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
?qualtRics:::qualtrics_api_request # httr request

# https://api.qualtrics.com/api-reference/YXBpOjYwOTE3-contacts
# create contact api -> post request

# update contact api -> in conjunction with generate_codes...
qualtRics::generate_url()





## How to add --data to the post request??



library(httr)
url <- "https://fra1.qualtrics.com/API/v3/mailinglists/ML_cROpOLdqoh22Tyu/contacts/"
url <- "http://httpbin.org/get"
r <- GET(url, add_headers(h))
r
headers(r)
content(r)

post_url <- "https://fra1.qualtrics.com/API/v3/mailinglists/ML_cROpOLdqoh22Tyu/contacts"
test <- "{}"
r <- POST(post_url, add_headers(h), body = body, encode = "json")
r
http_status(r)

post_url <- "https://ca1.qualtrics.com/API/v3/directories/directoryId/mailinglists/mailingListId/contacts"
headr <- add_headers("Content-Type" = "application/son", "X-API-TOKEN" = "")
b <- list(firstName = "string", lastName = "string", email = "string", phone = "string",
          extRef = "string", embeddedData = list(property1 = "string", property2 = "string"),
          language = "string", unsubscribed = TRUE)
r <- POST(post_url, headr, body = b)
r

