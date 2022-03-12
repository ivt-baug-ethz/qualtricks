# Email <- R6

Email <-
  R6::R6Class(
    "Email",
    private = list(.creds = NULL),
    public = list(
      mailinglist = NULL,
      email = NULL,
      user = NA_character_,
      provider = NA_character_,
      host = NA_character_,
      port = 587,
      use_ssl = TRUE,
      initialize = function(mailinglist = NULL, email = NULL, user = "daniehei",
                            provider = "outlook", host = "mail.ethz.ch", port = 587,
                            use_ssl = TRUE) {
        self$mailinglist <- mailinglist
        self$email <- email
        self$user <- user
        self$provider <- provider
        self$host <- host
        self$port <- port
        self$use_ssl <- use_ssl
        private$.creds <- blastula::creds(user, provider, host, port, use_ssl)
      },
      validate = function() {
        if(!is.null(self$mailinglist)) {
          assertthat::assert_that(is.data.frame(self$mailinglist),
                                  msg = "mailinglist must be a data.frame")
          assertthat::assert_that(all(c("firstName", "lastName", "email") %in% names(self$mailinglist)),
                                  msg = "firstName, lastName, and email must be present in mailinglist")
        }
      },
      ## TODO: document and replace firstName and lastName in self$mail ######################################
      write_mail = function(...) {
        self$email <- blastula::compose_email(...) ###
        invisible(self)
      },
      send_mail = function(...) {
        assertthat::assert_that(!is.null(self$email),
                                msg = "You have to write the mail first!")
        assertthat::assert_that(!is.null(self$mailinglist),
                                msg = "You have to provide a mailing list! Consider qualtRics::fetch_mailinglist()")
        self$validate()

        ## iterate over mailinglist
        apply(X = self$mailinglist, MARGIN = 1, FUN = function(x) {
          self$email %>%
            blastula::smtp_send(to = x["email"], credentials = private$.creds, ...)
        })

        invisible(self)
      },
      print = function() {
        assertthat::assert_that(!is.null(self$email),
                                msg = "You have to write the mail first!")
        print(self$email)
        invisible(self)
      }
    )
  )


qr <- add_image(file = "/Users/schnickdani/Downloads/SV_6u4YzY24OAfhWfQ-qrcode.png", width = 300)
logo <- add_image(file = "/Users/schnickdani/Downloads/eth-logo.png", width = 150, align = "left")
firstname <- "Daniel"
lastname <- "Heimgartner"

em <- Email$new()
em
em$write_mail(
  body = htmltools::HTML(
    glue::glue("<h1>Sehr geehrte(r) {firstname} {lastname}</h1>",
               "<p>Vielen Dank für Ihre Teilnahme an unserer Studie zum Thema <b>Homeoffice</b></p>",
               "<p>Sie gelangen mittels QR-Code zur Umfrage.</p>",
               "{qr}",
               "<p>Freundliche Grüsse<br>IVT Projektteam<br>Daniel Heimgartner<br>{logo}</p>")),
  footer = glue::glue("Sent by Daniel Heimgartner on {Sys.Date()}")
)
em
em$send_mail()
em$mailinglist <- data.frame(firstName = "dani", lastName = "heimi", email = "d.heimgartners@gmail.com")
em$send_mail(from = "daniel.heimgartner@ivt.baug.ethz.ch",
             subject = "Testing")
em





## blastual
library(blastula)

test <- prepare_test_message()
test
str(test)
names(test)
test$html_str
test$html_html
test$attachments
test$images


email <-
  compose_email(
    body = htmltools::HTML(
      glue::glue("<h1>Sehr geehrte(r) {firstname} {lastname}</h1>",
                 "<p>Vielen Dank für Ihre Teilnahme an unserer Studie zum Thema <i>Homeoffice</i></p>",
                 "<p>Sie gelangen mittels QR-Code zur Umfrage.</p>",
                 "{qr}",
                 "<p>Freundliche Grüsse<br>IVT Projektteam<br>Daniel Heimgartner<br>{logo}</p>")),
    footer = glue::glue("Sent by Daniel Heimgartner on {Sys.Date()}")
  )
email

email %>%
  smtp_send(
    to = "d.heimgartners@gmail.com",
    from = "daniel.heimgartner@ivt.baug.ethz.ch",
    subject = "Testing",
    credentials = creds(user = "daniehei", provider = "outlook", host = "mail.ethz.ch", port = 587, use_ssl = TRUE)
  )


