# Email <- R6

Email <-
  R6::R6Class(
    "Email",
    private = list(.creds = NULL),
    public = list(
      mail_adress = NULL,
      email = NULL,
      user = NA_character_,
      sender = NA_character_,
      provider = NA_character_,
      host = NA_character_,
      port = 587,
      use_ssl = TRUE,
      initialize = function(mail_adress = NULL, email = NULL, user = "daniehei",
                            sender = "daniel.heimgartner@ivt.baug.ethz.ch",
                            provider = "outlook", host = "mail.ethz.ch", port = 587,
                            use_ssl = TRUE) {
        self$mail_adress <- mail_adress
        self$email <- email
        self$user <- user
        self$sender <- sender
        self$provider <- provider
        self$host <- host
        self$port <- port
        self$use_ssl <- use_ssl
        private$.creds <- blastula::creds(user, provider, host, port, use_ssl)
      },
      ## TODO: document
      write_mail = function(...) {
        self$email <- blastula::compose_email(...) ###
        invisible(self)
      },
      send_mail = function(subject, ...) {
        assertthat::assert_that(!is.null(self$email),
                                msg = "You have to write the mail first!")
        assertthat::assert_that(!is.null(self$mail_adress),
                                msg = "You have to provide a mail_adress!")

        self$email %>%
          blastula::smtp_send(from = self$sender, to = self$mail_adress, subject = subject,  credentials = private$.creds, ...)

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
               "<p>Vielen Dank für Ihre Teilnahme an unserer Studie zum Thema <b>Homeoffice</b>.</p>",
               "<p>Sie gelangen mittels QR-Code zur Umfrage.</p>",
               "{qr}",
               "<p>Freundliche Grüsse<br>IVT Projektteam<br>Daniel Heimgartner<br>{logo}</p>")),
  footer = glue::glue("Sent by Daniel Heimgartner on {Sys.Date()}")
)
em
em$send_mail()
em$mail_adress <- "d.heimgartners@gmail.com"
em$send_mail(subject = "Testing")
em



EmailIterator <-
  R6::R6Class(
    "EmailIterator"
  )







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


