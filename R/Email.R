#' R6 Class representing a single E-Mail
#'
#' It let's you define the recipient, the email itself and the connection and is
#' a helper object for working with the `blastula` package.
#'
#' @seealso `blastula` package
#'
#' @examples
#' \dontrun{
#' qr <- add_image(file = "/Users/schnickdani/Downloads/SV_6u4YzY24OAfhWfQ-qrcode.png", width = 300)
#' logo <- add_image(file = "/Users/schnickdani/Downloads/eth-logo.png", width = 150, align = "left")
#' firstname <- "Daniel"
#' lastname <- "Heimgartner"
#'
#' body <-
#'   htmltools::HTML(
#'     glue::glue("<h1>Sehr geehrte(r) {firstname} {lastname}</h1>",
#'                "<p>Vielen Dank für Ihre Teilnahme an unserer Studie zum Thema <b>Homeoffice</b>.</p>",
#'                "<p>Sie gelangen mittels QR-Code zur Umfrage.</p>",
#'                "{qr}",
#'                "<p>Freundliche Grüsse<br>IVT Projektteam<br>Daniel Heimgartner<br>{logo}</p>"))
#'
#' footer <- glue::glue("Sent by Daniel Heimgartner on {Sys.Date()}")
#'
#' em <- Email$new()
#' em
#' em$write_mail(body = body, footer = footer)
#' em
#' em$send_mail()
#' em$mail_adress <- "foo@gmail.com"
#' em$send_mail(subject = "Testing")
#' em
#' }
#'
#' @export
Email <-
  R6::R6Class(
    "Email",
    private = list(
      .creds = NULL,
      email = NULL
    ),
    public = list(
      #' @field mail_adress Mail adress of the recipient
      mail_adress = NULL,

      #' @field user The username for the email account. Typically, this is the
      #' email address associated with the account. In the case of ETH this is
      #' your user name (e.g. daniehei);
      #' Reference: `blastula` package
      user = NA_character_,

      #' @field sender The adress of the sender. In the case of ETH this is your
      #' mail adress (e.g. <daniel.heimgartner@ivt.ethz.ch>)
      sender = NA_character_,

      #' @field provider An optional email provider shortname for autocompleting
      #' SMTP configuration details (the `host`, `port`, `use_ssl` options). Options
      #' currently include gmail, outlook, and office365. If nothing is provided
      #' then values for `host`, `port`, and `use_ssl` are expected;
      #' Reference: `blastula` package
      provider = NA_character_,

      #' @field host Configuration info for the SMTP server. The `host` and `port`
      #' parameters are the address and port for the SMTP server;
      #' Reference: `blastula` package
      host = NA_character_,

      #' @field port Configuration info for the SMTP server. The `host` and `port`
      #' parameters are the address and port for the SMTP server;
      #' Reference: `blastula` package
      port = 587,

      #' @field use_ssl is an option as to whether to use SSL: supply a `TRUE` or `FALSE` value.
      use_ssl = TRUE,

      #' @description
      #' Create a new email object.
      #' @param mail_adress Mail adress of recipient.
      #' @param user User name for email account.
      #' @param sender The adress of the sender.
      #' @param provider An email provider shortname (gmail, outlook or office 365).
      #' @param Configuration info for the SMTP server.
      #' @param use_ssl Use SSl.
      #' @return A new `Email` object.
      initialize = function(mail_adress = NULL, user = "daniehei",
                            sender = "daniel.heimgartner@ivt.baug.ethz.ch",
                            provider = "outlook", host = "mail.ethz.ch", port = 587,
                            use_ssl = TRUE) {
        self$mail_adress <- mail_adress
        self$user <- user
        self$sender <- sender
        self$provider <- provider
        self$host <- host
        self$port <- port
        self$use_ssl <- use_ssl
        private$.creds <- blastula::creds(user, provider, host, port, use_ssl)
      },

      #' @description
      #' Write an email. Wrapper around `blastula::compose_email`
      #' @param ... arguments passed to `blastula::compose_email`
      #' @seealso `blastula::compose_email`
      write_mail = function(...) {
        private$email <- blastula::compose_email(...)
        invisible(self)
      },

      #' @description
      #' Send the email. Wrapper around `blastula::smtp_send`
      #' @param subject Subject for the email
      #' @param ... args passed to `blastula::smtp_send`
      #' @seealso `blastula::smtp_send`
      send_mail = function(subject, ...) {
        assertthat::assert_that(!is.null(private$email),
                                msg = "You have to write the mail first!")
        assertthat::assert_that(!is.null(self$mail_adress),
                                msg = "You have to provide a mail_adress!")

        private$email %>%
          blastula::smtp_send(from = self$sender, to = self$mail_adress, subject = subject,  credentials = private$.creds, ...)

        invisible(self)
      },

      #' @description
      #' Print method for Email. Renders the email in the Viewer.
      print = function() {
        assertthat::assert_that(!is.null(private$email),
                                msg = "You have to write the mail first!")
        print(private$email)
        invisible(self)
      }
    )
  )






#' R6 Class allowing for easy distribution of an Email object to mailinglist.
#'
#' It let's you define a mailinglist, the email template itself and the connection and is
#' a helper object for working with the `blastula` package. While it inherits from
#' the Email class, an EmailIterator is intended to work with a template which defines
#' placeholders. The placeholders are then filled from the respective columns from
#' the mailinglist data.frame (e.g. person characteristics, qualtrics access codes, etc.)
#'
#' @seealso `blastula` package
#'
#' @examples
#' \dontrun{
#' template <- function(firstName, language, id) {
#'   body <- glue::glue("Hello {firstName}, your language is {language}")
#'   footer <- glue::glue("id = {id}")
#'   list(body = body, footer = footer)
#' }
#'
#'
#' ei <- EmailIterator$new(mailinglist = mailinglist)
#' ei$set_template(template)
#' ei$check_template()
#' ei$send_mails(subject = "test-mail")
#' }
#'
#' @export
EmailIterator <-
  R6::R6Class(
    "EmailIterator",
    inherit = Email,
    private = list(
      email_template = NULL
    ),
    public = list(
      #' @field mailinglist A data.frame containing at least an `email` column
      #' and other data to be provided in the template.
      mailinglist = NULL,

      #' @description
      #' Create a new EmailIterator object.
      #' @param mailinglist A data.frame containing at least an `email` column
      #' and other data to be provided in the template. Consider `qualtRics::fetch_mailinglist`.
      #' @param user Inherited from super `Email`
      #' @param sender Inherited from super `Email`
      #' @param provider Inherited from super `Email`
      #' @param host Inherited from super `Email`
      #' @param port Inherited from super `Email`
      #' @param use_ssl Inherited from super `Email`
      #' @return A new `EmailIterator` object.
      initialize = function(mailinglist,
                            ## from super
                            user, sender, provider, host, port, use_ssl) {
        super$initialize()
        self$mailinglist <- mailinglist
        self$validate()
      },
      validate = function() {
        assertthat::assert_that(is.data.frame(self$mailinglist),
                                msg = "mailinglist must be a data.frame")
        assertthat::assert_that("email" %in% names(self$mailinglist),
                                msg = "email must be present in mailinglist")
      },

      #' @description
      #' Sets the template. A template is a function with placeholders as parameters
      #' to be filled from the mailinglist.
      #' @param fn A function with placeholder as parameters to be filled from
      #' the mailinglist. The function `fn` must return a named list which will be
      #' passed as args to `blastula::compose_email`.
      #' @example
      #' fn <- function(firstName, language, id) {
      #'   body <- glue::glue("Hello {firstName}, your language is {language}")
      #'   footer <- glue::glue("id = {id}")
      #'   list(body = body, footer = footer)
      #' }
      set_template = function(fn) {
        assertthat::assert_that(is.function(fn),
                                msg = "fn must be a function")
        private$email_template <- fn
        invisible(self)
      },

      #' @description
      #' Render the email in the Viewer to check its appearence.
      #' @param ... args must correspond to `fn`.
      #' @example
      #' \dontrun{
      #' emailiterator$check_template(firstName = 'Daniel', language = 'German', id = 1010)
      #' }
      check_template = function(...) {
        assertthat::assert_that(!is.null(private$email_template),
                                msg = "You must provide a template first (use EmailIterator$set_template)")
        do.call(self$write_mail, as.list(private$email_template(...)))
        print(self)
        invisible(self)
      },

      #' @description
      #' Iterate over mailinglist and send mail to each recipient. Fills the
      #' placeholder values in the template with matching columns from the
      #' mailinglist.
      #' @param subject The E-Mail's subject.
      send_mails = function(subject) {

        template_args <- names(formals(private$email_template))
        assertthat::assert_that(all(template_args %in% names(self$mailinglist)),
                                msg = glue::glue("{template_args} must be present in mailinglist"))

        apply(X = self$mailinglist, MARGIN = 1, FUN = function(x) {
          args <- as.list(x[template_args])
          template <- do.call(private$email_template, args)
          self$mail_adress <- x["email"]
          do.call(self$write_mail, template)
          self$send_mail(subject = subject)
        }, simplify = FALSE)
        invisible(self)
      }
    )
  )






