#' Generate access codes
#'
#' @param n
#'
#' @return
#' @export
generate_codes <- function(n, write = FALSE) {
  codes <- stringi::stri_rand_strings(n, length = 4, pattern = "[A-Z]")
  pw <- stringi::stri_rand_strings(n, length = 4, pattern = "[1-9]")
  df <- data.frame(id = 1:n, code = codes, password = pw)

  if(write) {
    yaml::write_yaml(df, "codes.yml")
  }
}
