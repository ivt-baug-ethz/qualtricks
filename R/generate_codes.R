#' Generate access codes
#'
#' @param n number of codes to be generated
#' @param length length of codes and passwords
#' @param write logical, write to yaml file
#'
#' @return data.frame
#' @export
generate_codes <- function(n, length = 4, write = FALSE) {
  codes <- stringi::stri_rand_strings(n, length = length, pattern = "[A-Z]")
  pw <- stringi::stri_rand_strings(n, length = length, pattern = "[1-9]")
  df <- data.frame(id = 1:n, code = codes, password = pw)

  if(write) {
    yaml::write_yaml(df, "codes.yml")
  }

  df
}
