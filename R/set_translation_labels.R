#' Set translation labels
#'
#' Has to be called before `translate_survey` (has sideffects -> sets labels `labelr::labels$get()`)
#'
#' @param survey data.frame
#' @param codebook see `get_codebook`
#' @param .exclude passed to filter... better stick with the defaults... otherwise look into function body to understand what happens :S
#'
#' @return returns labels invisible and sets `labelr::labels`
#' @export
set_translation_labels <- function(survey, codebook,
                                   .exclude = rlang::quos(QuestionType == "TE" | stringr::str_detect(DataExportTag, "^Q")))
{
  ## check if translations have been added to codebook
  check <- "translations" %in% names(attributes(codebook))
  assertthat::assert_that(check,
                          msg = "Run codebook with add_translations set to TRUE")

  translations <- attributes(codebook)$translations

  nm <- sort(names(survey))


  translations <-
    translations %>%
    dplyr::mutate(key = purrr::map(DataExportTag, ~ nm[grep(.x, nm)])) %>%
    tidyr::unnest(key)


  if(!is.null(.exclude))
  {
    cb_exclude <-
      codebook %>%
      dplyr::filter(!!!.exclude)

    cb_exclude$flag <- 1
    translations <-
      translations %>%
      dplyr::left_join(dplyr::select(cb_exclude,
                                     DataExportTag,
                                     flag), by = "DataExportTag") %>%
      dplyr::mutate(flag = ifelse(is.na(flag), 0, flag)) %>%
      dplyr::filter(flag != 1) %>%
      dplyr::select(-flag)
  }


  labels <-
    translations %>%
    dplyr::select(key, type, from, to) %>%
    dplyr::filter(!stringr::str_detect(key, "_text$|_click$|_submit$|_count$"))

  gk <-
    labels %>%
    dplyr::group_by(key) %>%
    dplyr::group_keys() %>%
    dplyr::pull(key)

  labels <-
    labels %>%
    dplyr::group_by(key) %>%
    dplyr::arrange(type) %>%
    dplyr::group_split(.keep = FALSE)

  names(labels) <- gk

  labelr::labels$set(labels)

  invisible(labels)

}
