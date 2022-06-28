
#' Download translations for multi-lingual languages
#'
#' Please install qualtricks from ivt-baug-ethz and set up your Qualtrics credentials.
#'
#' @param survey_id
#' @param languages char vector of all language codes used in your survey, defaults to `c("DE", "EN")`
#'
#' @author Thomas Schatzmann \email[thomas.schatzmann@ivt.baug.ethz.ch]
#'
#' @return
#' @export
download_translations <- function(survey_id, languages = c("DE", "EN"))
{
  qualtricks::check_api_keys()
  base_url <- Sys.getenv("QUALTRICS_BASE_URL")
  key <- Sys.getenv("QUALTRICS_API_KEY")

  trans_list <-
    lapply(languages, function(lang)
    {
      translations_url <- sprintf("%s/API/v3/surveys/%s/translations/%s", base_url, survey_id, lang)

      translations <-
        httr::content(httr::GET(translations_url, httr::add_headers(.headers = c('x-api-token' = key))))$result %>%
        purrr::compact()

      tt <-
        data.frame(translations, stringsAsFactors = FALSE) %>%
        dplyr::select(-starts_with("Survey")) %>%
        t() %>%
        data.frame(stringsAsFactors = FALSE)

      colnames(tt) <- c(lang)
      tt
    })

  trans_df <- data.frame(trans_list)
  trans_df$question <- rownames(trans_df)
  trans_df <- dplyr::select(trans_df, question, everything())

  trans_df <-
    trans_df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(languages), function(x) trimws(qualtricks::strip_html(x))))


  split_qids <- data.frame(stringr::str_match(trans_df$question, '(QID\\d+)_(.*)'), stringsAsFactors = F)
  colnames(split_qids) <- c('question', 'qid', 'value_type')

  split_qids <-
    split_qids %>%
    dplyr::mutate(qid_nr = as.numeric(stringr::str_extract(qid, "\\d+")),
                  qid_type_nr = as.numeric(stringr::str_extract(value_type, "\\d+")),
                  qid_type = as.vector(stringr::str_match(value_type, "[:alpha:]+"))) %>%
    dplyr::select(question, qid_nr, qid_type, qid_type_nr)

  trans_df <-
    split_qids %>%
    dplyr::left_join(trans_df, by = "question") %>%
    dplyr::select(question, qid_nr, qid_type, qid_type_nr, dplyr::everything()) %>%
    dplyr::arrange(qid_nr, dplyr::desc(qid_type), qid_type_nr) %>%
    dplyr::as_tibble()

  return(trans_df)
}




#' Generates a code book for your survey
#'
#' Calls download_translations internally
#'
#' @param survey_id
#' @param languages char vector of all language codes used in your survey, defaults to `c("DE", "EN")`
#' (also passed to `download_translations`)
#'
#' @author Thomas Schatzmann \email[thomas.schatzmann@ivt.baug.ethz.ch]
#'
#' @return
#' @export
#' @examples
#' \dontrun{
#'   survey_id <- "SV_6u4YzY24OAfhWfQ"
#'   generate_codebook(survey_id)
#' }


generate_codebook <- function(survey_id, languages = c("DE", "EN"))
{
  qualtricks::check_api_keys()
  base_url <- Sys.getenv("QUALTRICS_BASE_URL")
  key <- Sys.getenv("QUALTRICS_API_KEY")

  translations <- preStudy::download_translations(survey_id, languages)

  quest_df <-
    qualtRics::survey_questions(survey_id) %>%
    dplyr::select(-question) %>%
    dplyr::mutate(qid_nr = as.numeric(stringr::str_remove(qid, "QID")), .after = "qid") %>%
    dplyr::rename(export_tag = qname) %>%
    dplyr::filter(!stringr::str_detect(export_tag, "intro")) %>%
    dplyr::select(-qid) %>%
    dplyr::arrange(qid_nr)

  # get question definitions myway...
  # https://api.qualtrics.com/api-reference/YXBpOjYwOTM2-qualtrics-survey-api
  url <- paste(base_url, "/API/v3/survey-definitions", survey_id, "questions", sep = "/")

  question_defs_df <-
    httr::content(httr::GET(url, httr::add_headers(.headers = c('x-api-token' = key))))$result$elements %>%
    purrr::compact()

  types_df <-
    lapply(question_defs_df, function(x)
    {
      y <- vector(mode = "list", length = 4)
      names(y) <- c("qid_nr","type","selector","subselector")

      y[["qid_nr"]] <- as.numeric(stringr::str_remove(x$QuestionID, "QID"))
      y[["type"]] <- as.character(x$QuestionType)
      y[["selector"]] <- as.character(x$Selector)
      y[["subselector"]] <- as.character(x$SubSelector)
      y
    })

  types_df <-
    types_df %>%
    tibblify::tibblify() %>%
    dplyr::filter(!type %in% c("Timing", "Meta")) %>%
    dplyr::mutate(type_description =
                    dplyr::case_when(type == "TE" ~ "text_entry",
                                     type == "MC" ~ "multiple_choice",
                                     type == "Matrix" ~ "matrix",
                                     type == "DB" ~ "descriptive_box",
                                     type == "SBS" ~ "side_by_side",
                                     type == "DD" ~ "dropdown",
                                     type == "Slider" ~ "slider",
                                     type == "CS" ~ "constant_sum",
                                     type == "RO" ~ "rank_order",
                                     TRUE ~ NA_character_),
                  selector_description =
                    dplyr::case_when(selector == "SAVR" ~ "single_answer_vertical",
                                     selector == "SAHR" ~ "single_answer_horizontal",
                                     selector == "SACOL" ~ "single_answer_col",
                                     selector == "MAVR" ~ "multiple_answer_vertical",
                                     selector == "MAHR" ~ "multiple_answer_horizontal",
                                     selector == "MACOL" ~ "multiple_answer_col",
                                     selector == "FORM" ~ "form",
                                     selector == "TE" ~ "text_entry",
                                     selector == "TB" ~ "text_box",
                                     selector == "SL" ~ "single_line",
                                     selector == "Likert" ~ "likert",
                                     selector == "VRTL" ~ "vertical",
                                     selector == "HSLIDER" ~ "horizontal",
                                     selector == "DL" ~ "dropdown",
                                     selector == "DND" ~ "drag_and_drop",
                                     TRUE ~ NA_character_),
                  subselector_description =
                    dplyr::case_when(subselector == "TX" ~ "text",
                                     subselector == "SingleAnswer" ~ "single_answer",
                                     subselector == "MultipleAnswer" ~ "multiple_answer",
                                     TRUE ~ NA_character_))

  recode_values_df <-
    lapply(question_defs_df, function(x)
    {
      y <- vector(mode = "list", length = 3)
      names(y) <- c("qid_nr", "question", "recode_value")

      y[["qid_nr"]] <- as.numeric(stringr::str_remove(x$QuestionID, "QID"))

      if (x$QuestionType == "Matrix") {
        y[["question"]] <- as.character(paste0(x$QuestionID, "_Answer", names(x$RecodeValues)))
      } else {
        y[["question"]] <- as.character(paste0(x$QuestionID, "_Choice", names(x$RecodeValues)))
      }

      y[["recode_value"]] <- as.numeric(as.vector(unlist(x$RecodeValues)))
      y
    })

  recode_values_df <-
    recode_values_df %>%
    tibblify::tibblify() %>%
    tidyr::unnest(cols = c(question, recode_value)) %>%
    dplyr::filter(!is.na(recode_value)) %>%
    dplyr::arrange(qid_nr, question)

  choice_export_tags_df <-
    lapply(question_defs_df, function(x)
    {
      if (!is.null(x$ChoiceDataExportTags)) {
        y <- vector(mode = "list", length = 3)
        names(y) <- c("qid_nr", "question", "choice_export_tag")
        y[["qid_nr"]] <- as.numeric(stringr::str_remove(x$QuestionID, "QID"))
        y[["question"]] <- as.character(paste0(x$QuestionID, "_Choice", names(x$ChoiceDataExportTags)))
        y[["choice_export_tag"]] <- as.character(as.vector(unlist(x$ChoiceDataExportTags)))
        y
      }
    })

  choice_export_tags_df <-
    choice_export_tags_df %>%
    tibblify::tibblify() %>%
    tidyr::unnest(cols = c(question, choice_export_tag)) %>%
    dplyr::filter(choice_export_tag != "FALSE")

  ## join translations with questions and recoded values, do some changes
  codebook <-
    translations %>%
    dplyr::inner_join(quest_df, by = "qid_nr") %>%
    dplyr::left_join(types_df, by = "qid_nr") %>%
    dplyr::left_join(recode_values_df, by = c("question","qid_nr")) %>%
    dplyr::left_join(choice_export_tags_df, by = c("question","qid_nr")) %>%
    dplyr::mutate(variable_name =
                    dplyr::case_when(is.na(choice_export_tag) & qid_type == "Choice" & !type %in% c("MC","RO") ~ export_tag,
                                     is.na(choice_export_tag) & qid_type == "Choice" & type == "RO" & !is.na(recode_value) ~ paste0(export_tag,"_",recode_value),
                                     is.na(choice_export_tag) & qid_type == "Choice" & type == "MC" & selector %in% c("SAVR","SAHR","SACOL","DL") ~ export_tag,
                                     is.na(choice_export_tag) & qid_type == "Choice" & type == "MC" & selector %in% c("MAVR","MAHR","MACOL") & !is.na(recode_value) ~ paste0(export_tag,"_",recode_value),
                                     !is.na(choice_export_tag) & qid_type == "Choice" & type != "MC" ~ choice_export_tag,
                                     TRUE ~ NA_character_)) %>%
    dplyr::select(variable_name, qid_nr, translation_type = qid_type, force_resp,
                  question_type = type_description, question_subtype = selector_description, question_subsubtype = subselector_description,
                  recode_value, dplyr::all_of(languages))

  return(codebook)

}






