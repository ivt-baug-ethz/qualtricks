#' Generate a codebook
#'
#' @seealso `generate_codebook()`
#'
#' @param survey_id
#' @param spec optionally passed to `tibblify()`, defaults to `NULL`
#' @param encoding passed to `get_questions`, defaults to `"utf-8"`
#' @param add_translations should translations data.frame be added as attribute, defaults to `TRUE`
#' Retrieve via `attributes(codebook, "translations")`
#' @param ... not implemented
#'
#' @return
#' @export
get_codebook <- function(survey_id, spec = NULL, encoding = "utf-8", add_translations = TRUE, ...)
{
  sq <- get_questions(survey_id, encoding = encoding)
  sq <- rjson::fromJSON(sq)
  sq_res <- sq$result

  q_df <- tibblify::tibblify(sq_res$elements, spec = spec)

  desc_df <-
    q_df %>%
    dplyr::select(QuestionID, DataExportTag, QuestionText, QuestionDescription,
                  QuestionType, Selector, SubSelector, ChoiceOrder)

  choices_df <-
    q_df %>%
    dplyr::select(QuestionID, Choices) %>%
    tidyr::unnest(cols = "Choices") %>%
    dplyr::select(QuestionID, key_choice = Display, value_choice = .names)

  answers_df <-
    q_df %>%
    dplyr::select(QuestionID, Answers) %>%
    tidyr::unnest(cols = "Answers") %>%
    dplyr::select(QuestionID, key_answer = Display, value_answer = .names)

  translations <- q_df[names(q_df) %in% c("QuestionID", "Language")] %>% tidyr::unnest(cols = "Language")

  t_choices_df <-
    translations %>%
    dplyr::select(QuestionID, Choices) %>%
    tidyr::unnest(cols = "Choices") %>%
    dplyr::select(QuestionID, t_key_choice = Display, value_choice = .names)

  t_answers_df <-
    translations %>%
    dplyr::select(QuestionID, Answers) %>%
    tidyr::unnest(cols = "Answers") %>%
    dplyr::select(QuestionID, t_key_answer = Display, value_answer = .names)

  codebook <-
    desc_df %>%
    dplyr::full_join(choices_df, by = "QuestionID") %>%
    dplyr::full_join(answers_df, by = "QuestionID") %>%
    dplyr::full_join(t_choices_df, by = c("QuestionID", "value_choice")) %>%
    dplyr::full_join(t_answers_df, by = c("QuestionID", "value_answer")) %>%
    dplyr::select(QuestionID, DataExportTag, QuestionText, QuestionDescription,
                  QuestionType, Selector, SubSelector, ChoiceOrder,
                  key_choice, t_key_choice, value_choice,
                  key_answer, t_key_answer, value_answer)

  codebook <-
    codebook %>%
    dplyr::mutate(dplyr::across(tidyselect:::where(is.character),
                                function(x)
                                {
                                  qualtricks::strip_html(x)
                                }))


  ## matrix -> choice and answer
  ## others -> only choice
  ## a little confusing...
  codebook <-
    codebook %>%
    dplyr::mutate(factor_levels = ifelse(is.na(key_answer), key_choice, key_answer),
                  factor_levels_t = ifelse(is.na(t_key_answer), t_key_choice, t_key_answer),
                  factor_value = ifelse(is.na(value_answer), value_choice, value_answer))



  ## own naming convention
  codebook <-
    codebook %>%
    dplyr::rename(QuestionSpecifier = value_choice,
                  Answer = factor_levels,
                  AnswerTranslation = factor_levels_t,
                  AnswerValue = factor_value) %>%
    dplyr::select(-tidyselect::contains("key_choice"),
                  -tidyselect::contains("key_answer"),
                  -value_answer)


  if(add_translations)
  {

    translations <-
      codebook %>%
      dplyr::select(QuestionID, DataExportTag, QuestionSpecifier,
                    from = Answer, AnswerTranslation, AnswerValue) %>%
      tidyr::pivot_longer(cols = c(AnswerTranslation, AnswerValue), names_to = "type", values_to = "to") %>%
      tidyr::drop_na()

    attr(codebook, "translations") <- translations
  }

  codebook
}
