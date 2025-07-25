#' Reformat Parameter Table for Model Input
#'
#' Reformats a parameter table for PREVAIL, standardising column names and units for either disease or vaccine parameters.
#'
#' @param custom_parameters Data frame of custom parameters.
#' @param default_parameters Data frame of the default parameters
#' @param disease Character string, name of the disease.
#' @param type One of \code{"disease"} or \code{"vaccine"}. Controls formatting logic.
#'
#' @return A data frame with reformatted parameters.
#' @importFrom dplyr mutate case_when
#' @export
reformat_parameters <- function(
    custom_parameters,
    default_parameters,
    disease,
    type = c("disease", "vaccine")
) {

  type <- match.arg(type)
  if (type == "vaccine") {
    custom_parameters %>%
      dplyr::mutate(type = "custom") %>%
      dplyr::rename(mean = value) %>%
      dplyr::bind_rows(default_parameters %>%
                         dplyr::mutate(type = "default")) %>%
      dplyr::mutate(
        vaccine = disease,
        disease = disease,
        unit = dplyr::case_when(
          parameter == "waning" ~ "years",
          parameter == "protection" ~ "proportion",
          TRUE ~ NA_character_
        ),
        parameter = paste(duration, "term", parameter, sep = "_")
      ) %>%
      group_by(parameter) %>%
      filter(!(type == "default" & any(type == "custom"))) %>%
      ungroup()

  } else {
    custom_parameters %>%
      dplyr::mutate(type = "custom") %>%
      dplyr::rename(mean = value) %>%
      dplyr::bind_rows(default_parameters %>%
              dplyr::mutate(type = "default")) %>%
      dplyr::mutate(
        disease = disease,
        parameter = dplyr::case_when(
          grepl("incubation", parameter, ignore.case = TRUE) ~ "incubation period",
          grepl("infectious", parameter, ignore.case = TRUE) ~ "infectious period",
          grepl("Reproductive|R0", parameter, ignore.case = TRUE) ~ "reproductive number",
          grepl("waning|immunity", parameter, ignore.case = TRUE) ~ "natural immunity waning",
          TRUE ~ parameter
        )
      ) %>%
      dplyr::mutate(
        mean = case_when(
          unit == "%" ~ mean/100,
          TRUE ~ mean
        ),
        unit = case_when(
          unit == "%" ~ "proportion",
          TRUE ~ unit
        )
      ) %>%
      group_by(parameter) %>%
      filter(!(type == "default" & any(type == "custom"))) %>%
      ungroup()
  }
}
