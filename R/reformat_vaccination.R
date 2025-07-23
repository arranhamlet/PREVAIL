#' Reformat Custom Vaccination Data for Model Input
#'
#' Adds required columns to a user-supplied vaccination data frame for routine vaccination coverage,
#' standardizing column names and values for use within the PREVAIL package.
#'
#' @param custom_vaccination A data.frame or tibble containing at minimum a year and value column.
#' @param iso Three-letter ISO country code.
#' @param disease Disease or vaccine name (character).
#'
#' @return A tibble with required columns: iso3, area, disease, vaccine_description, vaccination_name.
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @export
reformat_vaccination <- function(custom_vaccination, iso, disease) {

  dplyr::mutate(
    custom_vaccination,
    iso3 = iso,
    area = iso,
    disease = disease,
    vaccine_description = disease,
    vaccination_name = disease
  ) %>%
    dplyr::rename(dose_order = dose)

}
