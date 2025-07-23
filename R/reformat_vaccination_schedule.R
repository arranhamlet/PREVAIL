#' Reformat Vaccination Schedule Data
#'
#' Adds required columns and renames fields for a custom vaccination schedule,
#' formatting for PREVAIL model input.
#'
#' @param custom_data Data frame of custom vaccination schedule (must include columns \code{age} and \code{dose}).
#' @param disease Character string, name of the disease (default: "custom").
#' @param iso Character string, 3-letter ISO code (default: "custom").
#'
#' @return A data frame with required columns and standardised names for model input.
#' @importFrom dplyr mutate rename
#' @export
reformat_vaccination_schedule <- function(
    custom_data,
    disease = "custom",
    iso = "custom"
) {
  custom_data %>%
    dplyr::mutate(
      area = iso,
      iso3 = iso,
      vaccine_description = disease,
      target_pop_description = "all",
      target_pop = "all",
      vaccine_code = disease
    ) %>%
    dplyr::rename(
      age_administered = age,
      schedulerounds = dose
    )
}
