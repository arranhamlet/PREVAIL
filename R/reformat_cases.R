#' Reformat Custom Case Data for Model Input
#'
#' Adds required columns to custom case data to align with internal formatting
#' expectations for disease modeling.
#'
#' @param custom_data A data frame of user-supplied case data.
#' @param iso Character string; the 3-letter ISO country code (e.g., "ETH").
#' @param disease Character string; the disease name (e.g., "measles").
#'
#' @return A data frame with standardized columns: \code{iso3}, \code{name}, \code{disease_short}, and \code{disease_description}.
#' @export
#'
#' @importFrom dplyr mutate
#'
#' @examples
#' reformat_cases(data.frame(year = 2000:2002, value = c(100, 120, 80)), "KEN", "measles")
reformat_cases <- function(
    custom_data,
    iso,
    disease
) {
  dplyr::mutate(
    custom_data,
    iso3 = iso,
    name = iso,
    disease_short = disease,
    disease_description = disease
  )
}
