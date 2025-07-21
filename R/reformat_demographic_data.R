#' Reformat Demographic Data to Model Structure
#'
#' Expands, standardizes, and fills demographic data for use in the PREVAIL transmission model. Handles age expansion, year grid completion, and flexible missing data imputation (mean, median, or nearest neighbor).
#'
#' @param custom_data A data frame containing demographic data. Must include \code{year} and \code{value}. Optionally includes \code{age}, \code{area}, and \code{iso3}.
#' @param age_required Vector of required ages. If \code{NA}, age expansion is skipped.
#' @param years Vector of years to ensure are present (default: unique years in data).
#' @param iso Character string for area/iso3 if missing. Default is "custom".
#' @param value_allocation For group age ranges: \code{"maintain"} (default, keep value per group), or \code{"split"} (divide value evenly across ages).
#' @param fill_method Method for filling missing values: \code{"none"} (leave as NA), \code{"closest"} (fill by nearest year/age, or nearest year for unstratified), \code{"mean"} (fill by group-year mean), or \code{"median"} (by group-year median). Default is "none".
#'
#' @return
#' A tidy data frame: wide format if age present, else long format.
#'
#' @examples
#' df <- data.frame(year = 2000:2002, value = c(100, 110, 120))
#' reformat_demographic_data(df, age_required = 0:4, years = 2000:2005, fill_method = "closest")
#'
#' @importFrom dplyr group_by ungroup mutate arrange select if_else
#' @importFrom tidyr crossing complete pivot_wider
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @importFrom stats median
#' @importFrom utils tail
#' @export

reformat_demographic_data <- function(
    custom_data,
    age_required = NA,
    years = NULL,
    iso = "custom",
    value_allocation = c("maintain", "split"),
    fill_method = c("none", "closest", "mean", "median")
) {
  value_allocation <- match.arg(value_allocation)
  fill_method <- match.arg(fill_method)
  if (!all(c("year", "value") %in% names(custom_data))) stop("Input must contain columns 'year' and 'value'.")
  if (!"area" %in% names(custom_data)) custom_data$area <- iso
  if (!"iso3" %in% names(custom_data)) custom_data$iso3 <- iso

  # Store if unstratified by age BEFORE expansion
  unstratified_by_age <- !"age" %in% names(custom_data) || all(is.na(custom_data$age))
  # Expand years/ages
  years <- if (is.null(years)) sort(unique(custom_data$year)) else years
  ages <- if (all(is.na(age_required))) {
    if ("age" %in% names(custom_data)) sort(unique(custom_data$age)) else NA
  } else age_required

  # Repeat value for all ages at each year if not stratified by age
  if (unstratified_by_age) {
    custom_data <- tidyr::crossing(custom_data, age = ages)
  } else if (!all(is.na(age_required))) {
    expand_row <- function(row, ages) {
      a <- as.character(row$age)
      if (grepl("^[0-9]+$", a)) a_out <- as.numeric(a)
      else if (grepl("^[0-9]+[ ]*-[ ]*[0-9]+$", a)) {
        p <- as.numeric(strsplit(a, "-")[[1]]); a_out <- seq(p[1], p[2])
      } else if (grepl("^[0-9]+[ ]*\\+", a)) {
        lo <- as.numeric(sub("\\+", "", a)); a_out <- lo:200
      } else a_out <- NA
      a_out <- intersect(ages, a_out)
      n <- length(a_out)
      tibble::tibble(
        area = row$area, iso3 = row$iso3, year = row$year,
        age = a_out, value = if (value_allocation == "split") row$value / n else row$value
      )
    }
    custom_data <- purrr::map_dfr(1:nrow(custom_data), ~expand_row(custom_data[.,], ages))
  }
  # Expand grid
  custom_data <- tidyr::complete(
    custom_data, area, iso3, year = years, age = ages
  )

  # Fill methods
  if (fill_method == "mean") {
    custom_data <- custom_data %>%
      dplyr::group_by(area, iso3, year) %>%
      dplyr::mutate(value = ifelse(is.na(value), mean(value, na.rm = TRUE), value)) %>%
      dplyr::ungroup()
  } else if (fill_method == "median") {
    custom_data <- custom_data %>%
      dplyr::group_by(area, iso3, year) %>%
      dplyr::mutate(value = ifelse(is.na(value), stats::median(value, na.rm = TRUE), value)) %>%
      dplyr::ungroup()
  } else if (fill_method == "closest") {
    # Helper for stratified: closest by (year, age). For unstratified: closest by year only
    fill_closest <- function(val, y, a, stratified) {
      miss <- which(is.na(val))
      obs <- which(!is.na(val))
      if (length(obs) == 0) return(val)
      for (i in miss) {
        if (stratified) {
          d <- abs(y[i] - y[obs]) + abs(a[i] - a[obs])
        } else {
          d <- abs(y[i] - y[obs])
        }
        if (length(d) == 0 || all(is.na(d))) next
        val[i] <- val[obs[which.min(d)]]
      }
      val
    }
    custom_data <- custom_data %>%
      dplyr::group_by(area, iso3) %>%
      dplyr::arrange(year, age, .by_group = TRUE) %>%
      dplyr::mutate(value = fill_closest(value, as.numeric(year), as.numeric(age), !unstratified_by_age)) %>%
      dplyr::ungroup()
  }
  # Output
  if ("age" %in% names(custom_data) && !all(is.na(custom_data$age))) {
    custom_data %>%
      tidyr::pivot_wider(names_from = age, values_from = value, names_prefix = "x") %>%
      dplyr::ungroup()
  } else {
    custom_data %>%
      dplyr::select(-age) %>%
      dplyr::ungroup()
  }
}
