#' Reformat Demographic Data to Model Structure
#'
#' Reformat demographic data for use in the PREVAIL transmission model. Checks and standardizes columns, expands or assigns ages as needed, allocates values across age groups, expands to missing years, and fills NA values according to the specified method. Pivots to wide format if appropriate.
#'
#' @param custom_data A data frame containing demographic data. Must include columns \code{year} and \code{value}. Optionally includes \code{age}, \code{area}, and \code{iso3}.
#' @param age_required A vector of required ages (numeric or character). If \code{NA}, age expansion is skipped.
#' @param years Optional. A vector of years to ensure are present. Any missing (year, age) combinations are filled with NA values.
#' @param iso A character string for area/iso3 fields if missing. Default is "custom".
#' @param value_allocation How to allocate values across age groups: \code{"maintain"} (keep value per group) or \code{"split"} (divide value equally). Default is "maintain".
#' @param fill_method How to fill NA values in the \code{value} column: \code{"none"} (leave as NA), \code{"closest"} (fill with value from the closest available (year, age) pair), \code{"mean"} (fill with mean per year), or \code{"median"} (fill with median per year). Default is "none".
#'
#' @return
#' A standardized data frame, either long (with \code{year}, \code{age}, \code{value}) or wide (ages as columns).
#' @export
reformat_demographic_data <- function(
    custom_data,
    age_required = NA,
    years = NULL,
    iso = "custom",
    value_allocation = c("maintain", "split"),
    fill_method = c("none", "closest", "mean", "median")
) {
  value_allocation <- base::match.arg(value_allocation)
  fill_method <- base::match.arg(fill_method)
  data_cols <- base::names(custom_data)
  if (!base::all(c("year", "value") %in% data_cols)) {
    base::stop("Input must contain columns 'year' and 'value'.")
  }
  allowed_cols <- c("year", "value", "age", "area", "iso3")
  extra_cols <- base::setdiff(data_cols, allowed_cols)
  if (base::length(extra_cols) > 0) {
    base::warning(base::sprintf(
      "Input contains unexpected columns: %s", base::paste(extra_cols, collapse = ", ")))
  }
  if (!"area" %in% data_cols)  custom_data$area  <- iso
  if (!"iso3" %in% data_cols)  custom_data$iso3  <- iso
  col_order <- c("area", "iso3", base::setdiff(base::names(custom_data), c("area", "iso3")))
  custom_data <- custom_data[, col_order]

  # Always create the full expanded grid
  if (base::all(base::is.na(age_required))) {
    age_unique <- if ("age" %in% names(custom_data)) base::unique(custom_data$age) else NA
  } else {
    age_unique <- age_required
  }
  year_unique <- if (is.null(years)) base::unique(custom_data$year) else years

  # If no age in data, expand just by year
  if (!"age" %in% names(custom_data)) {
    grid <- base::expand.grid(
      area = base::unique(custom_data$area),
      iso3 = base::unique(custom_data$iso3),
      year = year_unique
    )
    expanded <- custom_data
    join_cols <- c("area", "iso3", "year")
  } else {
    grid <- base::expand.grid(
      area = base::unique(custom_data$area),
      iso3 = base::unique(custom_data$iso3),
      year = year_unique,
      age = age_unique
    )
    if (!base::all(base::is.na(age_required))) {
      get_age_range <- function(a) {
        a <- base::as.character(a)
        if (base::grepl("^[0-9]+$", a)) { base::as.numeric(a)
        } else if (base::grepl("^[0-9]+[ ]*-[ ]*[0-9]+$", a)) {
          parts <- base::as.numeric(base::strsplit(a, "-")[[1]])
          base::seq(parts[1], parts[2])
        } else if (base::grepl("^[0-9]+[ ]*\\+", a)) {
          lo <- base::as.numeric(base::sub("\\+", "", a)); lo:200
        } else { NA }
      }
      expanded <- base::lapply(base::seq_len(base::nrow(custom_data)), function(i) {
        row <- custom_data[i, ]
        ages_in_group <- base::intersect(age_unique, get_age_range(row$age))
        n_ages <- base::length(ages_in_group)
        if (n_ages == 0) return(NULL)
        value_out <- base::switch(
          value_allocation,
          maintain = row$value,
          split    = row$value / n_ages
        )
        base::data.frame(
          area = row$area,
          iso3 = row$iso3,
          year = row$year,
          age  = ages_in_group,
          value = value_out
        )
      })
      expanded <- dplyr::bind_rows(expanded)
    } else {
      expanded <- custom_data
    }
    join_cols <- c("area", "iso3", "year", "age")
  }

  custom_data <- dplyr::left_join(grid, expanded, by = join_cols)
  custom_data <- custom_data[, base::intersect(c("area", "iso3", "year", "age", "value"), base::names(custom_data))]

  # ---- Fill method logic ----
  if ("value" %in% base::names(custom_data) && any(is.na(custom_data$value))) {
    if (fill_method == "mean") {
      custom_data <- custom_data %>%
        dplyr::group_by(area, iso3, year) %>%
        dplyr::mutate(
          value = ifelse(
            is.na(value),
            mean(value, na.rm = TRUE),
            value
          )
        ) %>%
        dplyr::ungroup()
    } else if (fill_method == "median") {
      custom_data <- custom_data %>%
        dplyr::group_by(area, iso3, year) %>%
        dplyr::mutate(
          value = ifelse(
            is.na(value),
            stats::median(value, na.rm = TRUE),
            value
          )
        ) %>%
        dplyr::ungroup()
    } else if (fill_method == "closest" && "age" %in% names(custom_data)) {
      fill_closest_2d <- function(values, years, ages) {
        if (all(is.na(values))) return(values)
        missing <- which(is.na(values))
        observed <- which(!is.na(values))
        out <- values
        for (i in missing) {
          # Manhattan distance to every observed value
          dists <- abs(years[i] - years[observed]) + abs(ages[i] - ages[observed])
          closest <- observed[which.min(dists)]
          out[i] <- values[closest]
        }
        out
      }
      custom_data <- custom_data %>%
        dplyr::group_by(area, iso3) %>%
        dplyr::arrange(year, age, .by_group = TRUE) %>%
        dplyr::mutate(
          value = fill_closest_2d(value, as.numeric(year), as.numeric(age))
        ) %>%
        dplyr::ungroup()
    }
  }

  # Output
  if (base::setequal(base::setdiff(base::names(custom_data), c("area", "iso3")), c("year", "value"))) {
    return(dplyr::ungroup(custom_data))
  } else if ("age" %in% base::names(custom_data)) {
    wide_data <- tidyr::pivot_wider(
      custom_data, names_from = age, values_from = value, names_prefix = "x")
    if (!"area" %in% base::names(wide_data)) wide_data$area <- iso
    if (!"iso3" %in% base::names(wide_data)) wide_data$iso3 <- iso
    wide_data <- wide_data[, c("area", "iso3", base::setdiff(base::names(wide_data), c("area", "iso3")))]
    return(dplyr::ungroup(wide_data))
  } else {
    base::stop("Input must have columns 'year', 'age', and 'value'.")
  }
}
