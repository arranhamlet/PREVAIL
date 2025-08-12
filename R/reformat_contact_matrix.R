#' Reformat Data.frame into a Contact Matrix
#'
#' Converts a data.frame of age-band contacts into a numeric matrix for a specified age range.
#'
#' @param custom_data A data.frame with columns: 'age_from', 'age_to', and 'value'.
#' @param iso Character string for area/iso3 if missing. Default is "custom".
#' @param age_required Numeric vector specifying ages to include in matrix rows and columns (default is 0:100).
#' @param fill_method Method for filling values for missing age combinations. Options: 'closest' (default), 'none'.
#' @param value_allocation Currently unused, reserved for future expansion (maintain or split values across age ranges).
#'
#' @return Numeric matrix representing contact rates (rows = age_from, columns = age_to).
#'
#' @importFrom dplyr mutate rowwise ungroup select rename slice arrange pull
#' @importFrom tidyr unnest
#' @importFrom stats na.omit dist
#' @export
reformat_contact_matrix <- function(custom_data,
                                    iso = "custom",
                                    age_required = 0:100,
                                    fill_method = "closest",
                                    value_allocation = "maintain") {

  expand_ages <- function(age_band, max_age = max(age_required)) {
    if (grepl("-", age_band)) {
      bounds <- as.numeric(strsplit(age_band, "-")[[1]])
      seq(bounds[1], bounds[2])
    } else if (grepl("\\+", age_band)) {
      lower_bound <- as.numeric(sub("\\+", "", age_band))
      seq(lower_bound, max_age)
    } else {
      as.numeric(age_band)
    }
  }

  expanded_df <- custom_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age_from_expanded = list(expand_ages(as.character(age_from))),
      age_to_expanded = list(expand_ages(as.character(age_to)))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(age_from_expanded, age_to_expanded, value) %>%
    tidyr::unnest(age_from_expanded) %>%
    tidyr::unnest(age_to_expanded) %>%
    dplyr::rename(age_from = age_from_expanded, age_to = age_to_expanded)

  contact_matrix <- matrix(NA,
                           nrow = length(age_required),
                           ncol = length(age_required),
                           dimnames = list(age_required, age_required))

  for (i in seq_len(nrow(expanded_df))) {
    row_age <- as.character(expanded_df$age_from[i])
    col_age <- as.character(expanded_df$age_to[i])
    contact_matrix[row_age, col_age] <- expanded_df$value[i]
  }

  if (fill_method == "closest") {
    for (i in seq_len(nrow(contact_matrix))) {
      for (j in seq_len(ncol(contact_matrix))) {
        if (is.na(contact_matrix[i, j])) {
          nearest <- expanded_df %>%
            dplyr::mutate(dist = abs(age_from - age_required[i]) + abs(age_to - age_required[j])) %>%
            dplyr::arrange(dist) %>%
            dplyr::slice(1) %>%
            dplyr::pull(value)
          contact_matrix[i, j] <- nearest
        }
      }
    }
  } else if (fill_method == "none") {
    contact_matrix[is.na(contact_matrix)] <- 0
  }

  #Reformat for export
  c_list <- list(contact_matrix)
  names(c_list) <- iso
  c_list
}
