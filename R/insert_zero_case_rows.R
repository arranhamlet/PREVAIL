#' @title Insert Zero-Case Rows After Each Year
#'
#' @description
#' Internal utility to insert rows with \code{cases = 0} one year after each case report.
#' Ensures continuity in time-series plots and removes any zero-case duplicates if the year already exists.
#'
#' @param df A \code{data.frame} with columns: \code{year}, \code{cases},
#'   \code{iso3}, \code{name}, \code{disease_short}, \code{disease_description}.
#'
#' @return A \code{data.frame} with inserted zero-case rows and filtered duplicates.
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr arrange mutate row_number bind_rows distinct select desc
insert_zero_case_rows <- function(df) {
  df %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    dplyr::bind_rows(
      df %>%
        dplyr::mutate(year = year + 1, cases = 0)
    ) %>%
    dplyr::arrange(year, row_id, dplyr::desc(cases)) %>%
    dplyr::distinct(year, iso3, disease_short, .keep_all = TRUE) %>%
    dplyr::select(-row_id)
}
