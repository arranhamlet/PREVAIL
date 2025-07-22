#' Calculate Aging Rate, Reproductive Age Bounds, and Reproductive Weights
#'
#' Computes the aging rate vector, reproductive age bounds, and a reproductive weight data.frame
#' (used for birth assignment) for a demographic structure, for both single-year and aggregated age bands.
#'
#' @param aggregate_age Logical. If \code{TRUE}, aggregate into new_age_breaks.
#' @param new_age_breaks Numeric vector of age breakpoints.
#' @param inputs List with \code{N0} (must have \code{dim1}) and \code{crude_death} (must have \code{dim3}).
#' @param default_inputs List containing \code{population} (a data.frame with \code{dim1}, \code{dim2}, \code{value}).
#'
#' @return List with elements:
#'   \item{aging_rate}{A vector of aging rates (length = number of age groups).}
#'   \item{repro_low}{Index of lowest reproductive age group.}
#'   \item{repro_high}{Index of highest reproductive age group.}
#'   \item{repro_weight}{A data.frame (columns: dim1, dim2, value) giving reproductive weight per group.}
#'
#' @details
#' The reproductive window is ages 15–49 (inclusive, by single-year age index). For aggregated age bands,
#' the function determines which bins overlap with the reproductive window.
#'
#' @importFrom dplyr mutate group_by summarise select %>%
#' @importFrom magrittr %>%
#'
#' @export
calc_aging_and_repro <- function(aggregate_age, new_age_breaks, inputs, default_inputs) {

  aging_rate <- if (aggregate_age) {
    age_correct_last <- new_age_breaks
    age_correct_last[length(age_correct_last)] <- 101
    1 / (365 * base::diff(age_correct_last))
  } else {
    1 / 365
  }

  if (length(inputs$N0$dim1) == 101) {
    repro_low <- 16
    repro_high <- 50
  } else {
    age_lowers <- new_age_breaks[-length(new_age_breaks)]
    age_uppers <- new_age_breaks[-1]
    repro_low  <- min(which(age_uppers >= 16 & age_lowers <= 50))
    repro_high <- max(which(age_lowers <= 50 & age_uppers >= 16))
  }

  if (length(inputs$N0$dim1) == 101) {
    repro_weight <- rep(0, 101)
    repro_weight[16:50] <- 1
    repro_weight <- data.frame(
      dim1 = rep(seq_len(101), times = max(inputs$crude_death$dim3)),
      dim2 = rep(seq_len(max(inputs$crude_death$dim3)), each = 101),
      value = rep(repro_weight, times = max(inputs$crude_death$dim3))
    )
  } else {
    repro_weight <- default_inputs$population %>%
      dplyr::mutate(
        age_group =  findInterval(dim1 - 1, new_age_breaks),
        age_group = as.numeric(as.character(age_group)),
        in_repro = dim1 >= 16 & dim1 <= 50
      ) %>%
      dplyr::group_by(dim2, age_group) %>%
      dplyr::summarise(
        group_pop = sum(value),
        repro_pop = sum(value[in_repro]),
        repro_weight = ifelse(group_pop > 0, repro_pop / group_pop, 0),
        .groups = "drop"
      ) %>%
      dplyr::select(dim1 = age_group, dim2, value = repro_weight)
  }

  list(
    aging_rate = aging_rate,
    repro_low = repro_low,
    repro_high = repro_high,
    repro_weight = repro_weight
  )
}
