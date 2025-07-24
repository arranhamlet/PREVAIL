#' Calculate Aging Rates, Reproductive Parameters, and Maternal Protection
#'
#' Computes aging rates, reproductive age group indices, reproductive weights for birth assignments,
#' and maternal protection end parameters for both single-year and aggregated age bands.
#'
#' @param aggregate_age Logical; if `TRUE`, ages are aggregated into `new_age_breaks`.
#' @param new_age_breaks Numeric vector specifying the breakpoints for aggregated age groups.
#' @param inputs List containing demographic inputs:
#'   - `N0`: Initial population structure (must contain `dim1`).
#'   - `crude_death`: Crude death rates (must contain `dim3`).
#' @param default_inputs List containing default population data:
#'   - `population`: Data.frame with columns `dim1`, `dim2`, and `value`.
#' @param disease_param Data.frame containing disease-specific parameters, including maternal protection duration.
#'
#' @return A list containing:
#'   - `aging_rate`: Numeric vector of aging rates (per day).
#'   - `repro_low`: Integer index of the lowest reproductive age group.
#'   - `repro_high`: Integer index of the highest reproductive age group.
#'   - `repro_weight`: Data.frame (`dim1`, `dim2`, `value`) of reproductive weights per age group.
#'   - `maternal_prot_end`: Integer index indicating age group at which maternal protection ends.
#'   - `maternal_prot_weight`: Numeric proportion reflecting partial maternal protection coverage within the final age group.
#'
#' @details
#' Reproductive ages are defined as 15–49 years inclusive. When ages are aggregated, the function calculates
#' overlapping proportions of these age bands with the reproductive window. Maternal protection duration
#' is dynamically assigned to the appropriate age group based on provided parameters.
#'
#' @importFrom dplyr mutate group_by summarise select filter pull
#'
#' @export
calc_aging_and_repro <- function(aggregate_age, new_age_breaks, inputs, default_inputs, disease_param) {

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

  mat_prot_end <- disease_param %>%
    dplyr::filter(parameter == "age maternal protection ends") %>%
    mutate(mean_year = case_when(
      unit == "days" ~ mean/365,
      unit == "weeks" ~ mean/52,
      unit == "months" ~ mean/12,
      unit == "years" ~ mean
    )) %>% pull()

  if(length(inputs$N0$dim1) == 101){
    maternal_prot_end <- ceiling(mat_prot_end)
    maternal_prot_weight <- 1 - (maternal_prot_end - mat_prot_end)/maternal_prot_end
  } else {
    age_uppers <- new_age_breaks[-1]
    maternal_prot_end <- pmax(min(which(age_uppers >= ceiling(mat_prot_end))) - 1, 1)
    maternal_prot_weight <- 1 - (age_uppers[maternal_prot_end] - mat_prot_end)/age_uppers[maternal_prot_end]
  }

  list(
    aging_rate = aging_rate,
    repro_low = repro_low,
    repro_high = repro_high,
    repro_weight = repro_weight,
    maternal_prot_end = maternal_prot_end,
    maternal_prot_weight = maternal_prot_weight
  )
}
