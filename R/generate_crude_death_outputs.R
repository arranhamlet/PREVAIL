#' Generate Aggregated Crude Death Output
#'
#' Computes aggregated crude death counts and rates across new age bands from preprocessed demographic data.
#' The function joins crude death rates to population weights, aggregates by new age groups,
#' and compares total deaths before and after aggregation for consistency checking. Includes a diagnostic plot.
#'
#' @param preprocessed A list-like object containing demographic model inputs, with a component `processed_demographic_data$crude_death`.
#' @param weight_reformatted A data.frame or tibble with columns `time`, `age`, and `value` providing population weights, usually long-format population data.
#' @param age_breaks Numeric vector of age group cut points (e.g., `c(0, 5, 10, 15, 20, ..., 80, 100)`).
#'
#' @return Invisibly returns `TRUE` if total deaths are preserved after aggregation. Also produces a diagnostic line plot (deaths by time, original vs. aggregated).
#'
#' @importFrom dplyr mutate left_join summarise pull group_by rename select
#' @importFrom ggplot2 ggplot aes geom_line theme_bw labs
#' @keywords internal

generate_crude_death_outputs <- function(preprocessed, weight_reformatted, age_breaks) {

  #Set up raw data and join the population weights
  raw_d <- preprocessed$processed_demographic_data$crude_death
  comb_d <- raw_d %>%
    left_join(
      weight_reformatted %>%
        dplyr::rename(population = value), by = c("dim3" = "time", "dim1" = "age")
    )

  age_group_index <- cut(1:101, breaks = age_breaks + 1, right = FALSE, labels = FALSE)
  n_age_new <- length(age_breaks) - 1

  # Assign age group to each row based on dim1
  comb_d$age_group <- age_group_index[comb_d$dim1]


  #Work out proportion per age group and then
  agg_combo <- comb_d %>%
    mutate(deaths = population * value) %>%
    group_by(age_group, dim3) %>%
    summarise(
      deaths = sum(deaths),
      population = sum(population)
    ) %>%
    mutate(
      crude_death = deaths
    )

  death_upd <- comb_d %>%
    mutate(
      death = value * population
    ) %>%
    group_by(
      dim3
    ) %>%
    summarise(death = sum(death)) %>%
    mutate(type = "default")

  agg_upd <- agg_combo %>%
    select(-deaths) %>%
    dplyr::rename(death = crude_death,
                  dim1 = age_group) %>%
    group_by(
      dim3
    ) %>%
    summarise(death = 1000 * sum(death)) %>%
    mutate(type = "agg")


  #Why no line up
  ggplot(data = rbind(death_upd, agg_upd),
         mapping = aes(x = dim3, y = death, color = type)) +
    geom_line() +
    theme_bw() +
    labs(x = "", y = "")

  sum(agg_combo$crude_death) == sum(comb_d$value * comb_d$population)




}
