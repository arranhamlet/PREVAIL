#' Aggregate or Default Model Inputs by Age Structure
#'
#' This helper function performs optional aggregation of model inputs into a new age structure.
#' It returns both the default unaggregated inputs and the aggregated inputs (if requested).
#'
#' @param preprocessed A list from prepare_model_inputs() including demographic data.
#' @param cv_params Output from case_vaccine_to_param().
#' @param new_age_breaks Numeric vector of age breakpoints.
#' @param aggregate_age Logical. If TRUE, aggregates into new_age_breaks.
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item default_inputs: Model inputs without age aggregation.
#'     \item inputs: Aggregated model inputs (if aggregate_age is TRUE), else same as default.
#'   }
#' @importFrom dplyr mutate select rename left_join pull group_by ungroup arrange case_when
#' @importFrom tidyr pivot_longer replace_na drop_na
#' @importFrom tibble rownames_to_column
#' @keywords internal
aggregate_inputs <- function(preprocessed,
                             cv_params,
                             new_age_breaks,
                             aggregate_age) {

  n_age <- preprocessed$processed_demographic_data$input_data$n_age

  weight_reformatted <- preprocessed$processed_demographic_data$population_data %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "time") %>%
    tidyr::pivot_longer(cols = -time, names_to = "age", values_to = "value") %>%
    dplyr::mutate(time = as.integer(time),
                  age = as.integer(age),
                  year = (preprocessed$processed_demographic_data$input_data$year_start - 1) + time) %>%
    dplyr::group_by(time)

  crude_birth_augmented <- preprocessed$processed_demographic_data$crude_birth %>%
    dplyr::select(-population) %>%
    dplyr::left_join(
      preprocessed$processed_demographic_data$female_population %>%
        dplyr::mutate(dim1 = as.integer(dim1)),
      by = c("dim1", "dim2")
    ) %>%
    dplyr::mutate(value = value / 365)

  #Update vaccination
  vaccination_coverage_expanded <- generate_array_df(
    dim1 = preprocessed$processed_demographic_data$input_data$n_age,
    dim2 = preprocessed$processed_demographic_data$input_data$n_vacc,
    dim3 = preprocessed$processed_demographic_data$input_data$n_risk,
    dim4 = max(cv_params$vaccination_coverage$dim4),
    updates = cv_params$vaccination_coverage
  ) %>%
    group_by(dim4) %>%
    mutate(
      year = ifelse(is.na(year), unique(na.omit(year)), year)
    ) %>%
    ungroup() %>%
    #Update value to get correct coverage -
    mutate(
      value = 1 - (1 - pmin(value, 0.999)) ^ (1/365)
    )

  vaccination_people <- vaccination_coverage_expanded %>%
    dplyr::left_join(
      weight_reformatted %>%
        rename(pop = value), by = c("year" = "year", "dim1" = "age")
    ) %>%
    dplyr::mutate(value = pop * (value)) %>%
    tidyr::drop_na()

  cv_params$age_vaccination_beta_modifier_upd <- cv_params$age_vaccination_beta_modifier %>%
    dplyr::left_join(
      weight_reformatted %>%
        group_by(age) %>%
        summarise(value = sum(value)) %>%
        rename(pop = value), by = c("dim1" = "age")
    ) %>%
    dplyr::mutate(value = pop * value) %>%
    select(-pop)

  default_inputs <- list(
    age_beta_mod   = cv_params$age_vaccination_beta_modifier,
    vacc_cov       = vaccination_people %>%
      dplyr::mutate(value = value/pop),
    N0             = preprocessed$processed_demographic_data$N0,
    crude_death    = preprocessed$processed_demographic_data$crude_death %>%
      dplyr::mutate(value = value / 365),
    crude_birth    = crude_birth_augmented %>%
      dplyr::mutate(value = value / population / 1000),
    mig_in         = preprocessed$processed_demographic_data$migration_in_number %>%
      dplyr::mutate(value = value / 365),
    mig_dist       = preprocessed$processed_demographic_data$migration_distribution_values,
    seeded         = cv_params$seed_data,
    contact_matrix = preprocessed$processed_demographic_data$contact_matrix,
    population     = weight_reformatted %>%
      dplyr::rename(dim1 = age, dim2 = time),
    female_population = preprocessed$processed_demographic_data$female_population %>%
      dplyr::rename(value = population)
  )

  if (!aggregate_age) {
    return(list(default_inputs = default_inputs, inputs = default_inputs))
  }

  aging_vector <- diff(new_age_breaks) / 365
  aging_vector[is.infinite(aging_vector)] <- 0
  n_age <- length(new_age_breaks) - 1

  death_upd <- preprocessed$processed_demographic_data$crude_death %>%
    dplyr::mutate(value = value / 365 * weight_reformatted$value)

  inputs <- list(
    age_beta_mod   = aggregate_age_structure(cv_params$age_vaccination_beta_modifier_upd, age_breaks = new_age_breaks, method = "sum", weights = weight_reformatted, time_var = NULL),
    vacc_cov       = aggregate_age_structure(vaccination_people %>%
                                               select(dim1:year), age_breaks = new_age_breaks, method = "sum", weights = weight_reformatted, time_var = "dim4"),
    N0             = aggregate_age_structure(preprocessed$processed_demographic_data$N0, age_breaks = new_age_breaks, method = "sum", time_var = NULL),
    crude_death    = aggregate_age_structure(death_upd, age_breaks = new_age_breaks, method = "sum", time_var = "dim3"),
    crude_birth    = aggregate_age_structure(crude_birth_augmented %>% dplyr::select(-population), age_breaks = new_age_breaks, method = "sum", time_var = "dim2"),
    mig_in         = aggregate_age_structure(preprocessed$processed_demographic_data$migration_in_number %>% dplyr::mutate(value = value / 365), age_breaks = new_age_breaks, method = "sum", time_var = "dim4"),
    mig_dist       = aggregate_age_structure(preprocessed$processed_demographic_data$migration_distribution_values, age_breaks = new_age_breaks, method = "weighted.mean", weights = weight_reformatted, time_var = "dim2"),
    seeded         = aggregate_age_structure(cv_params$seed_data, age_breaks = new_age_breaks, method = "sum", time_var = "dim4"),
    contact_matrix = aggregate_contact_matrix(preprocessed$processed_demographic_data$contact_matrix, age_breaks = new_age_breaks, population = weight_reformatted, symmetric = TRUE),
    population     = aggregate_age_structure(weight_reformatted %>% dplyr::rename(dim1 = age, dim2 = time), age_breaks = new_age_breaks, method = "sum", time_var = "dim2"),
    female_population = aggregate_age_structure(preprocessed$processed_demographic_data$female_population %>% dplyr::rename(value = population), age_breaks = new_age_breaks, method = "sum", time_var = "dim2")
  )

  inputs$crude_birth <- inputs$crude_birth %>%
    dplyr::ungroup() %>%
    dplyr::select(-age_group) %>%
    dplyr::mutate(dim1 = as.numeric(dim1), dim2 = as.numeric(dim2)) %>%
    dplyr::group_by(dim1, dim2) %>%
    dplyr::left_join(
      inputs$female_population %>% dplyr::mutate(dim2 = as.integer(dim2)) %>%
        dplyr::rename(population = value),
      by = c("dim1", "dim2")
    ) %>%
    dplyr::mutate(value = value / population / 1000)

  inputs$crude_death <- inputs$crude_death %>%
    dplyr::mutate(dim1 = as.numeric(dim1), dim2 = as.numeric(dim2), dim3 = as.numeric(dim3)) %>%
    dplyr::arrange(dim1, dim2, dim3) %>%
    dplyr::left_join(
      inputs$population %>% dplyr::ungroup() %>% dplyr::select(dim1, dim2, population = value),
      by = c("dim1", "dim3" = "dim2")
    ) %>%
    dplyr::mutate(value = value / population)

  inputs$vacc_cov <- inputs$vacc_cov %>%
    dplyr::mutate(dim1 = as.numeric(dim1), dim2 = as.numeric(dim2), dim3 = as.numeric(dim3), dim4 = as.numeric(dim4)) %>%
    dplyr::arrange(dim1, dim2, dim3, dim4) %>%
    dplyr::left_join(
      inputs$population %>% dplyr::select(dim1, year, population = value),
      by = c("year", "dim1")
    ) %>%
    dplyr::mutate(value = value / population)

  inputs$age_beta_mod <- inputs$age_beta_mod %>%
    dplyr::mutate(dim1 = as.numeric(dim1), dim2 = as.numeric(dim2), dim3 = as.numeric(dim3)) %>%
    dplyr::arrange(dim1, dim2, dim3) %>%
    dplyr::left_join(
      inputs$population %>% dplyr::group_by(dim1) %>% dplyr::summarise(value = sum(value)) %>% dplyr::select(dim1, population = value),
      by = c("dim1")
    ) %>%
    dplyr::mutate(value = value / population)

  return(list(default_inputs = default_inputs, inputs = inputs))
}
