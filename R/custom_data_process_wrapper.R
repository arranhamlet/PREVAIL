#' custom_data_process_wrapper
#'
#' Load, Process, and Optionally Override All Model Input Data
#'
#' This wrapper function loads internal datasets and processes them into structured model input
#' parameters for a specified country, disease, and vaccine. Compared to \code{data_load_process_wrapper},
#' this function allows users to override any internal demographic, vaccination, or disease dataset
#' with a custom data frame provided through the \code{custom_*} arguments. All data are formatted
#' appropriately for use in the dynamic transmission model within PREVAIL.
#'
#' Any \code{custom_*} argument can be used to override the corresponding internal dataset
#' with a user-supplied data frame. If provided (not NA), these inputs are reformatted as needed
#' and replace the matching internal dataset for this model run.
#'
#' @param iso A 3-letter ISO country code identifying the country for analysis (e.g., "ETH" for Ethiopia).
#' @param disease A character string specifying the disease of interest (e.g., "measles", "diphtheria", "pertussis").
#' @param R0 Numeric scalar or vector specifying the basic reproduction number, defining disease transmissibility.
#' @param year_start Optional numeric value specifying the first year of the simulation window. Default ("") uses the earliest available data year.
#' @param year_end Optional numeric value specifying the last year of the simulation window. Default ("") uses the latest available data year.
#' @param WHO_seed_switch Logical; use WHO-style seeding to replicate historical case-reporting patterns (default TRUE).
#' @param aggregate_age Logical; aggregate data from single-year age groups into custom intervals (default TRUE).
#' @param new_age_breaks Numeric vector; breakpoints for age group aggregation. Default is c(0, 5, ..., Inf).
#'
#' @param custom_migration Optional. Data frame to override default migration data.
#' @param custom_fertility Optional. Data frame to override default fertility data.
#' @param custom_mortality Optional. Data frame to override default mortality data.
#' @param custom_population Optional. Data frame to override default population (total) data.
#' @param custom_contact_matricies Optional. Data frame to override default contact matrix data.
#' @param custom_routine_vaccination Optional. Data frame to override default routine vaccination data.
#' @param custom_sia_vaccination Optional. Data frame to override default SIA vaccination data.
#' @param custom_disease_data Optional. Data frame to override default disease surveillance data.
#' @param custom_vaccination_schedule Optional. Data frame to override default vaccination schedule.
#' @param custom_disease_parameters Optional. Data frame to override default disease parameter values.
#' @param custom_vaccine_parameters Optional. Data frame to override default vaccine parameter values.
#'
#' @return A named list containing structured parameters ready for use in the PREVAIL dynamic transmission model.
#'
#' @details
#' If any \code{custom_*} argument is provided, it replaces the corresponding PREVAIL internal dataset for that run.
#' Demographic custom data is reformatted with \code{\link{reformat_demographic_data}} before use.
#'
#' @export


custom_data_process_wrapper <- function(
    iso,
    disease,
    R0,
    year_start = "",
    year_end = "",
    WHO_seed_switch = TRUE,
    aggregate_age = TRUE,
    new_age_breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf),
    custom_migration = NA,
    custom_fertility = NA,
    custom_mortality = NA,
    custom_population = NA,
    custom_contact_matricies = NA,
    custom_routine_vaccination = NA,
    custom_sia_vaccination = NA,
    custom_disease_data = NA,
    custom_vaccination_schedule = NA,
    custom_disease_parameters = NA,
    custom_vaccine_parameters = NA
) {

  # ---- Load Package Data ----
  datasets <- list(
    migration              = PREVAIL::UN_WPP_migration,
    fertility              = PREVAIL::UN_WPP_fertility,
    mortality              = PREVAIL::UN_WPP_mortality,
    population_all         = PREVAIL::UN_WPP_population_total,
    population_female      = PREVAIL::UN_WPP_population_female,
    contact_matricies      = PREVAIL::contact_matricies,
    routine_vaccination    = PREVAIL::WHO_vaccination_routine,
    sia_vaccination        = PREVAIL::VIMC_vaccination_sia,
    disease_data           = PREVAIL::WHO_disease_reports,
    vaccination_schedule   = PREVAIL::WHO_vaccination_schedule,
    pre1980                = PREVAIL::vaccination_pre1980,
    disease_parameters     = PREVAIL::disease_parameters %>% dplyr::filter(disease == !!disease),
    vaccine_parameters     = PREVAIL::vaccine_parameters %>% dplyr::filter(disease == !!disease)
  )

  #Get years
  years_all <- get_years(1950:2024, start = year_start, end = year_end)

  # ---- Override with Custom Inputs if Provided ----
  if (all(!is.na(custom_migration)))            datasets$migration            <- reformat_demographic_data(custom_data = custom_migration, age_required = NA, iso = iso, years = years_all, fill_method = "closest", value_allocation = "maintain")
  if (all(!is.na(custom_fertility)))            datasets$fertility            <- reformat_demographic_data(custom_data = custom_fertility, age_required = 15:49, iso = iso, years = years_all, fill_method = "closest", value_allocation = "maintain")
  if (all(!is.na(custom_mortality)))            datasets$mortality            <- reformat_demographic_data(custom_data = custom_mortality, age_required = 0:100, iso = iso, years = years_all, fill_method = "closest", value_allocation = "maintain")
  if (all(!is.na(custom_population)))           datasets$population_all       <- reformat_demographic_data(custom_data = custom_population, age_required = 0:100, iso = iso, years = years_all, fill_method = "closest", value_allocation = "split")
  if (all(!is.na(custom_contact_matricies)))    datasets$contact_matricies    <- custom_contact_matricies
  if (all(!is.na(custom_routine_vaccination)))  datasets$routine_vaccination  <- reformat_vaccination(custom_routine_vaccination, iso = iso, disease = disease)
  if (all(!is.na(custom_sia_vaccination)))      datasets$sia_vaccination      <- reformat_vaccination(custom_sia_vaccination, iso = iso, disease = disease)
  if (all(!is.na(custom_disease_data)))         datasets$disease_data         <- custom_disease_data
  if (all(!is.na(custom_vaccination_schedule))) datasets$vaccination_schedule <- reformat_vaccination_schedule(custom_vaccination_schedule, iso = iso, disease = disease)
  if (all(!is.na(custom_disease_parameters)))   datasets$disease_parameters   <- reformat_parameters(custom_disease_parameters, disease = disease, type = "disease")
  if (all(!is.na(custom_vaccine_parameters)))   datasets$vaccine_parameters   <- reformat_parameters(custom_vaccine_parameters, disease = disease, type = "vaccine")

  # ---- Prepare Inputs ----
  preprocessed <- prepare_model_inputs(
    iso = iso,
    disease = disease,
    vaccine = disease,
    n_age = 101,
    migration = datasets$migration,
    fertility = datasets$fertility,
    mortality = datasets$mortality,
    population_all = datasets$population_all,
    population_female = datasets$population_female,
    contact_matricies = datasets$contact_matricies,
    disease_data = datasets$disease_data,
    vaccination_data_routine = datasets$routine_vaccination,
    vaccination_data_sia = datasets$sia_vaccination,
    year_start = year_start, year_end = year_end
  )

  # ---- Generate Parameters from Case and Vaccine Data ----
  cv_params <- case_vaccine_to_param(
    demog_data = preprocessed$processed_demographic_data,
    processed_vaccination = preprocessed$processed_vaccination_data,
    processed_vaccination_sia = preprocessed$processed_vaccination_sia,
    processed_case = preprocessed$processed_case_data,
    vaccination_schedule = datasets$vaccination_schedule %>% dplyr::filter(iso3 == iso),
    vaccination_pre1980 = datasets$pre1980,
    vaccine_parameters = datasets$vaccine_parameters,
    disease_parameters = datasets$disease_parameters,
    WHO_seed_switch = WHO_seed_switch,
    custom_routine_vaccination = custom_routine_vaccination,
    custom_sia_vaccination = custom_sia_vaccination
  )

  # ---- Time Scaling and Change Points ----
  times <- list(
    mig  = base::sort(with(preprocessed$processed_demographic_data, base::floor(c(tt_migration, base::max(tt_migration) + 1) * 365))),
    vac  = base::sort(with(cv_params, base::floor(c(tt_vaccination, base::max(tt_vaccination) + 1) * 365))),
    seed = base::sort(base::floor(cv_params$tt_seeded))
  )

  # ---- Optional Aggregation to New Age Structure ----
  aggregated_inputs <- aggregate_inputs(
    preprocessed = preprocessed,
    cv_params = cv_params,
    new_age_breaks = new_age_breaks,
    aggregate_age = aggregate_age
  )

  inputs <- aggregated_inputs$inputs
  default_inputs <- aggregated_inputs$default_inputs

  #Update aging and calculation of reproduction parameters
  aging_reproduction <- calc_aging_and_repro(
    aggregate_age = aggregate_age,
    new_age_breaks = new_age_breaks,
    inputs = inputs,
    default_inputs = default_inputs
  )

  # ---- Return Packaged Parameters ----
  packed_params <- param_packager(
    n_age                        = length(unique(inputs$age_beta_mod$dim1)),
    n_vacc                       = preprocessed$processed_demographic_data$input_data$n_vacc,
    n_risk                       = preprocessed$processed_demographic_data$input_data$n_risk,
    short_term_waning            = 1 / (max(datasets$vaccine_parameters$value[datasets$vaccine_parameters$parameter == "short_term_waning"]) * 365),
    long_term_waning             = 1 / (max(datasets$vaccine_parameters$value[datasets$vaccine_parameters$parameter == "long_term_waning"]) * 365),
    incubation_rate              = 1 / as.numeric(datasets$disease_parameters$value[datasets$disease_parameters$parameter == "incubation period"]),
    recovery_rate                = 1 / as.numeric(datasets$disease_parameters$value[datasets$disease_parameters$parameter == "infectious period"]),
    severe_recovery_rate         = 1 / as.numeric(datasets$disease_parameters$value[datasets$disease_parameters$parameter == "infectious period"]),
    natural_immunity_waning      = if (cv_params$nat_waning == 0) 0 else 1 / cv_params$nat_waning,
    R0                           = if (WHO_seed_switch) c(R0, 0) else R0,
    tt_R0                        = if (WHO_seed_switch) c(0, max(c(1, times$seed[2]))) else 0,
    vaccination_coverage         = inputs$vacc_cov,
    contact_matrix               = inputs$contact_matrix,
    age_vaccination_beta_modifier = inputs$age_beta_mod,
    S0                           = inputs$N0,
    Rpop0                        = 0,
    I0                           = 0,

    tt_birth_changes             = times$mig,
    tt_death_changes             = times$mig,
    tt_migration                 = times$mig,
    tt_vaccination_coverage      = times$vac,
    tt_seeded                    = if (WHO_seed_switch) times$seed else c(0, max(times$seed)),

    crude_birth                  = inputs$crude_birth,
    crude_death                  = inputs$crude_death,
    aging_rate                   = aging_reproduction$aging_rate,
    migration_in_number          = inputs$mig_in,
    migration_distribution_values = inputs$mig_dist,
    seeded                       = inputs$seeded,
    repro_low                    = aging_reproduction$repro_low,
    repro_high                   = aging_reproduction$repro_high,
    age_maternal_protection_ends = 1,
    protection_weight_vacc       = 0,
    protection_weight_rec        = 0,
    migration_represent_current_pop = 1,
    population                   = inputs$population,
    female_population            = inputs$female_population,
    repro_weight = aging_reproduction$repro_weight
  )

  # Attach input metadata
  packed_params$input_data <- data.frame(
    iso              = iso,
    disease          = disease,
    R0               = R0,
    year_start       = preprocessed$processed_demographic_data$input_data$year_start,
    year_end         = preprocessed$processed_demographic_data$input_data$year_end,
    WHO_seed_switch  = WHO_seed_switch,
    aggregate_age    = aggregate_age,
    age_breaks       = if (aggregate_age) {
      paste(new_age_breaks, collapse = ";")
    } else {
      paste(seq(1, 101), collapse = ";")
    }
  )

  packed_params

}
