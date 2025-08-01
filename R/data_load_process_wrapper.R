#' data_load_process_wrapper
#'
#' Load and Process All Model Input Data
#'
#' This wrapper function loads internal datasets and processes them into structured model input
#' parameters for a specified country, disease, and vaccine. It integrates demographic, vaccination,
#' and disease data, formatting them appropriately for use in the dynamic transmission model within PREVAIL.
#'
#' @param iso A 3-letter ISO country code identifying the country for analysis (e.g., "ETH" for Ethiopia).
#' @param disease A character string specifying the disease of interest (e.g., "measles", "diphtheria", "pertussis").
#' @param R0 Numeric scalar or vector specifying the basic reproduction number, defining disease transmissibility.
#' @param cfr_off Logical; Sets the CFR to zero. Used when calculating current susceptibility as prior mortality rates will already include disease specific mortality.
#' @param year_start Optional numeric value specifying the first year of the simulation window. Default ("") uses the earliest available data year.
#' @param year_end Optional numeric value specifying the last year of the simulation window. Default ("") uses the latest available data year.
#' @param WHO_seed_switch Logical indicating whether to use WHO-style seeding to replicate historical case-reporting patterns (default is `TRUE`).
#' @param aggregate_age Logical indicating whether to aggregate data from single-year age groups into custom age intervals (default is `TRUE`).
#' @param new_age_breaks Numeric vector specifying the breakpoints used to aggregate single-year age groups into broader age bands. Default is `c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)`.
#'
#' @return A named list containing structured parameters ready for use in the PREVAIL dynamic transmission model.
#'
#' @export

data_load_process_wrapper <- function(
    iso,
    disease,
    R0,
    cfr_off = TRUE,
    year_start = "",
    year_end = "",
    WHO_seed_switch = TRUE,
    aggregate_age = TRUE,
    new_age_breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
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
    disease_parameters     = PREVAIL::disease_parameters %>% dplyr::filter(tolower(disease) == !!disease),
    vaccine_parameters     = PREVAIL::vaccine_parameters %>% dplyr::filter(disease == !!disease)
  )

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
    WHO_seed_switch = WHO_seed_switch
  )

  # ---- Time Scaling and Change Points ----
  times <- list(
    mig  = base::sort(base::floor(preprocessed$processed_demographic_data$tt_migration * 365)),
    vac  = base::sort(base::floor(cv_params$tt_vaccination * 365)),
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
    default_inputs = default_inputs,
    disease_param = datasets$disease_parameters
  )

  # ---- Return Packaged Parameters ----
  packed_params <- param_packager(

    #Population parameters
    n_age                        = length(unique(inputs$age_beta_mod$dim1)),
    n_vacc                       = preprocessed$processed_demographic_data$input_data$n_vacc,
    n_risk                       = preprocessed$processed_demographic_data$input_data$n_risk,
    population                   = inputs$population,
    female_population            = inputs$female_population,
    repro_weight                 = aging_reproduction$repro_weight,
    contact_matrix               = inputs$contact_matrix,
    S0                           = inputs$N0,
    Rpop0                        = 0,
    I0                           = 0,
    repro_low                    = aging_reproduction$repro_low,
    repro_high                   = aging_reproduction$repro_high,
    age_maternal_protection_ends = aging_reproduction$maternal_prot_end,

    #Demographic changes
    crude_birth                  = inputs$crude_birth,
    crude_death                  = inputs$crude_death,
    aging_rate                   = aging_reproduction$aging_rate,
    migration_in_number          = inputs$mig_in,
    migration_distribution_values = inputs$mig_dist,
    migration_represent_current_pop = 1,

    #Vaccination parameters
    vaccination_coverage         = inputs$vacc_cov,
    short_term_waning            = 1 / (max(datasets$vaccine_parameters$value[datasets$vaccine_parameters$parameter == "short_term_waning"]) * 365),
    long_term_waning             = 1 / (max(datasets$vaccine_parameters$value[datasets$vaccine_parameters$parameter == "long_term_waning"]) * 365),
    age_vaccination_beta_modifier = inputs$age_beta_mod,
    protection_weight_vacc       = datasets$disease_parameters %>% subset(parameter == "maternal protection (vaccine)") %>% pull(mean)/100 * aging_reproduction$maternal_prot_weight,
    protection_weight_rec        = datasets$disease_parameters %>% subset(parameter == "maternal protection (infection)") %>% pull(mean)/100 * aging_reproduction$maternal_prot_weight,

    #Disease parameters
    incubation_rate              = 1 / datasets$disease_parameters %>% subset(parameter == "incubation period") %>% pull(mean),
    recovery_rate                = 1 /  datasets$disease_parameters %>% subset(parameter == "infectious period") %>% pull(mean),
    severe_recovery_rate         = 1 / datasets$disease_parameters %>% subset(parameter == "infectious period") %>% pull(mean),
    prop_severe = datasets$disease_parameters %>% subset(parameter == "proportion severe (hospitalized)") %>% pull(mean)/100,
    prop_complications = datasets$disease_parameters %>% subset(parameter == "proportion severe (hospitalized)") %>% pull(mean)/100 ,
    natural_immunity_waning      = if (cv_params$nat_waning == 0) 0 else 1 / cv_params$nat_waning,
    R0                           = if (WHO_seed_switch) c(R0, 0) else R0,
    cfr_normal = if(cfr_off == T) 0 else datasets$disease_parameters %>% subset(parameter == "cfr for standard cases") %>% pull(mean),
    cfr_severe = if(cfr_off == T) 0 else datasets$disease_parameters %>% subset(parameter == "cfr for severe cases") %>% pull(mean),

    #Time parameters
    tt_R0                        = if (WHO_seed_switch) c(0, max(c(1, times$seed[2]))) else 0,
    tt_birth_changes             = times$mig,
    tt_death_changes             = times$mig,
    tt_migration                 = times$mig,
    tt_vaccination_coverage      = times$vac,
    tt_seeded                    = if (WHO_seed_switch) times$seed else c(0, max(times$seed)),

    #Cases
    seeded                       = inputs$seeded,
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
