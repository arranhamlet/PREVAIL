#' Expand Pre-1980 Routine Vaccination Coverage
#'
#' Uses historical assumptions to estimate vaccination coverage prior to the first observed year.
#' Applies linear interpolation from an assumed introduction year up to the first observed coverage year.
#'
#' @param processed_vaccination Routine coverage data (to determine first year and vaccine).
#' @param vaccination_pre1980 Data frame of historical vaccine assumptions (intro year, starting coverage).
#' @param disease Disease name (lowercase) to match.
#' @param iso 3-letter ISO country code.
#'
#' @return A data frame of additional rows to prepend to coverage time series.
#' @importFrom data.table as.data.table setDT setnames rbindlist copy data.table
#' @keywords internal
expand_pre1980_vaccination <- function(processed_vaccination, vaccination_pre1980, disease, iso) {
  # Ensure input is data.table
  vac_pre1980_sub <- data.table::as.data.table(vaccination_pre1980)
  data.table::setDT(vac_pre1980_sub)

  vac_pre1980_sub[, rowname := .I]  # preserve row identity
  vac_pre1980_sub[, income_group := paste0(paste0(substr(unlist(strsplit(income_group, " |-")), 1, 1), collapse = ""), "C"),
                  by = rowname]

  setnames(vac_pre1980_sub, "disease", "disease_n")

  vac_pre1980_sub <- vac_pre1980_sub[
    tolower(disease_n) == disease &
      who_region == paste0(get_WHO_region(iso), "O") &
      income_group == as.character(get_income_group(iso))
  ]

  if (nrow(vac_pre1980_sub) == 0 || nrow(processed_vaccination) == 0) return(processed_vaccination)

  # Remove special populations
  processed_vaccination <- processed_vaccination[!grepl("birth|neonatal|pregnant|maternal",
                                                        processed_vaccination$vaccine_description, ignore.case = TRUE), ]

  first_year_vac <- processed_vaccination[processed_vaccination$year == min(processed_vaccination$year), ]

  year_diff <- min(first_year_vac$year) - vac_pre1980_sub$introduction_year
  if (year_diff <= 0) return(processed_vaccination)

  # Precompute interpolation
  vac_prop <- lapply(first_year_vac$coverage, function(e) {
    seq(
      from = vac_pre1980_sub$starting_coverage_percent,
      to = max(vac_pre1980_sub$starting_coverage_percent, e),
      length.out = 6
    )
  })

  # Efficient row expansion
  pre_1980 <- rbindlist(lapply(seq_len(year_diff), function(x) {
    rbindlist(lapply(seq_len(nrow(first_year_vac)), function(k) {
      row <- copy(first_year_vac[k])
      row$year <- vac_pre1980_sub$introduction_year + (x - 1)
      row$coverage <- vac_prop[[k]][min(x, length(vac_prop[[k]]))]
      row
    }))
  }))

  data.table::rbindlist(list(pre_1980, processed_vaccination))
}



#' Format Case and Vaccination Parameters for Model Input (Including Pre-1980)
#'
#' Wrapper function that prepares all time-varying parameters for use in an age-, vaccine-,
#' and risk-structured infectious disease model, including extrapolated vaccination coverage
#' prior to 1980 if available.
#'
#' @param demog_data Output list from `process_demography()`.
#' @param processed_vaccination Data frame of routine vaccine coverage.
#' @param processed_vaccination_sia Data frame of SIA coverage.
#' @param processed_case WHO case reports processed for the country of interest.
#' @param vaccination_schedule Full WHO vaccine schedule data frame.
#' @param vaccination_pre1980 Data frame of historical vaccination introduction assumptions.
#' @param vaccine_parameters Data frame of vaccination parameters.
#' @param custom_routine_vaccination Data frame or NA indicating custom routine vaccination.
#' @param custom_sia_vaccination Data frame or NA indicating custom sia vaccination.
#' @return A named list of parameters including coverage arrays and seeding inputs.
#'
#' @importFrom data.table data.table setDT rbindlist
#' @importFrom stats setNames
#' @keywords internal
case_vaccine_to_param <- function(
    demog_data,
    processed_vaccination,
    processed_vaccination_sia,
    processed_case,
    vaccination_schedule,
    vaccination_pre1980,
    vaccine_parameters,
    disease_parameters,
    WHO_seed_switch,
    custom_routine_vaccination = NA,
    custom_sia_vaccination = NA
) {

  iso <- demog_data$input_data$iso
  n_age <- demog_data$input_data$n_age
  ages <- 0:(n_age - 1)
  years <- demog_data$input_data$year_start:demog_data$input_data$year_end

  # Use data.table for combined extraction
  vaccination_sources <- c(
    processed_case$disease_description,
    processed_vaccination_sia$vaccination_name,
    processed_vaccination_sia$disease,
    processed_vaccination$vaccine,
    processed_vaccination$vaccine_description
  )

  vaccination_type <- paste(unique(vaccination_sources), collapse = "|")

  if (grepl("Diphtheria|Pertussis", vaccination_type, ignore.case = TRUE)) {
    vaccination_type <- paste0(vaccination_type, "|DTPCV1|DTPCV3|DTaP|DT|DTwP")
  }

  # Expand routine coverage pre-1980
  if(all(is.na(custom_routine_vaccination))){
    processed_vaccination <- expand_pre1980_vaccination(
      processed_vaccination,
      vaccination_pre1980 %>%
        subset(introduction_year %in% demog_data$input_data$year_start:demog_data$input_data$year_end),
      disease = tolower(unique(processed_case$disease_description)[1]),
      iso = iso
    )
  }
  # Filter and build vaccination input
  schedule <- filter_vaccine_schedule(vaccination_schedule, vaccination_type, iso)
  routine_df <- build_routine_vaccination_param(processed_vaccination, schedule, ages, years)

  #Correct for the routine data being recorded in %
  if(all(is.na(custom_routine_vaccination))){
    routine_df$value <- routine_df$value/100
  }

  # Build SIA input if needed
  sia_df <- if(nrow(processed_vaccination_sia) > 0) {
    build_sia_vaccination_param(processed_vaccination_sia, ages, years)
  } else {
    NULL
  }

  # Combine and deduplicate
  vacc_df <- combine_vaccination_params(routine_df, sia_df)
  data.table::setDT(vacc_df)

  zero_row <- rbind(data.table::data.table(dim1 = 1, dim2 = 1, dim3 = 1, dim4 = 0, year = 1950, value = 0))

  vacc_df <- rbindlist(list(zero_row, vacc_df))
  vacc_df <- vacc_df[, .(value = max(value)), by = .(dim1, dim2, dim3, dim4, year)]

  # Build seeded case input
  if (nrow(processed_case) > 0) {
    case_df <- build_seeded_case_param(processed_case, demog_data, years, ages)
    tt_seeded <- sort(c(0, match(unique(processed_case$year), years) - 1))
  } else {
    case_df <- data.table::data.table(dim1 = 1, dim2 = 1, dim3 = 1, dim4 = 1, value = 10)
    tt_seeded <- 0
  }

  # ---- Build Age-Vaccination Modifier Structure ----
  n_vacc <- demog_data$input_data$n_vacc
  vacc_order <- seq(2, n_vacc, by = 2)

  age_vaccination_beta_modifier <- purrr::map_dfr(vacc_order, function(j) {
    dose_details <- vaccine_parameters %>%
      dplyr::mutate(order = abs(j - dose)) %>%
      dplyr::filter(order == min(order))

    n_age <- demog_data$input_data$n_age

    short_term <- base::expand.grid(
      dim1 = seq_len(n_age),
      dim2 = j,
      dim3 = 1,
      value = dose_details$value[dose_details$parameter == "short_term_protection"]
    )

    long_term <- base::expand.grid(
      dim1 = seq_len(n_age),
      dim2 = j + 1,
      dim3 = 1,
      value = dose_details$value[dose_details$parameter == "long_term_protection"]
    )

    dplyr::bind_rows(short_term, long_term)
  })


  # ---- Natural Immunity Waning ----
  nat_waning <- disease_parameters %>%
    dplyr::filter(parameter == "natural immunity waning") %>%
    dplyr::pull(value) %>%
    tidyr::replace_na(0) %>%
    base::gsub("NA", 0, .) %>%
    base::as.numeric() * 365

  seed_time <- base::sort(base::floor(tt_seeded * 365))

  # ---- WHO Seeding ----
  if (WHO_seed_switch) {

    # Base seed entries: all except dim4 = 1, and doubled for WHO style
    base_seed <- case_df %>%
      dplyr::filter(dim4 != 1) %>%
      dplyr::mutate(dim4 = (dim4 * 2) - 2)

    # Duplicate with value = 0 and dim4 shifted forward
    zero_seed <- base_seed %>%
      dplyr::mutate(value = 0, dim4 = dim4 + 1)

    # Insert "patch" value for initial seeding
    zero_seed <- dplyr::bind_rows(
      zero_seed,
      zero_seed %>%
        dplyr::filter(dim4 == 3) %>%
        dplyr::mutate(dim4 = 1)
    ) %>%
      dplyr::mutate(value = dplyr::case_when(
        dim4 == 1 & dim1 == 1 ~ 10,
        TRUE ~ value
      ))

    seed_data <- dplyr::bind_rows(base_seed, zero_seed) %>%
      dplyr::arrange(dim4)

    # Adjust timepoints to match WHO-style seeding schedule
    original_times <- seed_time
    replicated <- base::unlist(base::lapply(original_times[original_times != 0], function(e) c(e, e + 1)))
    seed_time <- c(0, base::sort(replicated), base::max(replicated) + 364, base::max(replicated) + 365)

  } else {

    # Minimal fallback if WHO seed switch is off
    seed_time <- c(base::min(seed_time), base::max(seed_time) + 1)

    seed_data <- base::data.frame(
      dim1 = 1, dim2 = 1, dim3 = 1,
      dim4 = seq_along(seed_time),
      value = 10
    )
  }

  #Recalibrate dim4
  vacc_df <- vacc_df %>%
    arrange(dim4, year, dim1, dim2, dim3) %>%
    mutate(dim4 = dplyr::dense_rank(year))

  list(
    tt_vaccination = sort(replace(unique(match(vacc_df$year, years)), 1, 0)),
    vaccination_coverage = vacc_df,
    tt_seeded = seed_time,
    seeded = case_df,
    nat_waning = nat_waning,
    age_vaccination_beta_modifier = age_vaccination_beta_modifier,
    seed_data = seed_data
  )
}
