#Install missing packages, and load
pacman::p_load(
  PREVAIL,
  readxl,
  janitor,
  rio,
  here,
  stringi,
  tidyverse
)


# Import UN data and reformat ---------------------------------------------

#Load in raw UN data. The first 16 lines are information on the file and a logo, so we can skip them. We are also going to use clean_names() from the janitor package to clean up the names

fertility_raw <- read_xlsx(here("inst", "extdata", "data-raw", "WPP2024_FERT_F01_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx"), skip = 16, col_types = "text") %>%
  clean_names()


# DUE TO THE SIZE OF THE FILES, THESE TWO WILL NOT BE INCLUDED IN THE DATA-RAW FOLDER. Instructions to download are found in the readme.

femalepop_raw <- read_xlsx(here("inst", "extdata", "data-raw", "WPP2024_POP_F01_3_POPULATION_SINGLE_AGE_FEMALE.xlsx"), skip = 16, col_types = "text") %>%
  clean_names()

allpop_raw <- read_xlsx(here("inst", "extdata", "data-raw", "WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx"), skip = 16, col_types = "text") %>%
  clean_names()

###################################################################

mortality_raw <- read_xlsx(here("inst", "extdata", "data-raw", "WPP2024_MORT_F01_1_DEATHS_SINGLE_AGE_BOTH_SEXES.xlsx"), skip = 16, col_types = "text") %>%
  clean_names()

migration_raw <- read_xlsx(here("inst", "extdata", "data-raw", "WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"), skip = 16, col_types = "text") %>%
  clean_names()

# Reformat UN data
fertility_raw_upd <- fertility_raw %>%
  #Select the columns we want, and rename the larger names to shorter ones
  select(
    area = region_subregion_country_or_area,
    iso3 = iso3_alpha_code,
    year,
    x15:x49
  ) %>%
  #Subset to only specific countries/territories rather than larger geographies such as "World"
  filter(!is.na(iso3)) %>%
  #Mutate to numeric
  mutate(across(
    .cols = year:x49,
    .fns = as.numeric
  ))

femalepop_raw_upd <- femalepop_raw %>%
  #Select the columns we want, and rename the larger names to shorter ones
  select(
    area = region_subregion_country_or_area,
    iso3 = iso3_alpha_code,
    year,
    x0:x100
  ) %>%
  #Subset to only specific countries/territories rather than larger geographies such as "World"
  filter(!is.na(iso3)) %>%
  #Mutate to numeric
  mutate(across(
    .cols = year:x100,
    .fns = as.numeric
  ))

allpop_raw_upd <- allpop_raw %>%
  #Select the columns we want, and rename the larger names to shorter ones
  select(
    area = region_subregion_country_or_area,
    iso3 = iso3_alpha_code,
    year,
    x0:x100
  ) %>%
  #Subset to only specific countries/territories rather than larger geographies such as "World"
  filter(!is.na(iso3)) %>%
  #Mutate to numeric
  mutate(across(
    .cols = year:x100,
    .fns = as.numeric
  ))

mortality_raw_upd <- mortality_raw %>%
  #Select the columns we want, and rename the larger names to shorter ones
  select(
    area = region_subregion_country_or_area,
    iso3 = iso3_alpha_code,
    year,
    x0:x100
  ) %>%
  #Subset to only specific countries/territories rather than larger geographies such as "World"
  filter(!is.na(iso3)) %>%
  #Mutate to numeric
  mutate(across(
    .cols = year:x100,
    .fns = as.numeric
  ))

migration_raw_upd <- migration_raw %>%
  #Select the columns we want, and rename the larger names to shorter ones
  select(
    area = region_subregion_country_or_area,
    iso3 = iso3_alpha_code,
    year,
    migration_rate_1000 = net_migration_rate_per_1_000_population
  ) %>%
  #Subset to only specific countries/territories rather than larger geographies such as "World"
  filter(!is.na(iso3)) %>%
  #Mutate to numeric
  mutate(across(
    .cols = year:migration_rate_1000,
    .fns = as.numeric
  ))


# Import WHO vaccine and case data ----------------------------------------
cases_raw <- read_excel(here("inst", "extdata", "data-raw", "reported-cases-data.xlsx")) %>%
  clean_names()

routinevacc_raw <- read_excel(here("inst", "extdata", "data-raw", "coverage-data.xlsx")) %>%
  clean_names()

vaccine_abbreviations_diseases <- read.csv(here("inst", "extdata", "data-raw", "vaccine_abbreviations_diseases.csv"))

schedule_raw <- read_excel(here("inst", "extdata", "data-raw", "vaccine-schedule-data.xlsx")) %>%
  clean_names()

# Reformat WHO data
cases_upd <- cases_raw %>%
  select(
    iso3 = code,
    name,
    year,
    disease_short = disease,
    disease_description,
    cases
  ) %>%
  #Subset to only specific countries/territories rather than larger geographies such as "World"
  filter(!is.na(iso3)) %>%
  #lowercase
  mutate(
    disease_short = tolower(disease_short)
  )

routine_upd <- routinevacc_raw %>%
  select(
    iso3 = code,
    area = name,
    year,
    vaccine = antigen,
    vaccine_description = antigen_description,
    coverage_category,
    coverage_description = coverage_category_description,
    coverage = coverage
  ) %>%
  as.data.frame() %>%
  left_join(
    vaccine_abbreviations_diseases,
    by = c("vaccine" = "Abbreviation")
  )

schedule_upd <- schedule_raw %>%
  select(
    iso3 = iso_3_code,
    area = countryname,
    WHO_region = who_region,
    year,
    vaccine_code = vaccinecode,
    vaccine_description,
    schedulerounds,
    target_pop = targetpop,
    target_pop_description = targetpop_description,
    geoarea,
    age_administered = ageadministered,
    comment = sourcecomment
  )


# Load in and prepare contact matricies -----------------------------------
contact_matrix_files <- list.files(here("inst", "extdata", "data-raw", "contact_matrices_152_countries"), pattern = "all_locations", full.names = T)

# Loop, load in
full_list <- sapply(contact_matrix_files, function(filename){

  sheets <- readxl::excel_sheets(filename)
  #Replace some names in the Prem et al., (2017) matricies that have different spellings, or have changed, over time

  # your mapping: names = new, values = old
  new_old_names <- c(
    "Bolivia (Plurinational State of)" = "Bolivia (Plurinational State of",
    "China, Hong Kong SAR" = "Hong Kong SAR, China",
    "Czechia" = "Czech Republic",
    "Lao People's Democratic Republic" = "Lao People's Democratic Republi",
    "Sao Tome and Principe" = "Sao Tome and Principe ",
    "China, Taiwan Province of China" = "Taiwan",
    "North Macedonia" = "TFYR of Macedonia",
    "Türkiye" = "Turkey",
    "United Kingdom" = "United Kingdom of Great Britain",
    "Venezuela (Bolivarian Republic of)" = "Venezuela (Bolivarian Republic "
  )

  # sheets is your vector of strings to clean/standardize
  # patterns = old, replacements = new
  patterns     <- unname(new_old_names)
  replacements <- names(new_old_names)

  sheets_upd <- stri_replace_all_fixed(
    sheets,
    pattern = patterns,
    replacement = replacements,
    vectorize_all = FALSE
  )

  iso_code <- as.character(sapply(sheets_upd, function(y) PREVAIL::PREVAIL_locations$iso[PREVAIL::PREVAIL_locations$location %in% y]))

  x <- lapply(sheets, function(X) as.data.frame(readxl::read_excel(filename, sheet = X)))
  names(x) <- iso_code
  x
})

contact_list <- c(full_list[[1]], full_list[[2]])


# Pre-1980 vaccination coverage -------------------------------------------

vacc_pre1980 <- import(here("inst", "extdata", "data-raw", "vaccine_coverage_pre1980.xlsx")) %>%
  janitor::clean_names()


# Vaccine and disease parameters ------------------------------------------

vaccine_params <- import(here("inst", "extdata", "data-raw", "vaccine_protection.xlsx"))
disease_params <- import(here("inst", "extdata", "data-raw", "model_parameters.xlsx"))

# Names and locations -----------------------------------------------------
PREVAIL_locations <- migration_raw_upd %>% select(location = area, iso = iso3) %>% distinct()
rds_names <- c("contact_matricies", "disease_parameters", "fertility", "full_disease_df", "migration", "mortality",                "population_all", "population_female", "routine_vaccination_data", "sia_vaccination", "vaccination_pre1980", "vaccination_schedule",     "vaccine_parameters")


# Output everything -------------------------------------------------------
write_rds(fertility_raw_upd, here("inst", "extdata", "data-processed", "fertility.rds"))
write_rds(femalepop_raw_upd, here("inst", "extdata", "data-processed", "UN_WPP_population_female.rds"))
write_rds(allpop_raw_upd, here("inst", "extdata", "data-processed", "UN_WPP_population_total.rds"))
write_rds(mortality_raw_upd, here("inst", "extdata", "data-processed", "UN_WPP_mortality.rds"))
write_rds(migration_raw_upd, here("inst", "extdata", "data-processed", "UN_WPP_migration.rds"))
write_rds(cases_upd, here("inst", "extdata", "data-processed", "WHO_disease_reports.rds"))
write_rds(routine_upd, here("inst", "extdata", "data-processed", "WHO_vaccination_routine.rds"))
write_rds(schedule_upd, here("inst", "extdata", "data-processed", "WHO_vaccination_schedule.rds"))
write_rds(contact_list, here("inst", "extdata", "data-processed", "contact_matricies.rds"))
write_rds(vacc_pre1980, here("inst", "extdata", "data-processed", "vaccination_pre1980.rds"))
write_rds(PREVAIL_locations, here("inst", "extdata", "data-processed", "PREVAIL_locations.rds"))
write_rds(rds_names, here("inst", "extdata", "data-processed", "rds_names.rds"))
write_rds(vaccine_params, here("inst", "extdata", "data-processed", "vaccine_parameters"))
write_rds(disease_params, here("inst", "extdata", "data-processed", "disease_paramters.rds"))




















