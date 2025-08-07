if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  PREVAIL,
  dust2,
  monty,
  here,
  rio,
  tidyverse
)

#Serology data
sero_data <- import(here("analysis", "dmp", "data-derived", "seroprevalence_clean.csv"))

#Helper function for formatting the data
make_serodata <- function(data, age_breaks = 0:100) {

  age_groups <- data.frame(
    group = seq_along(age_breaks)[-length(age_breaks)],
    age_min = age_breaks[-length(age_breaks)],
    age_max = age_breaks[-1] - 1
  )

  data %>%
    mutate(time = (year - 1950) * 365) %>%
    rowwise() %>%
    mutate(
      serosurvey = list({
        # start with NA vector
        out <- rep(NA_real_, nrow(age_groups))
        for (i in seq_len(nrow(age_groups))) {
          if (min_age <= age_groups$age_max[i] && max_age >= age_groups$age_min[i]) {
            out[i] <- seroprev_prop
          }
        }
        out
      })
    ) %>%
    ungroup() %>%
    mutate(serosurvey = I(serosurvey)) %>%
    select(time, serosurvey)
}

#Set up a function to generate our age breaks from the data
generate_age_breaks <- function(df, global_min = 0, global_max = 100) {
  # 1. Ensure coverage of each serosurvey range
  core_breaks <- df %>%
    mutate(max_age = pmin(max_age, global_max)) %>%
    transmute(breaks = list(c(min_age, max_age + 1))) %>%
    pull(breaks) %>%
    unlist()

  # 2. Add start/end bounds
  all_breaks <- c(global_min, core_breaks, global_max + 1)

  # 3. Deduplicate and sort
  sort(unique(all_breaks))
}

#Filter to a countryle countries measles data - remove sample that only included migrant workers
country_mea <- sero_data %>%
  filter(iso3 == "SGP", disease_s == "Measles")

#Generate serological data in the correct format
these_age_breaks <- generate_age_breaks(country_mea)

sero_country <- make_serodata(data = country_mea,
                           age_breaks = these_age_breaks)

#Generate parameters
params <- PREVAIL::custom_data_process_wrapper(
  iso = "SGP",
  disease = "measles",
  R0 = 15,
  aggregate_age = T,
  new_age_breaks = these_age_breaks
)

#Defining domain and parameters
parameters_to_fit <- c("vaccination_modifier", "R0_modifier", "reporting_rate")
domain <- matrix(c(0, 2, 0, 2, 0, 100), nrow = 3, byrow = TRUE)

prior <- monty::monty_dsl({
  vaccination_modifier ~ Normal(mean = 1, sd = 10)
  R0_modifier ~ Normal(mean = 1, sd = 10)
  reporting_rate ~ Normal(mean = 50, sd = 100)
})

fit <- fit_transmission_model(
  parameters = params,
  serodata = sero_country,
  prior = prior,
  fitted_parameters = parameters_to_fit,
  domain = domain,
  vcv = diag(c(.1, .1, .1)),
  n_steps = 1000,
  n_particles = 1
)

print(fit$plots$combined)
print(fit$plots$sero)






