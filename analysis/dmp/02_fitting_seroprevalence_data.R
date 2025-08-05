if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  PREVAIL,
  dust2,
  monty,
  tidyverse
)

#Serology data
sero_data <- import(here("analysis", "dmp", "data-derived", "seroprevalence_clean.csv"))

#Restrict to one location - Singapore and reformat to what we want to input
sero_sing <- sero_data %>%
  filter(iso3 == "SGP", disease_s == "Measles") %>%
  mutate(
    time = (year - 1950) * 365,
    serosurvey = map2(min_age, max_age, ~{
      vec <- rep(NA_real_, 101)
      vec[.x:.y] <- seroprev_prop[cur_group_id()]
      vec
    }) %>% I()  # wrap in I() to make it a list-column
  ) %>%
  select(time, serosurvey)

#Generate parameters
params <- PREVAIL::custom_data_process_wrapper(
  iso = "SGP",
  disease = "measles",
  R0 = 15,
  aggregate_age = F
)

#Defining domain and parameters
parameters_to_fit <- c("reporting_rate", "R0_modifier")
domain <- matrix(c(0, 2, 0, 2), nrow = 2, byrow = TRUE)

prior1 <- monty::monty_dsl({
  reporting_rate ~ Normal(mean = 1, sd = 1)
  R0_modifier ~ Normal(mean = 1, sd = 1)
})

fit1 <- fit_transmission_model(
  parameters = params,
  serodata = sero_sing,
  prior = prior1,
  fitted_parameters = parameters_to_fit,
  domain = domain,
  vcv = diag(c(0.25, 0.25)),
  n_steps = 500,
  n_particles = 1
)

