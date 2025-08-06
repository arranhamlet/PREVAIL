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

#Filter to Singapore Measles data - remove sample that only included migrant workers
sing_mea <- sero_data %>%
  filter(iso3 == "SGP", disease_s == "Measles", !grepl("Migrant", country))

#Generate serological data in the correct format
these_age_breaks <- generate_age_breaks(sing_mea)

sero_sing <- make_serodata(data = sing_mea,
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
parameters_to_fit <- c("vaccination_modifier", "R0_modifier")
domain <- matrix(c(.75, 1.25, 0, 2), nrow = 2, byrow = TRUE)

prior1 <- monty::monty_dsl({
  vaccination_modifier ~ Normal(mean = 1, sd = .1)
  R0_modifier ~ Normal(mean = 1, sd = .1)
})

fit1 <- fit_transmission_model(
  parameters = params,
  serodata = sero_sing,
  prior = prior1,
  fitted_parameters = parameters_to_fit,
  domain = domain,
  vcv = diag(c(.1, .1)),
  n_steps = 500,
  n_particles = 1
)

print(fit1$plots$combined)
print(fit1$plots$sero)

samples_mat <- t(drop(fit1$samples$pars[, , 1]))
colnames(samples_mat) <- c("reporting_rate", "R0_modifier")

map_vals <- samples_mat[which.max(fit1$samples$density[, 1]), ]

param_sds <- apply(samples_mat, 2, stats::sd, na.rm = TRUE)
param_sds[is.na(param_sds) | param_sds == 0] <- 0.1

domain2 <- generate_domain_from_map(map_vals, margin = 0.25)

sd_from_domain <- apply(domain2, 1, function(x) diff(x) / 2)

prior2 <- monty::monty_dsl({
  reporting_rate ~ Normal(mean = !!map_vals["reporting_rate"], sd = !!sd_from_domain["reporting_rate"])
  R0_modifier ~ Normal(mean = !!map_vals["R0_modifier"], sd = !!sd_from_domain["R0_modifier"])
})

vcv2 <- diag(param_sds^4)

fit2 <- fit_transmission_model(
  parameters = params,
  serodata = sero_sing,
  prior = prior2,
  initial = as.list(map_vals),
  domain = domain2,
  vcv = vcv2,
  n_steps = 500,
  n_particles = 1
)

combine_ggplot(fit1$plots$sero,
               fit2$plots$sero)



