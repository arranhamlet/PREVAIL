#' Plot Model Seropositivity: Baseline vs Fitted vs Data
#'
#' Simulates the model using provided parameters and compares baseline and fitted outputs
#' for seropositivity against observed data. Returns a faceted ggplot by age group.
#'
#' @param param_list A named list of model parameters, as produced by
#'   \code{\link[PREVAIL]{custom_data_process_wrapper}}.
#' @param compare_data A data.frame of serological observations with columns:
#'   \code{time} (numeric) and \code{serosurvey} (list-column of numeric vectors by age).
#'   Default is \code{serodata}.
#' @param n_particles Number of particles to use in the dust system (default uses `n_particles` from calling environment).
#' @param n_groups Number of particle groups for simulation (default 2: baseline and fitted).
#'
#' @return A \code{ggplot2} object showing seropositivity over time by age group,
#'   with model baseline, fitted, and observed data overlaid.
#'
#' @importFrom dplyr %>% mutate filter bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_bw facet_wrap scale_x_continuous scale_y_continuous
#' @importFrom scales pretty_breaks
#' @export
plot_baseline_and_fitted_sero <- function(param_list, compare_data = serodata, n_particles = n_particles, n_groups = 2) {

  # Create and simulate model
  sys <- dust2::dust_system_create(transmission_model(), param_list,
                                   n_particles = n_particles, n_groups = n_groups)
  dust2::dust_system_set_state_initial(sys)
  time <- 0:max(compare_data$time)
  y <- dust2::dust_system_simulate(sys, time)
  all_states <- dust2::dust_unpack_state(sys, y)

  # Extract modelled seropositivity: base and fitted trajectories
  sero_obs_df <- do.call(rbind, sapply(seq_along(compare_data$serosurvey), function(a) {
    not_NA <- which(!is.na(compare_data$serosurvey[[a]]))
    do.call(rbind, sapply(not_NA, function(b) {
      data.frame(
        age = not_NA,
        time = seq_along(all_states$seropositive[not_NA, 1, ]),
        base = all_states$seropositive[not_NA, 1, ],
        fitted = all_states$seropositive[not_NA, 2, ]
      )
    }, simplify = FALSE))
  }, simplify = FALSE)) %>%
    pivot_longer(-c(time, age), names_to = "source", values_to = "value")

  # Format observed serosurvey data
  sero_true_df <- do.call(rbind, sapply(seq_len(nrow(compare_data)), function(x) {
    data.frame(
      time = compare_data$time[x],
      value = compare_data$serosurvey[[x]],
      source = "data"
    ) %>% mutate(age = seq_along(value))
  }, simplify = FALSE)) %>%
    filter(!is.na(value))

  # Combine and structure for plotting
  sero_obs_df$type <- "model"
  sero_plot_df <- bind_rows(sero_obs_df, sero_true_df) %>%
    mutate(source = factor(source, levels = c("base", "fitted", "data"))) %>%
    filter(time >= 100)

  # Create ggplot
  ggplot(sero_plot_df, aes(x = time, y = value, color = source)) +
    geom_line(data = filter(sero_plot_df, source %in% c("base", "fitted"))) +
    geom_point(data = filter(sero_plot_df, source == "data"),
               shape = 21, fill = "white", size = 2.5, stroke = 1) +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(breaks = pretty_breaks()) +
    labs(title = "Seropositivity Over Time",
         y = "Seropositive", x = "Time", color = "Data type", linetype = "Age") +
    theme_bw() +
    facet_wrap(~paste("Age: ", age), scales = "free_y")
}
