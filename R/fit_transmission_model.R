#' Fit Transmission Model Using PMCMC
#'
#' Runs a particle MCMC fit of the PREVAIL transmission model using pre-generated parameters and a serosurvey dataset.
#' This function simulates two models (baseline and fitted) using the \pkg{dust2} engine, and fits them using the
#' \pkg{monty} framework. Parameter input should be generated externally using
#' \code{\link[PREVAIL]{custom_data_process_wrapper}}.
#'
#' @param parameters A named list of model parameters, as produced by \code{\link[PREVAIL]{custom_data_process_wrapper}}.
#' @param serodata A data.frame with columns \code{time} and \code{serosurvey}, where \code{serosurvey} is a list-column
#'   containing numeric vectors of observed seropositivity by age.
#' @param domain A 2-row matrix specifying the lower and upper bounds for each parameter. Default is \code{matrix(c(0, 2, 0, 2), nrow = 2, byrow = TRUE)}.
#' @param vcv A variance-covariance matrix for the proposal distribution used in the random walk sampler. Default: \code{diag(c(0.1, 0.1))}.
#' @param n_steps Integer. Number of MCMC steps to perform (default: 1000).
#' @param n_chains Integer. Number of independent MCMC chains to run (default: 1).
#' @param n_particles Integer. Number of particles to use in the particle filter and simulation (default: 1).
#'
#' @return A named list with the following components:
#' \describe{
#'   \item{samples}{Output of \code{\link[monty]{monty_sample}}, including sampled parameters and log-likelihoods.}
#'   \item{posterior}{The constructed posterior object (prior + likelihood) used in the fit.}
#'   \item{all_states}{Full simulation output unpacked from \code{\link[dust2]{dust_unpack_state}}.}
#'   \item{plots}{List of \code{\link[ggplot2]{ggplot}} plots: \code{trace}, \code{loglik}, \code{combined}, and \code{sero}.}
#' }
#'
#' @details
#' The function initializes two model trajectories: one with the default parameters and one with the MAP (maximum a posteriori) values.
#' Serosurvey observations are matched by time and plotted alongside model predictions for visual validation.
#'
#' @importFrom monty monty_dsl monty_packer monty_sampler_random_walk monty_sample
#' @importFrom dust2 dust_unfilter_create dust_system_create dust_system_simulate dust_unpack_state
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs scale_x_continuous scale_y_continuous theme_bw theme element_blank element_text
#' @importFrom scales pretty_breaks comma
#' @importFrom patchwork plot_layout
#' @importFrom dplyr filter mutate rename bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#' params <- PREVAIL::custom_data_process_wrapper(iso = "KEN", disease = "measles", R0 = 15)
#' serodata <- data.frame(time = 2500, serosurvey = I(list(rep(0.85, 17))))
#' fit <- fit_transmission_model(parameters = params, serodata = serodata, n_steps = 500)
#' fit$plots$combined
#' fit$plots$sero
#' }
#'
#' @export
fit_transmission_model <- function(
    parameters,
    serodata,
    domain = matrix(c(0, 2, 0, 2), nrow = 2, byrow = TRUE),
    vcv = diag(c(0.1, 0.1)),
    n_steps = 1000,
    n_chains = 1,
    n_particles = 1
) {

  filter <- dust2::dust_unfilter_create(
    generator = transmission_model(),
    time_start = 0,
    data = serodata,
    n_particles = n_particles
  )

  prior <- monty::monty_dsl({
    reporting_rate ~ Normal(mean = 1, sd = 5)
    R0_modifier ~ Normal(mean = 15, sd = 5)
  })

  sir_packer <- monty::monty_packer(c("reporting_rate", "R0_modifier"), fixed = parameters)
  likelihood <- dust2::dust_likelihood_monty(filter, sir_packer)

  posterior <- prior + likelihood
  posterior$domain[1, ] <- domain[1, ]
  posterior$domain[2, ] <- domain[2, ]

  sampler <- monty::monty_sampler_random_walk(vcv = vcv)
  initial <- sir_packer$pack(list(reporting_rate = 1, R0_modifier = 1))
  samples <- monty::monty_sample(posterior, sampler, n_steps = n_steps, initial = initial, n_chains = n_chains)

  density_df <- tibble::tibble(
    iteration = seq_len(n_steps),
    reporting_rate = drop(samples$pars[1, , 1]),
    R0_modifier = drop(samples$pars[2, , 1]),
    loglik = drop(samples$density[, 1])
  ) %>%
    tidyr::pivot_longer(-iteration, names_to = "parameter", values_to = "value")

  p_trace <- ggplot2::ggplot(density_df %>% dplyr::filter(parameter != "loglik"),
                             ggplot2::aes(x = iteration, y = value, color = parameter)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(x = "Iteration", y = "Parameter Value", title = "Trace Plot", color = "Parameter") +
    ggplot2::theme_bw()

  p_loglik <- ggplot2::ggplot(density_df %>% dplyr::filter(parameter == "loglik"),
                              ggplot2::aes(x = iteration, y = value)) +
    ggplot2::geom_line(color = "black") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::comma) +
    ggplot2::labs(x = "Iteration", y = "Log Likelihood", title = "Log Likelihood Trace") +
    ggplot2::theme_bw()

  combined_plot <- (p_trace + ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                                             axis.text.x = ggplot2::element_blank())) /
    p_loglik +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "right")

  best_idx <- which.max(samples$density[, 1])
  parameters_alt <- parameters
  parameters_alt$reporting_rate <- samples$pars[1, best_idx, 1]
  parameters_alt$R0_modifier <- samples$pars[2, best_idx, 1]

  pars2 <- list(parameters, parameters_alt)
  sys <- dust2::dust_system_create(transmission_model(), pars2, n_particles = n_particles, n_groups = 2)
  dust2::dust_system_set_state_initial(sys)
  time <- 0:max(serodata$time)
  y <- dust2::dust_system_simulate(sys, time)
  all_states <- dust2::dust_unpack_state(sys, y)

   # Format model output as long data
  sero_obs_df <- tibble::tibble(
    time = seq_along(all_states$seropositive[4, 1, ]),
    base = all_states$seropositive[4, 1, ],
    fitted = all_states$seropositive[4, 2, ]
  ) %>%
    tidyr::pivot_longer(cols = -time, names_to = "source", values_to = "value")

  # Extract true observed data (assuming one row in serodata)
  sero_true_df <- tibble::tibble(
    time = serodata$time[1],
    value = unlist(serodata$serosurvey[[1]]),
    type = "data"
  )

  # Add type = "model" to model output
  sero_obs_df$type <- "model"

  # Combine for plotting
  sero_plot_df <- dplyr::bind_rows(
    sero_obs_df %>% dplyr::rename(source = source),
    sero_true_df %>% dplyr::rename(source = type)
  ) %>%
    mutate(source = factor(source, levels = c("base", "fitted", "data")))

  p_sero <- ggplot2::ggplot(sero_plot_df, ggplot2::aes(x = time, y = value, color = source)) +
    ggplot2::geom_line(data = dplyr::filter(sero_plot_df, source %in% c("base", "fitted"))) +
    ggplot2::geom_point(data = dplyr::filter(sero_plot_df, source == "data"),
                        shape = 21, fill = "white", size = 2.5, stroke = 1) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(title = "Seropositivity Over Time",
                  y = "Seropositive", x = "Time", color = "Legend") +
    ggplot2::theme_bw()


  list(
    samples = samples,
    posterior = posterior,
    all_states = all_states,
    plots = list(trace = p_trace, loglik = p_loglik,
                 combined = combined_plot, sero = p_sero)
  )
}
