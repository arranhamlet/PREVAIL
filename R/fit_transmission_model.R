#' Fit Transmission Model Using PMCMC
#'
#' Runs a particle MCMC fit of the PREVAIL transmission model using pre-generated parameters and a serosurvey dataset.
#' This function performs a PMCMC run using the \pkg{dust2} engine for likelihood evaluation and the \pkg{monty}
#' framework for inference. The function does not run simulation post-fitting — instead, it returns everything needed
#' for follow-up analysis or second-stage fitting.
#'
#' @param parameters A named list of model parameters, as produced by \code{\link[PREVAIL]{custom_data_process_wrapper}}.
#' @param serodata A data.frame with columns \code{time} and \code{serosurvey}, where \code{serosurvey} is a list-column
#'   containing numeric vectors of observed seropositivity by age.
#' @param prior A prior distribution object created using \code{\link[monty]{monty_dsl}}.
#' @param fitted_parameters Character vector of parameter names to fit (e.g., \code{c("reporting_rate", "R0_modifier")}).
#' @param initial A named list of initial parameter values, or \code{NULL} to default all to 1.
#' @param domain A matrix with two rows (lower and upper) and columns named for each fitted parameter. Defines parameter bounds.
#' @param vcv A variance-covariance matrix for the random walk proposal. Should be square with dimensions equal to number of parameters.
#' @param n_steps Integer. Number of MCMC steps to perform (default = 1000).
#' @param n_chains Integer. Number of independent MCMC chains (default = 1).
#' @param n_particles Integer. Number of particles used in the particle filter (default = 1).
#'
#' @return A named list containing:
#' \describe{
#'   \item{samples}{The output from \code{\link[monty]{monty_sample}}, including sampled parameters and log-likelihoods.}
#'   \item{posterior}{The final posterior object used for inference.}
#'   \item{summarised_draws}{Summary statistics (e.g., mean, median, quantiles) for each parameter from the posterior samples.}
#'   \item{all_states}{The full simulated state trajectories from the model for both base and fitted parameter sets.}
#'   \item{plots}{A list of \pkg{ggplot2} objects including trace plots, log-likelihood plot, combined trace-loglik plot, and seropositivity plot:
#'     \code{trace}, \code{loglik}, \code{combined}, \code{sero}.}
#' }
#'
#' @importFrom monty monty_dsl monty_packer monty_sampler_random_walk monty_sample
#' @importFrom dust2 dust_unfilter_create dust_likelihood_monty
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs scale_x_continuous scale_y_continuous theme_bw theme element_blank element_text
#' @importFrom scales pretty_breaks comma
#' @importFrom patchwork plot_layout
#' @importFrom dplyr filter mutate rename bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom posterior summarise_draws as_draws_df
#'
#' @examples
#' \dontrun{
#' params <- PREVAIL::custom_data_process_wrapper(iso = "KEN", disease = "measles", R0 = 15)
#' serodata <- data.frame(time = 2500, serosurvey = I(list(rep(0.85, 17))))
#' prior <- monty::monty_dsl({
#'   reporting_rate ~ Normal(mean = 1, sd = 5)
#'   R0_modifier ~ Normal(mean = 1, sd = 1)
#' })
#' fit <- fit_transmission_model(parameters = params, serodata = serodata, prior = prior)
#' fit$plots$combined
#' }
#'
#' @export
fit_transmission_model <- function(
    parameters,
    serodata,
    prior,
    fitted_parameters = c("reporting_rate", "R0_modifier"),
    initial = NULL,
    domain = matrix(c(0, 2, 0, 2), nrow = 2, byrow = T),
    vcv = diag(0.1, length(fitted_parameters)),
    n_steps = 1000,
    n_chains = 1,
    n_particles = 1
) {

  # Create particle filter
  filter <- dust2::dust_unfilter_create(
    generator = transmission_model(),
    time_start = 0,
    data = serodata,
    n_particles = n_particles
  )

  # Create parameter packer
  # Remove parameters that we want to fit if already found in the provided parameter list
  if(any(names(parameters) %in% fitted_parameters)){
    parameters <- parameters[-which(names(parameters) %in% fitted_parameters)]
  }

  sir_packer <- monty::monty_packer(fitted_parameters, fixed = parameters)
  likelihood <- dust2::dust_likelihood_monty(filter, sir_packer)

  # Reformat domain
  rownames(domain) <- fitted_parameters
  colnames(domain) <- c("lower", "upper")

  # Construct posterior
  posterior <- prior + likelihood
  posterior$domain[, 1] <- domain[, "lower"]
  posterior$domain[, 2] <- domain[, "upper"]

  # Create sampler
  sampler <- monty::monty_sampler_random_walk(vcv = vcv)

  # Default initial values if none provided
  if (is.null(initial)) {
    initial <- stats::setNames(as.list(rep(1, length(fitted_parameters))), fitted_parameters)
  }

  # Validate initial values inside domain
  initial_vec <- unlist(initial)
  domain_check <- initial_vec >= domain[fitted_parameters, "lower"] &
    initial_vec <= domain[fitted_parameters, "upper"]
  if (any(!domain_check)) {
    warning("Initial values fall outside the domain for: ",
            paste(fitted_parameters[!domain_check], collapse = ", "))
  }

  # Run sampling
  samples <- monty::monty_sample(
    posterior,
    sampler,
    n_steps = n_steps,
    initial = sir_packer$pack(initial),
    n_chains = n_chains
  )

  # Summarise draws
  sum_draws <- posterior::summarise_draws(posterior::as_draws_df(samples))

  # Format trace output
  trace_df <- data.frame(iteration = seq_len(n_steps))
  for (i in seq_along(fitted_parameters)) {
    trace_df[[fitted_parameters[i]]] <- drop(samples$pars[i, , 1])
  }
  trace_df$loglik <- drop(samples$density[, 1])

  long_trace <- trace_df %>% tidyr::pivot_longer(-iteration, names_to = "parameter", values_to = "value")

  p_trace <- ggplot2::ggplot(dplyr::filter(long_trace, parameter != "loglik"),
                             ggplot2::aes(x = iteration, y = value, color = parameter)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(x = "Iteration", y = "Parameter Value", title = "Trace Plot", color = "Parameter") +
    ggplot2::theme_bw()

  p_loglik <- ggplot2::ggplot(dplyr::filter(long_trace, parameter == "loglik"),
                              ggplot2::aes(x = iteration, y = value)) +
    ggplot2::geom_line(color = "black") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::comma) +
    ggplot2::labs(x = "Iteration", y = "Log Likelihood", title = "Log Likelihood Trace") +
    ggplot2::theme_bw()

  combined_plot <- (p_trace + ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                                             axis.text.x = ggplot2::element_blank())) /
    p_loglik + patchwork::plot_layout(guides = "collect") & ggplot2::theme(legend.position = "right")

  # MAP parameters
  best_idx <- which.max(samples$density[, 1])
  parameters_alt <- parameters
  for (i in 1:length(fitted_parameters)) {
    parameters_alt[[fitted_parameters[i]]] <- sum_draws %>% filter(variable == fitted_parameters[i]) %>% pull(median)
  }

  # Simulate model
  pars2 <- list(parameters, parameters_alt)
  sys <- dust2::dust_system_create(transmission_model(), pars2, n_particles = n_particles, n_groups = 2)
  dust2::dust_system_set_state_initial(sys)
  time <- 0:max(serodata$time)
  y <- dust2::dust_system_simulate(sys, time)
  all_states <- dust2::dust_unpack_state(sys, y)

  # Format sero output
  sero_obs_df <- do.call(rbind, sapply(1:length(serodata$serosurvey), function(a){
    not_NA <- which(!is.na(serodata$serosurvey[[a]]))
    do.call(rbind, sapply(not_NA, function(b){
      data.frame(
        age = not_NA,
        time = seq_along(all_states$seropositive[not_NA, 1, ]),
        base = all_states$seropositive[not_NA, 1, ],
        fitted = all_states$seropositive[not_NA, 2, ]
      )
    }, simplify = FALSE))
  }, simplify = FALSE)) %>%
    tidyr::pivot_longer(-c(time, age), names_to = "source", values_to = "value")

  sero_true_df <- do.call(rbind, sapply(1:nrow(serodata), function(x){
    data.frame(
      time = serodata$time[x],
      value = serodata$serosurvey[[x]],
      source = "data"
    ) %>%
      dplyr::mutate(
        age = 1:n()
      )
  }, simplify = FALSE)) %>%
    dplyr::filter(!is.na(value))

  sero_obs_df$type <- "model"
  sero_plot_df <- dplyr::bind_rows(sero_obs_df, sero_true_df) %>%
    dplyr::mutate(source = factor(source, levels = c("base", "fitted", "data"))) %>%
    dplyr::filter(time >= 100)

  p_sero <- ggplot2::ggplot(sero_plot_df, ggplot2::aes(x = time, y = value, color = source)) +
    ggplot2::geom_line(data = dplyr::filter(sero_plot_df, source %in% c("base", "fitted"))) +
    ggplot2::geom_point(data = dplyr::filter(sero_plot_df, source == "data"),
                        shape = 21, fill = "white", size = 2.5, stroke = 1) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(title = "Seropositivity Over Time",
                  y = "Seropositive", x = "Time", color = "Data type", linetype = "Age") +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~paste("Age: ", age), scales = "free_y")

  list(
    samples = samples,
    posterior = posterior,
    summarised_draws = sum_draws,
    all_states = all_states,
    plots = list(trace = p_trace, loglik = p_loglik,
                 combined = combined_plot, sero = p_sero)
  )
}
