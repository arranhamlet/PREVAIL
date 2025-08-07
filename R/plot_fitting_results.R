#' Plot PMCMC Fitting Results: Parameter Traces and Log-Likelihood
#'
#' Produces a two-panel plot showing the trace of fitted parameters and the log-likelihood
#' over PMCMC iterations. Designed for diagnostics of convergence and mixing.
#'
#' @param long_trace A data.frame in long format with columns \code{iteration}, \code{parameter},
#'   and \code{value}, typically output from a PREVAIL PMCMC run.
#'
#' @return A combined \code{ggplot2} object (via \code{patchwork}) showing:
#' \itemize{
#'   \item Parameter trace plot for all fitted parameters
#'   \item Log-likelihood trace plot
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme_bw element_blank element_text
#' @importFrom dplyr filter
#' @importFrom scales pretty_breaks comma
#' @importFrom patchwork plot_layout
#' @export
plot_fitting_results <- function(long_trace) {

  # Plot parameter traces (excluding log-likelihood)
  p_trace <- ggplot2::ggplot(
    dplyr::filter(long_trace, parameter != "loglik"),
    ggplot2::aes(x = iteration, y = value, color = parameter)
  ) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(
      x = "Iteration", y = "Parameter Value",
      title = "Trace Plot", color = "Parameter"
    ) +
    ggplot2::theme_bw()

  # Plot log-likelihood trace
  p_loglik <- ggplot2::ggplot(
    dplyr::filter(long_trace, parameter == "loglik"),
    ggplot2::aes(x = iteration, y = value)
  ) +
    ggplot2::geom_line(color = "black") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::comma) +
    ggplot2::labs(
      x = "Iteration", y = "Log Likelihood",
      title = "Log Likelihood Trace"
    ) +
    ggplot2::theme_bw()

  # Combine plots with patchwork
  combined_plot <- (
    p_trace +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank()
      )
  ) / p_loglik +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "right")

  combined_plot
}
