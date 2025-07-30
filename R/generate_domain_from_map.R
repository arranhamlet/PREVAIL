#' Generate a Valid Parameter Domain Around MAP Estimates
#'
#' Creates a 2-row matrix defining the domain for each parameter, centered on MAP values,
#' with optional margin, bounds, and a minimum enforced width.
#'
#' @param map_vals Named numeric vector of MAP estimates (e.g. from MCMC).
#' @param margin Numeric. Symmetric buffer to expand around each parameter (default = 0.25).
#' @param min_width Numeric. Minimum allowable width per parameter domain (default = 0.1).
#' @param lower_bound Numeric. Minimum allowed value for any parameter (default = 0).
#' @param upper_bound Numeric. Maximum allowed value for any parameter (default = 2).
#'
#' @return A 2-row matrix with named columns. Row 1 = lower bounds, Row 2 = upper bounds.
#' @export
generate_domain_from_map <- function(map_vals,
                                     margin = 0.25,
                                     min_width = 0.1,
                                     lower_bound = 0,
                                     upper_bound = 2) {
  # Validate input
  base::stopifnot(base::is.numeric(map_vals), !base::is.null(base::names(map_vals)))

  # Compute initial domain using margin
  domain_lower <- base::pmax(map_vals - margin, lower_bound)
  domain_upper <- base::pmin(map_vals + margin, upper_bound)

  # Check if width is too narrow
  domain_width <- domain_upper - domain_lower
  too_narrow <- domain_width < min_width

  # Expand narrow domains symmetrically around MAP
  domain_lower[too_narrow] <- base::pmax(map_vals[too_narrow] - min_width / 2, lower_bound)
  domain_upper[too_narrow] <- base::pmin(map_vals[too_narrow] + min_width / 2, upper_bound)

  # Format output as a 2-row matrix
  domain_matrix <- t(base::rbind(domain_lower, domain_upper))
  base::rownames(domain_matrix) <- base::names(map_vals)
  base::colnames(domain_matrix) <- c("lower", "upper")

  return(domain_matrix)
}
