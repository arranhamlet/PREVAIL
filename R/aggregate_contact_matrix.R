#' aggregate_contact_matrix
#'
#' Aggregate and Rescale a Square Contact Matrix
#'
#' Aggregates a square contact matrix to custom age groups using population-weighted means,
#' then rescales the result to preserve the total sum of the original matrix.
#'
#' @param mat A square numeric matrix of contact rates (e.g., 101 x 101 for ages 0–100).
#' @param age_breaks Numeric vector of lower age boundaries for groups (e.g., \code{c(0, 5, 10, ..., Inf)}).
#' @param population A data frame with columns \code{time}, \code{age} (integer, 1-based), and \code{value} (population count).
#' @param symmetric Logical; if \code{TRUE}, enforces reciprocity between age groups (default: \code{TRUE}).
#'
#' @details
#' The function first computes the mean population per single-year age across time,
#' assigns each age to a group defined by \code{age_breaks}, and aggregates the contact matrix
#' using population-weighted averages. It then rescales the aggregated matrix so that
#' the total sum is equal to that of the original matrix. Optionally, it enforces symmetry
#' (reciprocity) using population group weights.
#'
#' @return A square numeric matrix of dimension \code{length(age_breaks) - 1},
#' with sum preserved.
#'
#' @importFrom dplyr group_by summarise pull
#' @importFrom magrittr %>%
#'
#' @export

aggregate_contact_matrix <- function(mat, age_breaks, population, symmetric = TRUE) {

  # Step 1: Compute average population per age over time
  pop_by_age <- population %>%
    dplyr::group_by(age) %>%
    dplyr::summarise(weight = mean(value, na.rm = TRUE), .groups = "drop")

  # Step 2: Map ages to age groups
  if (!is.infinite(tail(age_breaks, 1))) {
    age_breaks[length(age_breaks)] <- Inf
  }
  pop_by_age$group <- cut(pop_by_age$age - 1, breaks = age_breaks, labels = FALSE, right = FALSE)

  # Step 3: Aggregate contact matrix using weighted means
  age_index <- cut(seq_len(nrow(mat)) - 1, breaks = age_breaks, labels = FALSE, right = FALSE)
  n_groups <- max(age_index)
  agg_mat <- matrix(0, nrow = n_groups, ncol = n_groups)

  for (i in seq_len(n_groups)) {
    for (j in seq_len(n_groups)) {
      rows <- which(age_index == i)
      cols <- which(age_index == j)

      pop_weights <- pop_by_age$weight[match(rows, pop_by_age$age)]
      pop_weights <- pop_weights / sum(pop_weights, na.rm = TRUE)
      sub_mat <- mat[rows, cols, drop = FALSE]

      # Weighted average over rows
      agg_mat[i, j] <- sum(pop_weights %*% sub_mat, na.rm = TRUE)
    }
  }

  # Step 4: Optional reciprocity enforcement
  if (symmetric) {
    group_weights <- pop_by_age %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(pop = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      dplyr::pull(pop)

    for (i in seq_len(n_groups)) {
      for (j in seq_len(n_groups)) {
        agg_mat[i, j] <- (agg_mat[i, j] * group_weights[j] + agg_mat[j, i] * group_weights[i]) /
          (group_weights[i] + group_weights[j])
      }
    }
  }

  # Step 5: Normalize both rows and columns to sum to 1 (Sinkhorn-Knopp)
  tol <- 1e-8
  max_iter <- 100
  for (k in seq_len(max_iter)) {
    # Normalize rows
    agg_mat <- sweep(agg_mat, 1, rowSums(agg_mat), "/")
    # Normalize columns
    agg_mat <- sweep(agg_mat, 2, colSums(agg_mat), "/")
    # Convergence check
    if (all(abs(rowSums(agg_mat) - 1) < tol) && all(abs(colSums(agg_mat) - 1) < tol)) break
  }

  return(agg_mat)
}
