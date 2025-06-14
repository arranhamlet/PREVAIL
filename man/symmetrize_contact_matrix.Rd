% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal_helpers.R
\name{symmetrize_contact_matrix}
\alias{symmetrize_contact_matrix}
\title{Symmetrize a Contact Matrix Using Population Weights}
\usage{
symmetrize_contact_matrix(C, pop)
}
\arguments{
\item{C}{A square numeric matrix of contact rates (e.g., POLYMOD contact matrix),
where each element \code{C[i, j]} represents the average number of daily contacts
made by an individual in age group \code{i} with individuals in age group \code{j}.}

\item{pop}{A numeric vector of population sizes for each age group. Must be the same
length as the number of rows and columns in \code{C}.}
}
\value{
A symmetric matrix of the same dimensions as \code{C}, with contact rates
  adjusted to satisfy reciprocal contact symmetry based on population sizes.
}
\description{
This function symmetrizes a square contact matrix using population sizes,
ensuring that total contacts between age groups are reciprocal.
}
\details{
The symmetrized matrix is computed using the formula:
\deqn{C^\text{sym}_{i,j} = \frac{n_i C_{i,j} + n_j C_{j,i}}{n_i + n_j}}
where \eqn{C_{i,j}} is the contact rate from age group \eqn{i} to \eqn{j},
and \eqn{n_i} and \eqn{n_j} are the population sizes of groups \eqn{i} and \eqn{j}.
}
\examples{
C <- matrix(c(10, 2, 1,
              2, 8, 2,
              1, 2, 6), nrow = 3, byrow = TRUE)
rownames(C) <- colnames(C) <- c("0-4", "5-17", "18+")
pop <- c(3000000, 7000000, 50000000)
symmetrize_contact_matrix(C, pop)

}
\keyword{internal}
