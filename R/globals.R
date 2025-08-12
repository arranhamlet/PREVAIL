#' Declare Global Variables for Package
#'
#' Declares global variables used in the package functions to avoid R CMD check notes about "no visible binding".
#' @name prevail-globals
#' @importFrom utils globalVariables
#' @keywords internal
utils::globalVariables(c(
  "age", "age_administered", "age_group", "age_group_label", "age_years",
  "area", "cases", "coverage", "crude_death", "cum_lower", "cum_median",
  "cum_upper", "data", "death", "deaths", "dim1", "dim2", "dim3", "dim4",
  "disease", "disease_n", "disease_short", "dose", "dose_order", "events",
  "group", "guide_legend", "guides", "head", "income_group", "iso3", "label",
  "lower", "max_year", "median", "min_year", "parameter", "period", "pop",
  "population", "prop", "prop_lower", "prop_upper", "reference", "risk",
  "rowname", "run", "schedulerounds", "state", "state_plot", "status",
  "status_simple", "susceptible_pct_increase", "tail", "target_pop",
  "target_pop_description", "time", "total_median", "type", "upper",
  "vaccination", "vaccination_name", "vaccine", "vaccine_code",
  "vaccine_description", "value", "value.update", "weight", "where",
  "who_region", ".", "year", ".I", ".N",
  ".SD",
  "age_from", "age_from_expanded", "age_to", "age_to_expanded",
  "dist", "duration",
  "group_pop", "in_repro", "introduction_year", "iteration",
  "migration_rate_1000", "na.omit",
  "repro_pop", "row_id", "serodata", "variable"
))
