reformat_demographic_data <- function(
    custom_data,
    age_required = NA,
    iso = "custom",
    value_allocation = c("maintain", "split")
) {
  value_allocation <- match.arg(value_allocation)
  data_cols <- names(custom_data)
  # Check required columns
  if (!all(c("year", "value") %in% data_cols)) {
    stop("Input must contain columns 'year' and 'value'.")
  }
  # Warn if unexpected columns are present
  allowed_cols <- c("year", "value", "age", "area", "iso3")
  extra_cols <- setdiff(data_cols, allowed_cols)
  if (length(extra_cols) > 0) {
    warning(sprintf("Input contains unexpected columns: %s", paste(extra_cols, collapse = ", ")))
  }
  # Add area and iso3 columns if missing
  if (!"area" %in% data_cols)  custom_data$area  <- iso
  if (!"iso3" %in% data_cols)  custom_data$iso3  <- iso
  # Ensure area/iso3 are the first columns if possible
  col_order <- c("area", "iso3", setdiff(names(custom_data), c("area", "iso3")))
  custom_data <- custom_data[, col_order]
  # -------- Age expansion logic --------
  if (!all(is.na(age_required))) {
    if (!"age" %in% names(custom_data)) {
      # Expand: every row for every age_required
      custom_data <- custom_data[rep(seq_len(nrow(custom_data)), each = length(age_required)), ]
      custom_data$age <- rep(age_required, times = nrow(custom_data) / length(age_required))
      custom_data <- custom_data[, c(col_order[1:2], "year", "age", "value")]
    } else {
      # Assign value only for years/ages covered by group, otherwise NA
      get_age_range <- function(a) {
        a <- as.character(a)
        if (grepl("^[0-9]+$", a)) {          # single age
          as.numeric(a)
        } else if (grepl("^[0-9]+[ ]*-[ ]*[0-9]+$", a)) { # range (e.g. 15-29)
          parts <- as.numeric(strsplit(a, "-")[[1]])
          seq(parts[1], parts[2])
        } else if (grepl("^[0-9]+[ ]*\\+", a)) { # open-ended (e.g. 80+)
          lo <- as.numeric(sub("\\+", "", a))
          lo:200
        } else {
          NA
        }
      }
      # For each row in the original data, expand only ages within group
      expanded <- lapply(seq_len(nrow(custom_data)), function(i) {
        row <- custom_data[i, ]
        ages_in_group <- intersect(age_required, get_age_range(row$age))
        n_ages <- length(ages_in_group)
        if (n_ages == 0) {
          return(NULL)
        }
        value_out <- switch(
          value_allocation,
          maintain = row$value,
          split    = row$value / n_ages
        )
        data.frame(
          area = row$area,
          iso3 = row$iso3,
          year = row$year,
          age  = ages_in_group,
          value = value_out
        )
      })
      # Fill missing (year, age) with NA
      expanded <- dplyr::bind_rows(expanded)
      # Full grid for NA fill
      grid <- expand.grid(
        area = unique(custom_data$area),
        iso3 = unique(custom_data$iso3),
        year = unique(custom_data$year),
        age  = age_required
      )
      custom_data <- dplyr::left_join(grid, expanded, by = c("area", "iso3", "year", "age"))
      custom_data <- custom_data[, c("area", "iso3", "year", "age", "value")]
    }
  }
  # Main structure logic
  if (setequal(setdiff(names(custom_data), c("area", "iso3")), c("year", "value"))) {
    return(custom_data)
  } else if ("age" %in% names(custom_data)) {
    wide_data <- custom_data %>%
      tidyr::pivot_wider(names_from = age, values_from = value, names_prefix = "x")
    if (!"area" %in% names(wide_data)) wide_data$area <- iso
    if (!"iso3" %in% names(wide_data)) wide_data$iso3 <- iso
    wide_data <- wide_data[, c("area", "iso3", setdiff(names(wide_data), c("area", "iso3")))]
    return(wide_data)
  } else {
    stop("Input must have columns 'year', 'age', and 'value'.")
  }
}
