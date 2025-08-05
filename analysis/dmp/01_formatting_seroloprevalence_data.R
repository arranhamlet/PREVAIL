if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rio,
  here,
  tidyverse
)

#Import data
sero_raw <- import(here("analysis", "dmp", "data-raw", "seroprevalence_review.xlsx"))

# Helper function to parse min/max ages
parse_age_range <- function(text) {
  text <- tolower(text)

  if (str_detect(text, "all ages")) {
    return(c(0, 100))
  }

  text %>%
    str_replace_all("–", "-") %>%
    str_replace_all("months?|mo", "mo") %>%
    str_replace_all("years?|yrs?|y", "yr") %>%
    str_replace_all(">", "gt_") %>%
    str_replace_all("≥", "gte_") %>%
    str_replace_all("<", "lt_") %>%
    str_extract_all("gte_\\d+|gt_\\d+|lt_\\d+|\\d+\\s*mo|\\d+") %>%
    unlist() %>%
    {
      if (length(.) == 0) {
        c(NA, NA)
      } else if (any(str_detect(., "lt_"))) {
        c(0, as.numeric(str_remove(.[str_detect(., "lt_")], "lt_")))
      } else if (any(str_detect(., "gte_|gt_"))) {
        val <- as.numeric(str_remove(.[str_detect(., "gte_|gt_")], "gte_|gt_"))
        c(val, 100)  # cap at 100 instead of Inf
      } else if (length(.) == 1) {
        c(as.numeric(str_remove(., "gte_|gt_|lt_")), NA)
      } else {
        . <- str_remove_all(., "mo")
        . <- as.numeric(.)
        .[is.na(.)] <- 0
        if (any(str_detect(text, "mo"))) . <- . / 12
        range(.)
      }
    }
}

parse_seroprev <- function(x) {
  # Extract all numeric values (last one is usually seroprev)
  all_vals <- str_extract_all(x, "\\d+(\\.\\d+)?(e[-+]?\\d+)?")[[1]]

  if (length(all_vals) == 0) {
    return(tibble(seroprev_raw_value = NA, seroprev_unit = NA, seroprev_prop = NA_real_))
  }

  # Use last value, or midpoint if it's a range
  if (str_detect(x, "\\d+\\s*–\\s*\\d+")) {
    nums <- str_match(x, "(\\d+(\\.\\d+)?)\\s*–\\s*(\\d+(\\.\\d+)?)")[, c(2, 4)] %>% as.numeric()
    val <- mean(nums)
  } else {
    val <- as.numeric(tail(all_vals, 1))
  }

  # Convert to proportion if needed
  unit <- if (val > 1) "%" else "proportion"
  prop <- if (unit == "%") val / 100 else val

  # Invert if text implies negative
  if (str_detect(x, regex("lacked|seronegative|non-immune|susceptible", ignore_case = TRUE))) {
    prop <- 1 - prop
  }

  tibble(
    seroprev_raw_value = val,
    seroprev_unit = unit,
    seroprev_prop = prop
  )
}


#Full pipeline
sero_clean <- sero_raw %>%
  janitor::clean_names() %>%
  select(
    iso3, country, location_scope, year,
    age_group = age_group_s, sample_size, disease_s, overall_seroprev
  ) %>%
  mutate(
    parsed_age = map(age_group, parse_age_range),
    min_age = map_dbl(parsed_age, 1),
    max_age = map_dbl(parsed_age, 2)
  ) %>%
  select(-parsed_age) %>%
  separate_rows(overall_seroprev, sep = ";") %>%
  bind_cols(map_dfr(.$overall_seroprev, parse_seroprev))


#Export
sero_export <-  sero_clean %>%
  select(-c(age_group, overall_seroprev, seroprev_raw_value, seroprev_unit))

export(sero_export, here("analysis", "dmp", "data-derived", "seroprevalence_clean.csv"))


