#' Assign World Bank Income Group from ISO3 Country Codes
#'
#' Maps ISO3 country codes to World Bank income group:
#' \itemize{
#'   \item \strong{HIC}: High-Income Countries
#'   \item \strong{UMIC}: Upper-Middle-Income Countries
#'   \item \strong{LMIC}: Lower-Middle-Income Countries
#'   \item \strong{LIC}: Low-Income Countries
#' }
#'
#' Returns an ordered factor with levels \code{c("HIC", "UMIC", "LMIC", "LIC")}.
#' If a code is not found, returns \code{NA} and issues a warning.
#'
#' @param iso3cs Character vector of ISO3 country codes.
#'
#' @return Ordered factor of income group, with levels \code{c("HIC", "UMIC", "LMIC", "LIC")}.
#'
#' @examples
#' get_income_group(c("KEN", "FRA", "IND", "XXX"))
#'
#' @export
get_income_group <- function (iso3cs)
{
  LIC <- c("AFG", "GNB", "SOM", "BFA", "PRK", "SSD", "BDI",
           "LBR", "SDN", "CAF", "MDG", "SYR", "TCD", "MWI", "TGO",
           "COD", "MLI", "UGA", "ERI", "MOZ", "YEM", "ETH", "NER",
           "GMB", "RWA", "GIN", "SLE")
  LMIC <- c("AGO", "HND", "PHL", "DZA", "IND", "WSM", "BGD",
            "IDN", "STP", "BLZ", "IRN", "SEN", "BEN", "KEN", "SLB",
            "BTN", "KIR", "LKA", "BOL", "KGZ", "TZA", "CPV", "LAO",
            "TJK", "KHM", "LSO", "TLS", "CMR", "MRT", "TUN", "COM",
            "FSM", "UKR", "COG", "MNG", "UZB", "CIV", "MAR", "VUT",
            "DJI", "MMR", "VNM", "EGY", "NPL", "PSE", "SLV", "NIC",
            "ZMB", "SWZ", "NGA", "ZWE", "GHA", "PAK", "HTI", "PNG")
  UMIC <- c("ALB", "GAB", "NAM", "ASM", "GEO", "MKD", "ARG",
            "GRD", "PAN", "ARM", "GTM", "PRY", "AZE", "GUY", "PER",
            "BLR", "IRQ", "ROU", "BIH", "JAM", "RUS", "BWA", "JOR",
            "SRB", "BRA", "KAZ", "ZAF", "BGR", "LCA", "CHN", "LBN",
            "VCT", "COL", "LBY", "SUR", "CRI", "MYS", "THA", "CUB",
            "MDV", "TON", "DMA", "MHL", "TUR", "DOM", "MUS", "TKM",
            "GNQ", "MEX", "TUV", "ECU", "MDA", "FJI", "MNE", "VEN")
  HIC <- c("AND", "GRC", "POL", "ATG", "GRL", "PRT", "ABW",
           "GUM", "PRI", "AUS", "HKG", "QAT", "AUT", "HUN", "SMR",
           "BHS", "ISL", "SAU", "BHR", "IRL", "SYC", "BRB", "IMN",
           "SGP", "BEL", "ISR", "SXM", "BMU", "ITA", "SVK", "VGB",
           "JPN", "SVN", "BRN", "KOR", "ESP", "CAN", "KWT", "KNA",
           "CYM", "LVA", "LIE", "SWE", "CHL", "LTU", "CHE", "HRV",
           "LUX", "TWN", "CUW", "MAC", "TTO", "CYP", "MLT", "TCA",
           "CZE", "MCO", "ARE", "DNK", "NRU", "GBR", "EST", "NLD",
           "USA", "FRO", "NCL", "URY", "FIN", "NZL", "VIR", "FRA",
           "MNP", "PYF", "NOR", "DEU", "OMN", "GIB", "PLW", "GUF")
  output <- factor(dplyr::case_when(iso3cs %in% LIC ~ 4, iso3cs %in%
                                      LMIC ~ 3, iso3cs %in% UMIC ~ 2, iso3cs %in% HIC ~ 1,
                                    TRUE ~ as.numeric(NA)), levels = seq(4), labels = c("HIC",
                                                                                        "UMIC", "LMIC", "LIC"), ordered = T)
  if (any(is.na(output))) {
    warning(paste0("Unable to get assign the following iso3cs an income group: ",
                   paste0(iso3cs[is.na(output)], collapse = ", ")))
  }
  return(output)
}
