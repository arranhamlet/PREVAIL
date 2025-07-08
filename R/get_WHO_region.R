#' Assign WHO Region to ISO3 Country Codes
#'
#' Maps ISO3 country codes to their corresponding WHO region code:
#' \itemize{
#'   \item \strong{AFR}: Africa
#'   \item \strong{AMR}: Americas
#'   \item \strong{SEAR}: South-East Asia
#'   \item \strong{EUR}: Europe
#'   \item \strong{EMR}: Eastern Mediterranean
#'   \item \strong{WPR}: Western Pacific
#' }
#'
#' If a code is not found, returns \code{NA} and issues a warning.
#'
#' @param iso3cs Character vector of ISO3 country codes.
#'
#' @return Character vector of WHO region codes (AFR, AMR, SEAR, EUR, EMR, WPR, or NA).
#'
#' @examples
#' get_WHO_region(c("KEN", "FRA", "IND", "XYZ"))
#'
#' @export
get_WHO_region <- function (iso3cs){
  AFR <- c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV",
           "CMR", "CAF", "TCD", "COM", "COG", "CIV", "COD", "GNQ",
           "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB",
           "KEN", "LSO", "LBR", "MDG", "MWI", "MLI", "MRT", "MUS",
           "MOZ", "NAM", "NER", "NGA", "RWA", "STP", "SEN", "SYC",
           "SLE", "ZAF", "SSD", "TGO", "UGA", "TZA", "ZMB", "ZWE")
  AMR <- c("ATG", "ARG", "BHS", "BRB", "BLZ", "BOL", "BRA",
           "CAN", "CHL", "COL", "CRI", "CUB", "DMA", "DOM", "ECU",
           "SLV", "GRD", "GTM", "GUY", "HTI", "HND", "JAM", "MEX",
           "NIC", "PAN", "PRY", "PER", "KNA", "LCA", "VCT", "SUR",
           "TTO", "USA", "URY", "VEN", "GUF", "ABW", "CUW")
  SEAR <- c("BGD", "BTN", "PRK", "IND", "IDN", "MDV", "MMR",
            "NPL", "LKA", "THA", "TLS")
  EUR <- c("ALB", "AND", "ARM", "AUT", "AZE", "BLR", "BEL",
           "BIH", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN",
           "FRA", "GEO", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR",
           "ITA", "KAZ", "KGZ", "LVA", "LTU", "LUX", "MLT", "MCO",
           "MNE", "NLD", "MKD", "NOR", "POL", "PRT", "MDA", "ROU",
           "RUS", "SMR", "SRB", "SVK", "SVN", "ESP", "SWE", "CHE",
           "TJK", "TUR", "TKM", "UKR", "GBR", "UZB")
  EMR <- c("AFG", "BHR", "DJI", "EGY", "IRN", "IRQ", "JOR",
           "KWT", "LBN", "LBY", "MAR", "OMN", "PAK", "QAT", "SAU",
           "SOM", "SDN", "SYR", "TUN", "ARE", "YEM", "PSE")
  WPR <- c("AUS", "BRN", "KHM", "CHN", "COK", "FJI", "JPN",
           "KIR", "LAO", "MYS", "MHL", "FSM", "MNG", "NRU", "NZL",
           "NIU", "PLW", "PNG", "PHL", "KOR", "WSM", "SGP", "SLB",
           "TON", "TUV", "VUT", "VNM", "HKG", "TWN", "MAC", "PYF",
           "NCL")
  output <- dplyr::case_when(iso3cs %in% AFR ~ "AFR", iso3cs %in%
                               AMR ~ "AMR", iso3cs %in% SEAR ~ "SEAR", iso3cs %in% EUR ~
                               "EUR", iso3cs %in% EMR ~ "EMR", iso3cs %in% WPR ~ "WPR",
                             TRUE ~ as.character(NA))
  if (any(is.na(output))) {
    warning(paste0("Unable to get assign the following iso3cs a WHO region: ",
                   paste0(iso3cs[is.na(output)], collapse = ", ")))
  }
  return(output)
}
