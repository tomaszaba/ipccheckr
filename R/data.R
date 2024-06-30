#'
#' A survey data with anthropometric measurements, including edema
#'
#' A SMART survey data with a data structure typical of data of ENA for SMART
#' Software. Location information anonymised.
#'
#' @format A data frame with 11 columns and 7650 rows
#'
#' | **Variable** | **Description** |
#' | :--- | :--- |
#' | *area* | Location where the survey took place |
#' | *dos* | Survey date |
#' | *cluster* | Primary sampling unit |
#' | *team* | Enumerator IDs |
#' | *sex* | Sex, "m" = boys, "f" = girls |
#' | *dob* | Date of birth |
#' | *age* | Age in months, typically estimated using local event calendars |
#' | *weight* | Weight (kg) |
#' | *height* | Height (cm) |
#' | *edema* | Edema, "n" = no, "y" = yes |
#' | *muac* | Mid-upper arm circumference (mm) |
#'
#' @examples
#'
#' anthro_data
#'
"anthro_data"

#'
#' District level representative survey data with zscores calculated
#'
#' @format A data frame with 10 columns and 7740 rows
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *area* | Location where the survey took place |
#' | *cluster* | Primary sampling unit |
#' | *sex* | Sex, "m" = boys, "f" = girls |
#' | *age* | calculated age in months with two decimal places |
#' | *edema* | Edema, "n" = no, "y" = yes |
#' | *muac* | Mid-upper arm circumference (mm) |
#' | *wfhz* | Weight-for-height z-scores with 3 decimal places |
#' | *flag_wfhz* | Flagged observations. 1=flagged, 0=not flagged |
#' | *mfaz* | MUAC-for-age z-scores with 3 decimal places |
#' | *flag_mfaz* | Flagged observations: 1=flagged, 0=not flagged |
#'
#' @examples
#' prev_data
#'
"prev_data"

# MUAC screening data ----------------------------------------------------------
#'
#' A MUAC screening data from an anonymized setting
#'
#' @format A data frame with 4 columns and 661 rows
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *sex* | Sex, "m" = boys, "f" = girls |
#' | *months* | calculated age in months with two decimal places |
#' | *edema* | Edema, "n" = no, "y" = yes |
#' | *muac* | Mid-upper arm circumference (mm) |
#'
#' @examples
#' muac_data
#'
"muac_data"
