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
