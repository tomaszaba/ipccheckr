#'
#' Raw data of a district level representative survey
#'
#' @description
#' #' `anthro.01` is about a two-stage and PPS cluster sampling survey data
#' conducted in two district following the SMART survey methodology in two
#' livelihood zones. The location information was anonymized for confidentiality.
#'
#' @format A tibble with 1191 rows and 11 columns.
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
#' anthro.01
#'
"anthro.01"

#'
#'
#' Province representative survey conducted in Mozambique
#'
#' @description
#' `anthro.02` is about a household budget survey conducted in Mozambique in
#' 2019/2020, known as IOF (*Inquérito ao Orçamento Familiar* in portuguese).
#' The data is publicly available [here](https://mozdata.ine.gov.mz/index.php/catalog/88#metadata-data_access).
#' The survey had a module on nutrition with anthropometric measurements taken
#' from children age 0-59 months for weight-for-height and 6-59 months for MUAC.
#' *IOF* is a cluster and PPS-based, survey, with sampling done in two stages,
#' designed to give representative estimates at province level. Its data
#' collection spans for a period of 12 months, with anthropometric measurements
#' taken during that period too. Read the [Bureau of Statistic's website on
#' IOF](https://mozdata.ine.gov.mz/index.php/catalog/88#metadata-sampling) for
#' more details.
#'
#' `anthro.02` has been processed for this package's purpose.
#'
#' @format A tibble with 2267 rows and 14 columns.
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *province* | The administrative unit (admin 1) where data was collected. |
#' | *strata* | Rural and Urban |
#' | *cluster* | Primary sampling unit |
#' | *sex* | Sex, "m" = boys, "f" = girls |
#' | *age* | calculated age in months with two decimal places |
#' | *weight* | Weight (kg) |
#' | *height* | Height (cm) |
#' | *edema* | Edema, "n" = no, "y" = yes |
#' | *muac* | Mid-upper arm circumference (mm) |
#' | *wtfactor* | Survey weights |
#' | *wfhz* | Weight-for-height z-scores with 3 decimal places |
#' | *flag_wfhz* | Flagged observations. 1=flagged, 0=not flagged |
#' | *mfaz* | MUAC-for-age z-scores with 3 decimal places |
#' | *flag_mfaz* | Flagged observations. 1=flagged, 0=not flagged |
#'
#' @examples
#' anthro.02
#'
#'
"anthro.02"


#'
#' District level SMART surveys conducted in four district in Mozambique
#'
#' @description
#' This example data contains survey data of four districts. Two of them have their WFHZ
#' standard deviation classified as problematic, and the are other two within range of
#' acceptable standard deviation. The data is used to test the performance of WFHZ based
#' prevalence when used on a data set with multiple survey areas that may or not have
#' different classification for standard deviation that may warrant different analysis
#' approach, as the function is designed for.
#'
#' @format A tibble of 943 x 9.
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *district* | The administrative unit (admin 1) where data was collected. |
#' | *cluster* | Primary sampling unit |
#' | *team* | Survey teams |
#' | *sex* | Sex, "m" = boys, "f" = girls |
#' | *age* | calculated age in months with two decimal places |
#' | *weight* | Weight (kg) |
#' | *height* | Height (cm) |
#' | *edema* | Edema, "n" = no, "y" = yes |
#' | *muac* | Mid-upper arm circumference (mm) |
#'
"anthro.03"


#'
#' MUAC data from a community-based sentinel site from an anonymized location
#'
#' @description
#' Data in `anthro.04` was generated from a community-based sentinel site of three provinces.
#' Each province data set holds different scenarios that informs the appropriate analysis
#' approach to follow. One province (province 3) has its MFAZ standard deviation and age
#' ratio tests classified as problematic. Another province (province 2) has its age ratio
#' classified as problematic, but with a within range standard deviation. Lastly, province 1
#' has both tests falling within range of nor problematic. The data is used to test the
#' performance of `[compute_muac_prevalence()]` based when used on a multiple survey areas
#' data that may or not have on the aforementioned test that may then warrant a different
#' analysis approach, as the function is designed for.
#'
#' @format A tibble of 3,002 x 8.
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *province* |
#' | *cluster* | Primary sampling unit |
#' | *sex* | Sex, "m" = boys, "f" = girls |
#' | *age* | calculated age in months with two decimal places |
#' | *muac* | Mid-upper arm circumference (mm) |
#' | *edema* | Edema, "n" = no, "y" = yes |
#' | *mfaz* | MUAC-for-age z-scores with 3 decimal places |
#' | *flag_mfaz* | Flagged observations. 1=flagged, 0=not flagged |
#'
"anthro.04"


#'
#' A SMART survey data with standard deviation on weight-for-height zscores
#' classified as problematic
#'
#' @description
#' A SMART survey data with weight-for-height data where standard deviation is
#' problematic. The data is used to test that `compute_wfhz_prevalence()` works as
#' designed for when standard deviation is problematic.
#'
#' @format A tibble with 303 rows and 6 columns.
#'
#' | **Variable** | **Description** |
#' | :--- | :---|
#' | *cluster* | Primary sampling unit |
#' | *sex* | Sex, "m" = boys, "f" = girls |
#' | *age* | calculated age in months with two decimal places |
#' | *edema* | Edema, "n" = no, "y" = yes |
#' | *wfhz* | MUAC-for-age z-scores with 3 decimal places |
#' | *flag_wfhz* | Flagged observations. 1=flagged, 0=not flagged |
#'
#' @examples
#' wfhz.01
#'
#'
#' @rdname data
#'
"wfhz.01"


#'
#' A MUAC screening data from an anonymized setting
#'
#' @format A tibble with 661 rows and 4 columns.
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *sex* | Sex, "m" = boys, "f" = girls |
#' | *months* | calculated age in months with two decimal places |
#' | *edema* | Edema, "n" = no, "y" = yes |
#' | *muac* | Mid-upper arm circumference (mm) |
#'
#' @examples
#' mfaz.01
#'
"mfaz.01"

#' A SMART survey data with MUAC
#'
#' @description
#' A SMART survey data collected in an anonymized location. This data has
#' mfaz standard deviation and age ratio within range for a normal prevalence
#' analysis. It is, thus, used to check if `compute_muac_prevalence()` performs
#' as designed.
#'
#' @format A tibble with 303 rows and 7 columns.
#'
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *cluster* | Primary sampling unit |
#' | *sex* | Sex, "m" = boys, "f" = girls |
#' | *age* | calculated age in months with two decimal places |
#' | *edema* | Edema, "n" = no, "y" = yes |
#' | *mfaz* | MUAC-for-age z-scores with 3 decimal places |
#' | *flag_mfaz* | Flagged observations. 1=flagged, 0=not flagged |
#'
#' @examples
#' mfaz.02
#'
"mfaz.02"
