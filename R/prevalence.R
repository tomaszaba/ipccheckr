# Function to add new vectors with case definitions ----------------------------
#'
#' Add acute malnutrition case-definitions to the data frame
#'
#' Use `define_wasting()` to add the case-definitions in your input data frame.
#'
#' @param df The data frame object with containing the vectors with zscores, muac
#' edema.
#' @param zscore The vector storing zscores values with 3 decimal places.
#' @param muac An integer vector containing MUAC measurements in mm.
#' @param edema A character vector of "y" = Yes, "n" = No bilateral edema.
#' Default is NULL.
#' @param base A choice of options to which you case definion should be based on.
#'
#' @returns A data frame with three vectors added to the input data frame: "gam",
#' "sam" and "mam". If base = "combined" the vector names change to "cgam" for
#' combined GAM, "csam" for combined SAM and "cmam" for combined MAM.
#'
#' @examples
#' # MUAC-based case-definition ----
#' x <- prev_data |>
#' define_wasting(muac = muac, edema = edema, base = "muac")
#' head(x)
#'
#' # Weight-for-height based case-definition ----
#' x <- prev_data |>
#' define_wasting(zscore = wfhz, edema = edema, base = "wfhz")
#' head(x)
#'
#' # Combined case-definition ----
#' x <- prev_data |>
#' define_wasting(zscore = wfhz, muac = muac, edema = edema, base = "combined")
#' head(x)
#'
#' @export
#'
define_wasting <- function(df, zscore = NULL, muac = NULL, edema = NULL,
                             base = c("wfhz", "muac", "combined")) {

  ## Match argument ----
  base <- match.arg(base)

  ## Create logical vectors of gam, sam and mam ----
  switch(
    base,
    "wfhz" = {
      df |>
        mutate(
          gam = wasting_cases_whz({{ zscore }}, edema = edema, cases = "gam"),
          sam = wasting_cases_whz({{ zscore }}, edema = edema, cases = "sam"),
          mam = wasting_cases_whz({{ zscore }}, edema = edema, cases = "mam")
        )
    },
    "muac" = {
      df |>
        mutate(
          gam = wasting_cases_muac({{ muac }}, edema = edema, cases = "gam"),
          sam = wasting_cases_muac({{ muac }}, edema = edema, cases = "sam"),
          mam = wasting_cases_muac({{ muac }}, edema = edema, cases = "mam")
        )
    },
    "combined" = {
      df |>
        mutate(
          cgam = wasting_cases_combined({{ zscore }}, {{ muac }}, edema = edema, cases = "cgam"),
          csam = wasting_cases_combined({{ zscore }}, {{ muac }}, edema = edema, cases = "csam"),
          cmam = wasting_cases_combined({{ zscore }}, {{ muac }}, edema = edema, cases = "cmam")
        )
    }
  )
}

# The CDC analysis approach ----------------------------------------------------

#'
#'
apply_CDC_age_weighting <- function(muac, age,
                                    edema = NULL, status = c("sam", "mam")) {

  ## Match arguments ----
  status <- match.arg(status)

  if (!is.null(edema)) {
    ### Define cases ----
    nut_status <- classify_wasting(muac, edema)

    ### Compute age weighted prevalence ----
    age_group <- ifelse(age < 24, "under_2", "over_2")
    nut_U2 <- ifelse(age_group == "under_2" & nut_status == status, 1, 0)
    nut_O2 <- ifelse(age_group == "over_2" & nut_status == status, 1, 0)
    w_prev <- mean(nut_U2, na.rm = TRUE) + (2 * mean(nut_O2, na.rm = TRUE)) / 3

  } else {
    ### Define cases ----
    nut_status <- classify_wasting(muac)

    ### Compute age weighted prevalence ----
    age_group <- ifelse(age < 24, "under_2", "over_2")
    nut_U2 <- ifelse(age_group == "under_2" & nut_status == status, 1, 0)
    nut_O2 <- ifelse(age_group == "over_2" & nut_status == status, 1, 0)
    w_prev <- mean(nut_U2, na.rm = TRUE) + (2 * mean(nut_O2, na.rm = TRUE)) / 3
  }
  w_prev
}
