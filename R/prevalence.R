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

#'
#'
# get_prevalence_estimates <- function(df, base = c("wfhz", "muac", "comb")) {
#   ## Match arguments ----
#   base <- match.arg(base)
#
#   ## Execute code conditionally ----
#   switch(base,
#     "wfhz" = {
#       ### Check standard deviation ----
#       x <- df |>
#         summarise(std = sd(remove_flags(.data$wfhz, "zscore"), na.rm = TRUE))
#       ### Check if standard deviation > 1.2 ----
#       if (x[["std"]] > 1.2) {
#         ### Normalize zscores to have mean = 0 and standard deviation = 1 ----
#         x <- summarise(mean = mean(.data$wfhz, na.rm = TRUE))
#         y <- summarise(std = sd(remove_flags(.data$wfhz, "zscore"), na.rm = TRUE))
#         for i in df$wfhz
#
#       }
#     }
#   )
# }
