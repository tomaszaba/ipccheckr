# Function to normalize/standardize scores -------------------------------------

#'
#' Normalize zscores to follow a normal distribution
#'
#' @description
#'
#' According to the [SMART Methodology.](https://smartmethodology.org/),
#' when standard deviation of weight-for-height zscores is problematic,
#' the acute malnutrition prevalence is re-calculated after normalizing the
#' zscores to get a standard deviation of 1.0.
#'
#' `normalize_zscore()` helps to achieve this end by normalizing the zscores. It
#' subtracts each element in a vector of zscores with the observed mean of the
#' same vector and then divides by the observed standard deviation of the same
#' vector.
#'
#' @param x A numeric (double) vector storing zscores (with a least 3 decimal
#' places)
#'
#' @returns A numeric vector of the same length as the input vector
#'
#'
normalize_zscore <- function(x) {

  ## Get mean zscore ----
  mean_x <- mean(remove_flags(x, "zscore"), na.rm = TRUE)
  ## Get zscore's standard deviation ----
  std_x <- sd(remove_flags(x, "zscore"), na.rm = TRUE)
  ## Empty vector ----
  norm_x <- numeric(length(x))
  ## Normalize each element in x ----
  for (i in seq_along(x)) {
    norm_x[i] <- (x[i] - mean_x) / std_x
  }
  norm_x
}


# Function to identify the type of treatment for prevalence --------------------
#'
#' A helper function to tell how to go about MUAC prevalence analysis based on
#' on the output of age ratio and standard deviation test results
#'
#' @param age_ratio_class,sd_class Character vectors storing age ratio's p-values
#' and standard deviation's classification, respectively.
#'
#' @returns A character vector of the same length containing the indication of
#' what to do for the MUAC prevalence analysis: "weighted", "unweighted" and
#' "missing". If "weighted", the CDC weighting approach is applied to correct for
#' age bias. If "unweighted" a normal complex sample analysis is applied, and for
#' the latter, NA are thrown.
#'
#'
tell_muac_analysis_strategy <- function(age_ratio_class, sd_class) {
  case_when(
    age_ratio_class == "Problematic" & sd_class != "Problematic" ~ "weighted",
    age_ratio_class != "Problematic" & sd_class == "Problematic" ~ "missing",
    age_ratio_class == "Problematic" & sd_class == "Problematic" ~ "missing",
    .default = "unweighted"
  )
}


# Function on acute malnutrition case-definition -------------------------------

#'
#' Case-Definition: is an observation acutely malnourished?
#'
#' [define_wasting_cases_muac()], [define_wasting_cases_whz()] and
#' [define_wasting_cases_combined()] help you get through with your wasting
#' case-definition for each observation. It should be used inside dplyr::mutate()
#' or base::transform(). It was designed to be used inside [define_wasting()].
#'
#' @param muac An integer vector containing MUAC measurements in mm.
#' @param zscore A double vector containing weight-for-height zscores with 3
#' decimal places.
#' @param edema A character vector of "y" = Yes, "n" = No bilateral edema.
#' Default is NULL.
#' @param cases A choice of wasting case definition you wish to apply. For combined
#' acute malnutrition with [define_wasting_cases_combined()] cases options are:
#' c("cgam", "csam", "cmam").
#'
#' @returns A dichotomous vector of 1=Yes and 0=No.
#'
#' @examples
#' # example code
#'
#' @rdname case_definitions
#'
define_wasting_cases_muac <- function(muac, edema = NULL,
                               cases = c("gam", "sam", "mam")) {
  ## Match argument ----
  cases <- match.arg(cases)

  ## Define cases based on MUAC ----
  if (!is.null(edema)) {
    switch(
      cases,
      "gam" = {gam <- ifelse(muac < 125 | edema == "y", 1, 0)},
      "sam" = {sam <- ifelse(muac < 115 | edema == "y", 1, 0)},
      "mam" = {mam <- ifelse((muac >= 115 & muac < 125 & edema == "n"), 1, 0)}
    )
  } else {
    switch(
      cases,
      "gam" = {gam <- ifelse(muac < 125, 1, 0)},
      "sam" = {sam <- ifelse(muac < 115, 1, 0)},
      "mam" = {mam <- ifelse((muac >= 115 & muac < 125), 1, 0)}
    )
  }
}

#'
#'
#' @rdname case_definitions
#'
#'
define_wasting_cases_whz <- function(zscore, edema = NULL,
                              cases = c("gam", "sam", "mam")) {
  ## Match argument ----
  cases <- match.arg(cases)

  ## Define cases based on wfhz ----
  if (!is.null(edema)) {
    switch(
      cases,
      "gam" = {gam <- ifelse(zscore < -2 | edema == "y", 1, 0)},
      "sam" = {sam <- ifelse(zscore < - 3 | edema == "y", 1, 0)},
      "mam" = {mam <- ifelse((zscore >= -3 & zscore < -2 & edema == "n"), 1, 0)}
    )
  } else {
    switch(
      cases,
      "gam" = {gam <- ifelse(zscore < -2, 1, 0)},
      "sam" = {sam <- ifelse(zscore < - 3, 1, 0)},
      "mam" = {mam <- ifelse(zscore >= -3 & zscore < -2, 1, 0)}
    )
  }
}

#'
#'
#' @rdname case_definitions
#'
#'
define_wasting_cases_combined <- function(zscore, muac, edema = NULL,
                                   cases = c("cgam", "csam", "cmam")) {

  ## Match argument ----
  cases <- match.arg(cases)

  ## Define cases based on combined methods ----
  if (!is.null(edema)) {
    switch(
      cases,
      "cgam" = {cgam <- ifelse(zscore < -2 | muac < 125 | edema == "y", 1, 0)},
      "csam" = {csam <- ifelse(zscore < -3 | muac < 115 | edema == "y", 1, 0)},
      "cmam" = {cmam <- ifelse((zscore >= -3 & zscore < -2) | (muac >= 115 & muac < 125) & (edema == "n"), 1, 0)}
    )
  } else {
    switch(
      cases,
      "cgam" = {cgam <- ifelse(zscore < -2 | muac < 125, 1, 0)},
      "csam" = {csam <- ifelse(zscore < -3 | muac < 115, 1, 0)},
      "cmam" = {cmam <- ifelse((zscore >= -3 & zscore < -2) | (muac >= 115 & muac < 125), 1, 0)}
    )
  }
}


# Function to add new vectors with case definitions ----------------------------
#'
#' Add acute malnutrition case-definitions to the data frame
#'
#' Use `define_wasting()` to add the case-definitions in your input data frame.
#'
#' @param df The data frame object containing the vectors with zscores, muac and
#' edema.
#' @param zscore The vector storing zscores values with 3 decimal places.
#' @param muac An integer vector containing MUAC measurements in mm.
#' @param edema A character vector of "y" = Yes, "n" = No bilateral edema.
#' Default is NULL.
#' @param base A choice of options to which you case definition should be based on.
#'
#' @returns A data frame with three vectors added to the input data frame: "gam",
#' "sam" and "mam". If base = "combined" the vector names change to "cgam",
#' "csam" and "cmam" for combined global, severe and moderate acute malnutrition
#' respectively.
#'
#' @examples
#' # MUAC-based case-definition ----
#' x <- anthro.02 |>
#' define_wasting(
#' muac = muac,
#' edema = edema,
#' base = "muac"
#' )
#' head(x)
#'
#' # Weight-for-height based case-definition ----
#' x <- anthro.02 |>
#' define_wasting(
#' zscore = wfhz,
#' edema = edema,
#' base = "wfhz"
#' )
#' head(x)
#'
#' # Combined case-definition ----
#' x <- anthro.02 |>
#' define_wasting(
#' zscore = wfhz,
#' muac = muac,
#' edema = edema,
#' base = "combined"
#' )
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
          gam = define_wasting_cases_whz(
            zscore = {{ zscore }},
            edema = {{ edema }},
            cases = "gam"
            ),
          sam = define_wasting_cases_whz(
            zscore = {{ zscore }},
            edema = {{ edema }},
            cases = "sam"
            ),
          mam = define_wasting_cases_whz(
            zscore = {{ zscore }},
            edema = {{ edema }},
            cases = "mam")
        )
    },
    "muac" = {
      df |>
        mutate(
          gam = define_wasting_cases_muac(
            muac = {{ muac }},
            edema = {{ edema }},
            cases = "gam"
            ),
          sam = define_wasting_cases_muac(
            muac = {{ muac }},
            edema = {{ edema }},
            cases = "sam"
          ),
          mam = define_wasting_cases_muac(
            muac = {{ muac }},
            edema = {{ edema }},
            cases = "mam"
            )
        )
    },
    "combined" = {
      df |>
        mutate(
          cgam = define_wasting_cases_combined(
            zscore = {{ zscore }},
            muac = {{ muac }},
            edema = {{ edema }},
            cases = "cgam"
          ),
          csam = define_wasting_cases_combined(
            zscore = {{ zscore }},
            muac = {{ muac }},
            edema = {{ edema }},
            cases = "csam"
          ),
          cmam = define_wasting_cases_combined(
            zscore = {{ zscore }},
            muac = {{ muac }},
            edema = {{ edema }},
            cases = "cmam"
            )
        )
    }
  )
}
