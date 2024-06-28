# Function to detect outliers --------------------------------------------------

#'
#' Remove detected outliers
#'
#' `remove_flags()` removes flags detected by [flag_outliers()]. It helps you
#' compute your statistics when flags needs to be removed, such as in standard
#' deviation.
#'
#' @param x A numeric vector containing zscore or crude MUAC values.
#'
#' @param unit A choice of the units to which you wish remove flags on. variable into.
#'
#' @returns A vector of same size, with flagged data replaced by `NA`s.
#'
remove_flags <- function(x, unit = c("zscore", "crude")) {

  ## Match arguments ----
  unit <- match.arg(unit)

  ## Control flow based on unit ----
  switch(
    unit,
    ### Remove flags when unit = "zscore" ----
    "zscore" = {
      mean_x <- mean(x, na.rm = TRUE)
      zs <- ifelse((x < (mean_x - 3) | x > (mean_x + 3)) | is.na(x), NA_real_, x)
    },
    ### Remove flags when unit = "crude" ----
    "crude" = {
      cr <- ifelse(x < 100 | x > 200 | is.na(x), NA_integer_, x)
    }
  )
}

# Function on acute malnutrition case-definition -------------------------------

#'
#' Case-Definition: is an observation acutely malnourished?
#'
#' [wasting_cases_muac()], [wasting_cases_whz()] and [wasting_cases_combined()]
#' help you get through with your wasting case definition for each observation.
#' It should be used inside dplyr::mutate() or base::transform(). It was designed
#' to be used inside [define_wasting()].
#'
#' @param muac An integer vector containing MUAC measurements in mm.
#' @param zscore A double vector containing weight-for-height zscores with 3
#' decimal places.
#' @param edema A character vector of "y" = Yes, "n" = No bilateral edema.
#' Default is NULL.
#' @param cases A choice of wasting case definition you wish to apply. For combined
#' acute malnutrition with [wasting_cases_combined()] cases options are:
#' c("cgam", "csam", "cmam").
#'
#' @returns A dichotomous vector of 1=Yes and 0=No.
#'
#' @examples
#' # example code
#'
#' @rdname case_definition
#'
wasting_cases_muac <- function(muac, edema = NULL,
                               cases = c("gam", "sam", "mam")) {
  ## Match argument ----
  cases <- match.arg(cases)

  ## Define cases based on MUAC ----
  if (!is.null(edema)) {
    switch(
      cases,
      ### GAM ----
      "gam" = {gam <- ifelse(muac < 125 | edema == "y", 1, 0)},
      ### SAM ----
      "sam" = {sam <- ifelse(muac < 115 | edema == "y", 1, 0)},
      ### MAM ----
      "mam" = {mam <- ifelse((muac >= 115 & muac < 125 & edema == "n"), 1, 0)}
    )
  } else {
    switch(
      cases,
      ### GAM without edema ----
      "gam" = {gam <- ifelse(muac < 125, 1, 0)},
      ### SAM without edema ----
      "sam" = {sam <- ifelse(muac < 115, 1, 0)},
      ### MAM ----
      "mam" = {mam <- ifelse((muac >= 115 & muac < 125), 1, 0)}
    )
  }
}

#'
#'
#' @rdname case_definition
#'
#'
wasting_cases_whz <- function(zscore, edema = NULL,
                              cases = c("gam", "sam", "mam")) {
  ## Match argument ----
  cases <- match.arg(cases)

  ## Define cases based on wfhz ----
  if (!is.null(edema)) {
    switch(
      cases,
      ### GAM ----
      "gam" = {gam <- ifelse(zscore < -2 | edema == "y", 1, 0)},
      ### SAM ----
      "sam" = {sam <- ifelse(zscore < - 3 | edema == "y", 1, 0)},
      ### MAM ----
      "mam" = {mam <- ifelse((zscore >= -3 & zscore < -2 & edema == "n"), 1, 0)}
    )
  } else {
    switch(
      cases,
      ### GAM without edema ----
      "gam" = {gam <- ifelse(zscore < -2, 1, 0)},
      ### SAM without edema ----
      "sam" = {sam <- ifelse(zscore < - 3, 1, 0)},
      ### MAM ----
      "mam" = {mam <- ifelse(zscore >= -3 & zscore < -2, 1, 0)}
    )
  }
}

#'
#'
#' @rdname case_definition
#'
#'
wasting_cases_combined <- function(zscore, muac, edema = NULL,
                                   cases = c("cgam", "csam", "cmam")) {

  ## Match argument ----
  cases <- match.arg(cases)

  ## Define cases based on combined methods ----
  if (!is.null(edema)) {
    switch(
      cases,
      ### Combined GAM ----
      "cgam" = {cgam <- ifelse(zscore < -2 | muac < 125 | edema == "y", 1, 0)},
      ### Combined SAM ----
      "csam" = {csam <- ifelse(zscore < -3 | muac < 115 | edema == "y", 1, 0)},
      ### Combined MAM ----
      "cmam" = {cmam <- ifelse((zscore >= -3 & zscore < -2) | (muac >= 115 & muac < 125) & (edema == "n"), 1, 0)}
    )
  } else {
    switch(
      cases,
      ### Combined GAM without edema ----
      "cgam" = {cgam <- ifelse(zscore < -2 | muac < 125, 1, 0)},
      ### Combined SAM without edema ----
      "csam" = {csam <- ifelse(zscore < -3 | muac < 115, 1, 0)},
      ### Combined MAM ----
      "cmam" = {cmam <- ifelse((zscore >= -3 & zscore < -2) | (muac >= 115 & muac < 125), 1, 0)}
    )
  }
}

# Function to normalize/standardize scores -------------------------------------

#'
#' Normalize Zscores to follow a normal distribution
#'
#' According to the [SMART Methodology.](https://smartmethodology.org/),
#' when standard deviation of weight-for-height zscores is greater than 1.2,
#' the acute malnutrition prevalence is re-calculated using a standard deviation
#' of 1.0.
#'
#' `normalize_zscore()` helps to achieve this end by normalizing the zscores. It
#' subtracts each element in a vector of zscores with the mean of the same vector
#' and then devides by the standard deviation of the same vector.
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

# Function to classify nutritional status --------------------------------------

#'
#' A helper function to classify nutritional status
#'
#' `classify_wasting()` is used a helper inside [apply_CDC_age_weighting()] to
#' classify nutritional status into "sam", "mam" or "not wasted" and then the
#' vector returned is used downstream to calculate the proportions of children
#' with severe and moderate acute malnutrition.
#'
#' @param muac An integer vector containing MUAC values. They should be in
#' millimeters.
#'
#' @param edema Optional. Its a vector containing data on billateral pitting
#' edema coded as "y" for yes and "n" for no.
#'
#'
classify_wasting <- function(muac, edema = NULL) {
  if (!is.null(edema)) {
    #edema <- ifelse(edema == 1, "y", "n")
    x <- case_when(
      muac < 115 | edema == "y" ~ "sam",
      muac >= 115 & muac < 125 & edema == "n" ~ "mam",
      .default = "not wasted"
    )
  } else {
    x <- case_when(
      muac < 115 ~ "sam",
      muac >= 115 & muac < 125 ~ "mam",
      .default = "not wasted"
    )
  }
  x
}
