#'
#' Classify how much high is the proportion of flagged data
#'
#' `classify_percent_flagged()` tells you how much high is the proportion of
#' of flagged data in your data set, an indication of quality of data. Its a
#' reusable function for MFAZ, WHZ and crude MUAC. The cut-offs for MFAZ and
#' crude MUAC are the same with the upper limit of 2%. This is based on the
#' research findings by [Bilukha, O., & Kianian, B. (2023).](https://doi.org/10.1111/mcn.13478),
#' from a multi-country analysis, found that the correlation between the mean
#' MFAZ and crude MUAC was almost perfect (r=99). As for WHZ, the cut-offs are
#' exactly those in the [SMART Methodology](https://smartmethodology.org/).
#'
#' @param p A numeric vector containing the proportions of flagged data
#'
#' @param type The method to which you wish to classify how much high are the
#' proportions of flagged data. A choice between "mfaz" for MFAZ, "whz" for WHZ
#' and "crude" for crude MUAC.
#'
#' @returns A character vector with the correspondent classification of the
#' amount of flagged data. The categories of classification ranges are:
#' "Excellent", "Good", "Acceptable", "Problematic".
#'
#' @examples
#'
#' ## Take a vector with the proportions of flagged data ----
#' prop <- c(0.0, 0.0, 0.01, 0.015, 0.2, 0.015, 0.016, 0.017, 0.05, 0.06,
#' 0.03, 0.03, 0.04, 0.000001, 0)
#'
#' ## Apply the function setting type to "whz" for instance ----
#' classify_percent_flagged(prop, type = "whz")
#'
#' @export
#'
classify_percent_flagged <-  function(p, type = c("mfaz", "whz", "crude")) {

  type <- match.arg(type)

  if (type == "mfaz" || type == "crude") {

    ## classify percent of outliers in MFAZ ----
    x <- cut(
      x = p,
      breaks = c(0, 0.01, 0.015, 0.02, Inf),
      labels = c("Excellent", "Good", "Acceptable", "Problematic"),
      include.lowest = TRUE,
      right = TRUE
    )
  }

  if (type == "whz") {

    ## classify percent of outliers in WHZ ----
    x <- cut(
      x = p,
      breaks = c(0, 0.025, 0.05, 0.075, Inf),
      labels = c("Excellent", "Good", "Acceptable", "Problematic"),
      include.lowest = TRUE,
      right = TRUE
    )
  }
  x
}


#'
#' Classify how much high is the difference in age ration and in sex ratio
#'
#' `classify_age_sex_ratio()` works on the results yielded by [nipnTK::ageRatioTest()].
#' It helps you know how much high is the statistical difference between children
#' age 6-29 months of those age 30-59 months. Likewise, with regard to sex,
#' function works on the results yielded by [nipnTK::sexRatioTest()] to know
#' how much high is the difference between boy and girls in your sample data.
#'
#' @param p A numeric vector containing the test p-values.
#'
#' @returns A character vector with the correspondent classification.
#'
#' @examples
#'
#' ## Have a numeric vector storing p-values ----
#' pvalues <- c(0, 0, 0.01, 0.011, 0.2, 0.015, 0.016, 0.017,
#' 0.05, 0.06,0.03, 0.03, 0.04, 0.000001, 0.07
#' )
#'
#' ## Apply the function ----
#' classify_age_sex_ratio(pvalues)
#'
#' @export
#'
classify_age_sex_ratio <- function(p) {
  case_when(
    p > 0.1 ~ "Excellent",
    p > 0.05 ~ "Good",
    p > 0.001 ~ "Acceptable",
    TRUE ~ "Problematic"
  )
}


#'
#' Classify how much high is the value of standard deviation
#'
#' `classify_sd()` helps you to know the magnitude of the data's standard
#' deviation. You can use this function for either WHZ, MFAZ or crude MUAC.
#' Cut-offs for WHZ are based on the [SMART Methodology](https://smartmethodology.org/).
#' Cut-offs for MFAZ are also based on SMART, but informed by
#' [Bilukha, O., & Kianian, B. (2023).](https://doi.org/10.1111/mcn.13478).
#' For crude MUAC, the cut-offs are based on the
#' [IPC AMN guidelines](https://www.ipcinfo.org/ipcinfo-website/resources/ipc-manual/en/)
#'
#' @param sd A numeric vector containing values for standard deviation of the
#' method you wish the work on.
#'
#' @param type The method to which you wish to classify how much high is the
#' value of standard deviation. A choice between "zscore" MFAZ or WHZ and
#' "crude" for crude MUAC.
#'
#' @returns A character vector with the correspondent classification.
#'
#' @examples
#'
#' ## Have a vector with standard deviation ----
#' sdvalues <- seq(0.7, 1.3, by = 0.001) |>
#' sample(size = 9, replace = TRUE)
#'
#' ## Apply the function with `type = "zscore` ----
#' classify_sd(sdvalues, type = "zscore")
#'
#' ## Using `type = "crude"` ----
#' ### Create sample data ----
#' sdvalues <- seq(9, 30, by = 2) |>
#' sample(size = 20, replace = TRUE)
#'
#' ### Apply the function with `type = "crude"` ----
#' classify_sd(sdvalues, type = "crude")
#'
#' @export
#'
classify_sd <-  function(sd, type = c("zscore", "crude")) {

  type <- match.arg(type)

  if (type == "zscore") {

    ## Classify WHZ and MFAZ-based standard deviation ----
    x <- case_when(
      sd > 0.9 & sd < 1.1 ~ "Excellent",
      sd > 0.85 & sd < 1.15 ~ "Good",
      sd > 0.8 & sd < 1.20 ~ "Acceptable",
      TRUE ~ "Problematic"
      )
  }

  if (type == "crude") {

    ## Classify crude MUAC-based standard deviation ----
    x <- cut(
      x = sd,
      breaks = c(-Inf, 13, 14, 15, Inf),
      labels = c("Excellent", "Acceptable", "Poor", "Problematic"),
      include.lowest = FALSE,
      right = FALSE
    )
  }
  x
}


#'
#' Classify how much high is the value of Skewness and Kurtosis
#'
#' `classify_skew_kurt()` helps you to know the magnitude of the Skewness and
#' Kurtosis from your data. This is only useful for WHZ and MFAZ. The function
#' works on the results yielded by [nipnTK::skewKurt()].
#' Cut-offs for WHZ are based on the [SMART Methodology](https://smartmethodology.org/).
#'
#' @param sk A numeric vector containing values of either Skewness or Kurtosis.
#'
#' @returns A character vector with the correspondent classification.
#'
#' @examples
#'
#' #Have a numeric vector storing values for skewness or kurtosis ----
#' sk <- seq(-5, 1, by = 0.05) |> sample(size = 20, replace = TRUE)
#'
#' # Apply function
#' classify_skew_kurt(sk)
#'
#' @export
#'
classify_skew_kurt <- function(sk) {
  cut(
    x = sk,
    breaks = c(-Inf, 0.2, 0.4, 0.6, Inf),
    labels = c("Excellent", "Good", "Acceptable", "Problematic"),
    include.lowest = FALSE,
    right = TRUE
  )
}

#'
#' Get the overall data quality classification
#'
#' `classify_overall_quality()` helps you in knowing the overall status of your
#' data quality. It classifies the overall score generated by
#' [compute_quality_score()] into four categories, as it is done in the
#' [SMART Methodology](https://smartmethodology.org/),
#' namely: "Excellent", "Good", "Acceptable" and "Problematic". Beware that
#' the overall classification should be used as an indication to further
#' scrutinize of data before taking the decision to validate or invalidate the
#' results.
#'
#' @param df A data frame containing a vector with the quality scores generated by
#' [compute_quality_score()].
#'
#' @returns A character vector of the same length, but a different width as the
#' input `df` is returned with a new column called `quality_class`.
#'
#'
#' @export
#'
classify_overall_quality <- function(df) {

  qclass <- with(
    df,
    data.frame(
      quality_class <- cut(
        x = quality_score,
        breaks = c(0, 9, 14, 24, Inf),
        labels = c("Excellent", "Good", "Acceptable", "Problematic"),
        include.lowest = TRUE,
        right = TRUE
      )
    )
  )
  qclass$quality_class
}
