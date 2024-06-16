#'
#' Assign a penalty point for the amount of proportion flagged data and standard deviation
#'
#' The function assigns a penalty score for a given category of test classification.
#' The score range varies between 0 (when "Excellent") to 20 (when "Problematic") for
#' both flagged data and standard deviation. This was borrowed from the
#' [ENA for SMART software](https://smartmethodology.org/)
#' In the SMART Methodology, flagged data and standard deviation are tho test
#' criteria that gets the highest penalty scores, so it is here.
#'
#' @param x A character vector containing the test classifications of proportion
#' of flagged data and the value of standard deviation.
#'
#' @returns A numeric vector with the corresponding penalty points (scores) according
#' to the classification.
#'
#' @examples
#'
#' ## Sample data ----
#' x <- c("Excellent", "Problematic", "Acceptable", "Good")
#' ## Apply the function ----
#' assign_penalty_points_flags_and_sd(x)
#'
#' @export
#'
assign_penalty_points_flags_and_sd <- function(x) {
  case_when(
    x == "Excellent" ~ 0,
    x == "Good" ~ 5,
    x == "Acceptable" ~ 10,
    x == "Problematic" ~ 20
  )
}

#'
#' Assign a penalty point for the amount of selection biases in age and sex ratios
#'
#' The function assigns a penalty score for a age and sex ratio's test classification.
#' The score range varies between 0 (when "Excellent") to 10 (when "Problematic") for
#' both, according to the [ENA for SMART software](https://smartmethodology.org/).
#'
#' @param x A numeric vector containing p-values from either age or sex ratio
#' test results.
#'
#' @returns A numeric vector with the corresponding penalty points (scores) according
#' to the classification.
#'
#' @examples
#'
#' ## A vector storing age ratio or sex ratio p-values' classification ----
#' x <- c("Excellent", "Problematic", "Acceptable", "Good")
#'
#' ## Apply the function ----
#' assign_penalty_points_age_sex_ratio(x)
#'
#' @export
#'
assign_penalty_points_age_sex_ratio <- function(x) {
  case_when(
    x == "Excellent" ~ 0,
    x == "Good" ~ 2,
    x == "Acceptable" ~ 4,
    x == "Problematic" ~ 10
  )
}

#'
#' Assign a penalty point for the amount of issues in Skweness and Kurtosis
#'
#' The function assigns a penalty score for a Skewness and Kurtosis test classification.
#' The score range varies between 0 (when "Excellent") to 5 (when "Problematic") for
#' both, according to the [ENA for SMART software](https://smartmethodology.org/).
#'
#' @param x A numeric vector containing Skewness or Kurtosis test results classification.
#'
#' @returns A numeric vector with the corresponding penalty points (scores) according
#' to the classification.
#'
#' @examples
#'
#' ## A vector storing Skewness or Kurtosis test classification ----
#'
#' x <- c("Excellent", "Problematic", "Acceptable", "Good")
#'
#' ## Apply the function ----
#' assign_penalty_points_skew_kurt(x)
#'
#' @export
#'
assign_penalty_points_skew_kurt <- function(x) {
  case_when(
    x == "Excellent" ~ 0,
    x == "Good" ~ 1,
    x == "Acceptable" ~ 3,
    x == "Problematic" ~ 5
  )
}

#'
#' Get the overall WHZ or MFAZ's quality score
#'
#' `compute_quality_score()` provides the overall quality score of either WHZ or MFAZ,
#' by adding up the scores across each test criteria. This is an input to
#' [classify_overall_quality()].
#'
#' @param df A data frame containing the scores. If you wish the get the overall
#' quality score for MFAZ, the input data frame must have seven (7) required
#' columns containing test classification of flagged data, sex ratio, age ratio,
#' standard deviation, skewness, kurtosis, crude MUAC's digit preference.
#' Alternatively, if you wish to get the quality score of WHZ, then the input
#' data frame must have the exact same columns in the plausibility report of the
#' ENA for SMART software.
#'
#' @param type The method you wish to get the overall quality score for.
#' A choice between "mfaz" and "whz". If you wish to know the overall survey
#' score of your WHZ data, set `type = whz`, otherwise set `type = mfaz` for
#' MFAZ. If by mistake a different input choice is given, an error will be
#' thrown with a message guiding how to go about.
#'
#' @returns A vector (named `"quality_score"`) with the overall quality scores.
#'
#' @examples
#' # example code
#' ## Create a `df` object ----
#'
#' df <- data.frame(
#' flagged_class = "Excellent",
#' age_ratio_class = "Good",
#' sex_ratio_class = "Problematic",
#' dps_class = "Excellent",
#' sd_class = "Excellent",
#' skew_class = "Good",
#' kurt_class = "Acceptable"
#' )
#'
#' ## Apply function ----
#' compute_quality_score(df, type = "mfaz")
#'
#' # You can also choose to chain the functions with a pipe operator ----
#' df |>
#' compute_quality_score(type = "mfaz")
#'
#' @export
#'
compute_quality_score <- function(df, type = c("mfaz", "whz")) {
  type <- match.arg(type)

  if (type == "mfaz") {

    ### Get MFAZ's quality score ----
    qscore <- df |>
      summarise(
        quality_score = sum(
          across(
            .cols = c(
              .data$flagged_class,
              .data$sd_class
              ),
            .fns = assign_penalty_points_flags_and_sd
          ),
          across(
            .cols = c(
              .data$sex_ratio_class,
              .data$age_ratio_class,
              .data$dps_class
              ),
            .fns = assign_penalty_points_age_sex_ratio
          ),
          across(
            .cols = c(
              .data$skew_class,
              .data$kurt_class
              ),
            .fns = assign_penalty_points_skew_kurt
          )
        )
      )
    qscore[["quality_score"]]

  } else {
    ### Get WHZ's quality score (REVISE)----
    qscore <- df |>
      summarise(
        quality_score = sum(
          across(
            .cols = c(
              .data$flagged_class,
              .data$sd_class
              ),
            .fns = assign_penalty_points_flags_and_sd
          ),
          across(
            .cols = c(
              .data$sex_ratio_class,
              .data$age_ratio_class,
              .data$dps_wgt_class,
              .data$dps_hgt_class
              ),
            .fns = assign_penalty_points_age_sex_ratio
          ),
          across(
            .cols = c(
              .data$skew_class,
              .data$kurt_class
              ),
            .fns = assign_penalty_points_skew_kurt
          )
        )
      )
    qscore[["quality_score"]]
  }
}

