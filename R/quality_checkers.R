#'
#' Plausibility checkers: MUAC-for-age z-scores, Weight-for-Height z-scores and
#' MUAC
#'
#' @description
#' `check_plausibility_mfaz()`, `check_plausibility_wfhz()` and
#' `check_plausibility_muac()` lets you know the quality of your data, based on
#' the statistics around MUAC-for-age zscores, weight-for-height z-scores and on
#' crude MUAC, respectively. Note that `check_plausibility_wfhz()` is all about
#' WHZ only. If you wish to know about MUAC checks consider using either
#' `check_plausibility_mfaz()` or `check_plausibility_muac()`
#'
#' @param df A data frame object returned by [process_muac_data()] for
#' `check_plausibility_mfaz()` and `check_plausibility_muac()` and returned by
#' [process_wfhz_data()] for `check_plausibility_wfhz()`.
#'
#' @param sex A vector telling whether a given child is a boy or girl.
#'
#' @param age A vector containing children's age in months.
#'
#' @param muac A vector containing MUAC measurements.
#'
#' @param weight A vector containing weight measurements in kilograms.
#'
#' @param height A vector containing height measurements in centimeters.
#'
#' @param flags A character vector telling whether or not an observation is an
#' outlier.
#'
#' @param area A vector with values on where was the data collected. If you are
#' analyzing a data set with just one area, provide it anyway to
#' `check_plausibility_mfaz()` or `check_plausibility_wfhz()`
#'
#' @returns A summarized data frame containing quality checks statistics and
#' respective classification.
#'
#'
#' @examples
#'
#' ## Check Plausibility: MFAZ ----
#'
#' anthro.01 |>
#' process_age(
#' svdate = "dos",
#' birdate = "dob",
#' age = age
#' ) |>
#' process_muac_data(
#' sex = sex,
#' age = "age",
#' muac = muac,
#' .recode_sex = TRUE,
#' .recode_muac = TRUE,
#' unit = "cm"
#' ) |>
#' check_plausibility_mfaz(
#' flags = flag_mfaz,
#' sex = sex,
#' muac = muac,
#' age = age,
#' area = area
#' )
#'
#' ## Check Plausibility: WFHZ ----
#'
#' anthro.01 |>
#' process_age(
#' svdate = "dos",
#' birdate = "dob",
#' age = age
#' ) |>
#' process_wfhz_data(
#' sex = sex,
#' weight = weight,
#' height = height,
#' .recode_sex = TRUE
#' ) |>
#' check_plausibility_wfhz(
#' sex = sex,
#' age = age,
#' weight = weight,
#' height = height,
#' flags = flag_wfhz,
#' area = area
#' )
#'
#' ## Check Plausibility: MUAC ----
#'
#' anthro.01 |>
#' process_muac_data(
#' sex = sex,
#' muac = muac,
#' age = NULL,
#' .recode_sex = TRUE,
#' .recode_muac = FALSE,
#' unit = "none"
#' ) |>
#' check_plausibility_muac(
#' flags = flag_muac,
#' sex = sex,
#' muac = muac
#' )
#'
#' @rdname plausibility_checkers
#'
#' @export
#'
check_plausibility_mfaz <- function(df, sex, muac, age, flags, area) {

  ## Summarise statistics  ----
  df <- df |>
    dplyr::group_by({{ area  }}) |>
    dplyr::summarise(
      n = n(),
      flagged = sum({{ flags }}, na.rm = TRUE) / n(),
      flagged_class = classify_percent_flagged(.data$flagged, type = "mfaz"),
      sex_ratio = sexRatioTest({{ sex }}, code = c(1, 2))$p,
      sex_ratio_class = classify_age_sex_ratio(.data$sex_ratio),
      age_ratio = age_ratio_test({{ age }}, .expectedP = 0.66)$p,
      age_ratio_class = classify_age_sex_ratio(.data$age_ratio),
      dps = digitPreference({{ muac }}, digits = 1, values = 0:9)$dps,
      dps_class = digitPreference({{ muac }}, digits = 1, values = 0:9)$dpsClass,
      sd = sd(remove_flags(.data$mfaz, unit = "zscore"), na.rm = TRUE),
      sd_class = classify_sd(.data$sd, type = "zscore"),
      skew = skewKurt(remove_flags(.data$mfaz, unit = "zscore"))$s,
      skew_class = classify_skew_kurt(.data$skew),
      kurt = skewKurt(remove_flags(.data$mfaz, unit = "zscore"))$k,
      kurt_class = classify_skew_kurt(.data$kurt),
      .groups = "drop"
    )

  ## Add quality score to the data frame ----

  df[["quality_score"]] <- df |>
    group_by({{ area }}) |>
    compute_quality_score(type = "mfaz")

  ## Add quality class to the data frame ----

  df[["quality_class"]] <- df |>
    group_by({{ area }}) |>
    classify_overall_quality()

  ## Return data frame ----
  df
}


#'
#'
#' @rdname plausibility_checkers
#'
#' @export
#'
check_plausibility_wfhz <- function(df, sex, age, weight, height, flags, area) {


  ## Summarise statistics  ----
  df <- df |>
    group_by({{ area  }}) |>
    summarise(
      n = n(),
      flagged = sum({{ flags }}, na.rm = TRUE) / n(),
      flagged_class = classify_percent_flagged(.data$flagged, type = "whz"),
      sex_ratio = sexRatioTest({{ sex }}, code = c(1, 2))$p,
      sex_ratio_class = classify_age_sex_ratio(.data$sex_ratio),
      age_ratio = ageRatioTest({{ age }}, ratio = 0.85)$p,
      age_ratio_class = classify_age_sex_ratio(.data$age_ratio),
      dps_wgt = digitPreference({{ weight }}, digits = 1)$dps,
      dps_wgt_class = digitPreference({{ weight }}, digits = 1)$dpsClass,
      dps_hgt = digitPreference({{ height }}, digits = 1)$dps,
      dps_hgt_class = digitPreference({{ height }}, digits = 1)$dpsClass,
      sd = sd(remove_flags(.data$wfhz, unit = "zscore"), na.rm = TRUE),
      sd_class = classify_sd(.data$sd, type = "zscore"),
      skew = skewKurt(remove_flags(.data$wfhz, unit = "zscore"))$s,
      skew_class = classify_skew_kurt(.data$skew),
      kurt = skewKurt(remove_flags(.data$wfhz, unit = "zscore"))$k,
      kurt_class = classify_skew_kurt(.data$kurt),
      .groups = "drop"
    )

  ## Add quality score to the data frame ----

  df[["quality_score"]] <- df |>
    group_by({{ area }}) |>
    compute_quality_score(type = "whz")

  ## Add quality class to the data frame ----

  df[["quality_class"]] <- df |>
    group_by({{ area }}) |>
    classify_overall_quality()

  ## Return data frame ----
  df
}



#'
#' @rdname plausibility_checkers
#'
#' @export
#'
check_plausibility_muac <- function(df, flags, sex, muac) {

  ## Summarise statistics  ----
  df <- df |>
    summarise(
      n = n(),
      flagged = sum({{ flags }}, na.rm = TRUE) / n(),
      flagged_class = classify_percent_flagged(.data$flagged, type = "crude"),
      sex_ratio = sexRatioTest({{ sex }}, code = c(1, 2))[["p"]],
      sex_ratio_class = classify_age_sex_ratio(.data$sex_ratio),
      dps = digitPreference({{ muac }}, digits = 0, values = 0:9)[["dps"]],
      dps_class = digitPreference({{ muac }}, digits = 0, values = 0:9)[["dpsClass"]],
      sd = sd(remove_flags({{ muac }}, unit = "crude"), na.rm = TRUE),
      sd_class = classify_sd(.data$sd, type = "crude"),
      .groups = "drop"
    )

  ## Return data frame ----
  df
}
