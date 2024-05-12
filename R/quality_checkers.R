#'
#' Plausibility check: MUAC-for-age zscores
#'
#' `check_plausibility_mfaz()` lets you know the quality of your data, based on
#'the statistics around MFAZ.
#'
#'@param df A data frame object returned by [process_muac_data()].
#'
#'@param flags A character vector telling whether or not an observation is an
#' outlier.
#'
#'@param sex A vector telling whether a given child is a boy or girl.
#'
#'@param muac A vector containing MUAC measurements. This is used by
#' [nipnTK::digitPreference].
#'
#'@param age A vector containing children age in months.
#'
#'@param area A vector with values on where was the data collected. If you are
#' analyzing a data set with just one area, provide it anyway to `check_plausibility_mfaz()`
#'
#'@returns A summarized data frame of 18 columns containing quality checks
#' statistics and respective classification. The classification is the same as that
#' of SMART Methodology.
#'
#'@examples
#'
#'quality <- data.01 |>
#'process_age() |>
#'process_muac_data(
#'sex = "sex",
#'age = "age",
#'muac = "muac",
#'.recode_sex = TRUE,
#'.recode_muac = TRUE,
#'unit = "cm"
#') |>
#'check_plausibility_mfaz(
#'flags = "flags",
#'sex = "sex",
#'muac = "muac",
#'age = "age",
#'area = area
#')
#'
#'quality
#'
#'@export
#'
check_plausibility_mfaz <- function(df, flags, sex, muac, age, area) {

  df <- df |>
    dplyr::group_by({{ area  }}) |>
    dplyr::summarise(
      n = n(),
      flagged = do.call(sum, list(flags, na.rm = TRUE)) / n(),
      flagged_class = do.call(classify_percent_flagged, list(flagged, method = "mfaz")),
      sex_ratio = do.call(sexRatioTest, list(sex, code = c(1, 2)))$p,
      sex_ratio_class = do.call(classify_age_sex_ratio, list(sex_ratio)),
      age_ratio = do.call(age_ratio_test, list(age, .expectedP = 0.66))$p,
      age_ratio_class = do.call(classify_age_sex_ratio, list(age_ratio)),
      dps = digitPreference(muac, digits = 1, values = 0:9)$dps,
      dps_class = digitPreference(muac, digits = 1, values = 0:9)$dpsClass,
      sd = do.call(sd, list(do.call(remove_flags, list(mfaz)), na.rm = TRUE)),
      sd_class = do.call(classify_sd, list(sd, method = "zscore")),
      skew = do.call(skewKurt, list(do.call(remove_flags, list(mfaz))))$s,
      skew_class = do.call(classify_skew_kurt, list(skew)),
      kurt = do.call(skewKurt, list(do.call(remove_flags, list(mfaz))))$k,
      kurt_class = do.call(classify_skew_kurt, list(kurt)),
      .groups = "drop"
    )

  ## Add quality score to the data frame ----

  df[["quality_score"]] <- df |>
    group_by({{ area }}) |>
    get_quality_score(method = "mfaz")

  ## Add quality class to the data frame ----

  df[["quality_class"]] <- df |>
    group_by({{ area }}) |>
    classify_overall_quality()

  df
}

#'
#'Plausibility check: Weight-for-Height z-scores
#'
#'`check_plausibility_whz()` lets you know the quality of your data, based on
#'statistics around WHZ, but without including MUAC, as it is on SMART plausibility
#'report.
#'
#'@param df A data frame object returned by [process_whz_data()].
#'
#'@param sex A vector telling whether a given child is a boy or girl.
#'
#'@param age A vector containing children age in months.
#'
#'@param wgt A vector containing weight measurements in kilograms.
#'
#'@param hgt A vector containing height measurements in centimeters.
#'
#'@param flags A character vector telling whether or not an observation is an
#' outlier.
#'
#'@param area A vector with values on where was the data collected. If you are
#' analyzing a data set with just one area, provide it anyway to `check_plausibility_whz()`
#'
#'@returns A summarized data frame of 18 columns containing quality checks
#' statistics and respective classification. The classification is the same as that
#' of SMART Methodology.
#'
#'@examples
#'quality <- data.01 |>
#'process_age() |>
#'process_whz_data(
#'sex = "sex",
#'wgt = "weight",
#'hgt = "height",
#'.recode_sex = TRUE
#') |>
#'check_plausibility_whz(
#'flags = "flags",
#'sex = "sex",
#'age = "age",
#'wgt = "weight",
#'hgt = "hgt",
#'area = area
#')
#'
#'quality
#'
#'
#'@export
#'
check_plausibility_whz <- function(df, sex, age, wgt, hgt, flags, area) {

  df <- df |>
    group_by({{ area  }}) |>
    summarise(
      n = n(),
      flagged = do.call(sum, list(flags, na.rm = TRUE)) / n(),
      flagged_class = do.call(classify_percent_flagged, list(flagged, method = "whz")),
      sex_ratio = do.call(sexRatioTest, list(sex, code = c(1, 2)))$p,
      sex_ratio_class = do.call(classify_age_sex_ratio, list(sex_ratio)),
      age_ratio = do.call(ageRatioTest, list(age, .expectedP = 0.85))$p,
      age_ratio_class = do.call(classify_age_sex_ratio, list(age_ratio)),
      dps_wgt = digitPreference(wgt, digits = 1, values = 0:9)$dps,
      dps_wgt_class = digitPreference(wgt, digits = 1, values = 0:9)$dpsClass,
      dps_hgt = digitPreference(hgt, digits = 1, values = 0:9)$dps,
      dps_hgt_class = digitPreference(hgt, digits = 1, values = 0:9)$dpsClass,
      sd = do.call(sd, list(do.call(remove_flags, list(wfh)), na.rm = TRUE)),
      sd_class = do.call(classify_sd, list(sd, method = "zscore")),
      skew = do.call(skewKurt, list(do.call(remove_flags, list(wfh))))$s,
      skew_class = do.call(classify_skew_kurt, list(skew)),
      kurt = do.call(skewKurt, list(do.call(remove_flags, list(wfh))))$k,
      kurt_class = do.call(classify_skew_kurt, list(kurt)),
      .groups = "drop"
    )

  ## Add quality score to the data frame ----

  df[["quality_score"]] <- df |>
    group_by({{ area }}) |>
    get_quality_score(method = "mfaz")

  ## Add quality class to the data frame ----

  df[["quality_class"]] <- df |>
    group_by({{ area }}) |>
    classify_overall_quality()

  df
}

#'
#'Plausibility check: Crude MUAC data.
#'
#'`check_plausibility_crude_muac()` lets you know the quality of your crude MUAC
#'data. This should used only when the variables age and or sex are missing from
#'your dataset, as not having them cannot allow you calculate MUAC-for-age
#'zscores as required in [zscorer::addWGSR()].
#'
#'@param df A data frame object returned by [process_muac_data()]
#'
#'@param flags A character vector telling whether or not an observation is an
#' outlier.
#'
#'@param sex A vector telling whether a given child is a boy or girl.
#'
#'@param muac A vector containing MUAC measurements. This is used by
#' [nipnTK::digitPreference].
#'
#'@param area A vector with values on where was the data collected. If you are
#' analyzing a data set with just one area, provide it anyway to
#' `check_plausibility_crude_muac()`
#'
#'@examples
#'
#'quality <- data.01 |>
#'process_muac_data(
#'sex = "sex",
#'muac = "muac",
#'.recode_sex = TRUE,
#'.recode_muac = FALSE,
#'unit = "none"
#') |>
#'check_plausibility_crude_muac()
#'
#'## View object ----
#'quality
#'
#'@export
#'
check_plausibility_crude_muac <- function(df, flags, sex, muac, area) {

  df <- df |>
    group_by({{ area }}) |>
    summarise(
      n = n(),
      flagged = do.call(sum, list(flags, na.rm = TRUE)) / n(),
      flagged_class = do.call(classify_percent_flagged,
                              list(flagged, method = "crude")),
      sex_ratio = do.call(sexRatioTest, list(sex, code = c(1, 2)))[["p"]],
      sex_ratio_class = do.call(classify_age_sex_ratio, list(sex_ratio)),
      dps = digitPreference(muac, digits = 0, values = 0:9)[["dps"]],
      dps_class = digitPreference(muac, digits = 0, values = 0:9)[["dpsClass"]],
      sd = do.call(sd, list(do.call(remove_flags, list(muac)), na.rm = TRUE)),
      sd_class = do.call(classify_sd, list(sd, method = "crude")),
      .groups = "drop"
    )
  df
}
