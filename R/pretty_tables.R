#' Get a prettified formatted and presentable output table
#'
#' You may want to share the plausibility report in a table. You usually care for
#' a well formatted and pretty table, with values rounded, scientific notations
#' converted into conventional notations, etc. `generate_pretty_table_mfaz()`,
#' `generate_pretty_table_whz()` and `generate_pretty_table_muac()` does that
#' for you so you already.
#'
#' @param df An output data frame returned by [check_plausibility_mfaz()],
#' [check_plausibility_whz()] or [check_plausibility_muac()].
#'
#' @returns An output data frame of the same size as the input, but with values
#' formatted, columns renamed, and ready to share.
#'
#' @examples
#'
#' anthro_data |>
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
#' flags = flags,
#' sex = sex,
#' muac = muac,
#' age = age,
#' area = area
#' ) |>
#' generate_pretty_table_mfaz()
#'
#' anthro_data |>
#' process_muac_data(
#' sex = sex,
#' muac = muac,
#' age = NULL,
#' .recode_sex = TRUE,
#' .recode_muac = FALSE,
#' unit = "none"
#' ) |>
#' check_plausibility_muac(
#' flags = flags,
#' sex = sex,
#' muac = muac
#' ) |>
#' generate_pretty_table_muac()
#'
#' @rdname pretty_table
#'
#' @export
#'
#'
generate_pretty_table_mfaz <- function(df) {

  ## Format data frame ----
  df <- df |>
    mutate(
      flagged = .data$flagged |>
        label_percent(accuracy = 0.1, suffix = "%", decimal.mark = ".")(),
      sex_ratio = .data$sex_ratio |>
        label_pvalue()(),
      age_ratio = .data$age_ratio |>
        label_pvalue()(),
      sd = round(.data$sd, digits = 2),
      dps = round(.data$dps),
      skew = round(.data$skew, digits = 2),
      kurt = round(.data$kurt, digits = 2)
    ) |>
    ## Rename columns ----
    rename(
      `Flagged data (%)` = .data$flagged,
      `Class. of flagged data` = .data$flagged_class,
      `Sex ratio (p)` = .data$sex_ratio,
      `Class. of sex ratio` = .data$sex_ratio_class,
      `Age ratio (p)` = .data$age_ratio,
      `Class. of age ratio` = .data$age_ratio_class,
      `DPS (#)` = .data$dps,
      `Class. of DPS` = .data$dps_class,
      `Standard Dev* (#)` = .data$sd,
      `Class. of standard dev` = .data$sd_class,
      `Skewness* (#)` = .data$skew,
      `Class. of skewness` = .data$skew_class,
      `Kurtosis* (#)` = .data$kurt,
      `Class. of kurtosis` = .data$kurt_class,
      `Overall score` = .data$quality_score,
      `Overall quality` = .data$quality_class
    )
  ## Return data frame ----
  df
}


#'
#'
#' @rdname pretty_table
#'
#' @export
#'
#'
generate_pretty_table_whz <- function(df) {

  ## Format data frame ----
    df <- df |>
      mutate(
        flagged = .data$flagged |>
          label_percent(accuracy = 0.1, suffix = "%", decimal.mark = ".")(),
        sex_ratio = .data$sex_ratio |>
          label_pvalue()(),
        age_ratio = .data$age_ratio |>
          label_pvalue()(),
        sd = round(.data$sd, digits = 2),
        dps_wgt = round(.data$dps_wgt),
        dps_hgt = round(.data$dps_hgt),
        skew = round(.data$skew, digits = 2),
        kurt = round(.data$kurt, digits = 2)
      ) |>
      ## Rename columns ----
      rename(
        `Flagged data (%)` = .data$flagged,
        `Class. of flagged data` = .data$flagged_class,
        `Sex ratio (p)` = .data$sex_ratio,
        `Class. of sex ratio` = .data$sex_ratio_class,
        `Age ratio (p)` = .data$age_ratio,
        `Class. of age ratio` = .data$age_ratio_class,
        `DPS weight (#)` = .data$dps_wgt,
        `Class. DPS weight` = .data$dps_wgt_class,
        `DPS height (#)` = .data$dps_hgt,
        `Class. DPS height` = .data$dps_hgt_class,
        `Standard Dev* (#)` = .data$sd,
        `Class. of standard dev` = .data$sd_class,
        `Skewness* (#)` = .data$skew,
        `Class. of skewness` = .data$skew_class,
        `Kurtosis* (#)` = .data$kurt,
        `Class. of kurtosis` = .data$kurt_class,
        `Overall score` = .data$quality_score,
        `Overall quality` = .data$quality_class
      )
    ## Return data frame ----
  df
}


#'
#'
#' @rdname pretty_table
#'
#' @export
#'
generate_pretty_table_muac <- function(df) {

  ## Format data frame ----
    df <- df |>
      mutate(
        flagged = .data$flagged |>
          label_percent(accuracy = 0.1, suffix = "%", decimal.mark = ".")(),
        sex_ratio = .data$sex_ratio  |>  scales::label_pvalue()(),
        sd = round(.data$sd, digits = 2),
        dps = round(.data$dps)
      ) |>
      ## Rename columns ----
      rename(
        `Flagged data (%)` = .data$flagged,
        `Class. of flagged data` = .data$flagged_class,
        `Sex ratio (p)` = .data$sex_ratio,
        `Class. of sex ratio` = .data$sex_ratio_class,
        `DPS (#)` = .data$dps,
        `Class. of DPS` = .data$dps_class,
        `Standard Dev* (#)` = .data$sd,
        `Class. of standard dev` = .data$sd_class
      )

    ## Return data frame ----
    df
}
