#'Get a beatified and presentable output table
#'
#'You may want to share the plausibility report in a table. You usually care for
#'a well formatted and beauty table, with values rounded, scientific notations
#'converted into conventional notations, etc. `return_pretty_ouput_mfaz()` does
#'that for you so you already.
#'
#'@param df An output data frame returned by [check_plausibility_mfaz()].
#'
#'@returns An output data frame of the same size as the input, but with values
#'formatted, columns renamed, and ready to share.
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
#') |>
#'return_pretty_ouput_mfaz()
#'
#'@export
#'
#'
return_pretty_ouput_mfaz <- function(df){

return(
  df |>
    mutate(
      flagged = flagged |>
        label_percent(accuracy = 0.1, suffix = "%", decimal.mark = ".")(),
      sex_ratio = sex_ratio |>
        label_pvalue()(),
      age_ratio = age_ratio |>
        label_pvalue()(),
      sd = round(sd, digits = 2),
      dps = round(dps),
      skew = round(skew, digits = 2),
      kurt = round(kurt, digits = 2)
    ) |>
    rename(
      `Flagged data (%)` = flagged,
      `Class. of flagged data` = flagged_class,
      `Sex ratio (p)` = sex_ratio,
      `Class. of sex ratio` = sex_ratio_class,
      `Age ratio (p)` = age_ratio,
      `Class. of age ratio` = age_ratio_class,
      `DPS (#)` = dps,
      `Class. of DPS` = dps_class,
      `Standard Dev* (#)` = sd,
      `Class. of standard dev` = sd_class,
      `Skewness* (#)` = skew,
      `Class. of skewness` = skew_class,
      `Kurtosis* (#)` = kurt,
      `Class. of kurtosis` = kurt_class,
      `Overall score` = quality_score,
      `Overall quality` = quality_class
    )
)
}

#'Get a beatified and presentable output table
#'
#'You may want to share the plausibility report in a table. You usually care for
#'a well formatted and beauty table, with values rounded, scientific notations
#'converted into conventional notations, etc. `return_pretty_ouput_whz()` does
#'that for you so you already.
#'
#'@param df An output data frame returned by [check_plausibility_whz()].
#'
#'@returns An output data frame of the same size as the input, but with values
#'formatted, columns renamed, and ready to share.
#'
#'@examples
#'
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
#') |>
#'return_pretty_ouput_whz()
#'
#'@export
#'
#'
return_pretty_ouput_whz <- function(df){

  return(
    df |>
      mutate(
        flagged = flagged |>
          label_percent(accuracy = 0.1, suffix = "%", decimal.mark = ".")(),
        sex_ratio = sex_ratio |>
          label_pvalue()(),
        age_ratio = age_ratio |>
          label_pvalue()(),
        sd = round(sd, digits = 2),
        dps_wgt = round(dps_wgt),
        dps_hgt = round(dps_hgt),
        skew = round(skew, digits = 2),
        kurt = round(kurt, digits = 2)
      ) |>
      rename(
        `Flagged data (%)` = flagged,
        `Class. of flagged data` = flagged_class,
        `Sex ratio (p)` = sex_ratio,
        `Class. of sex ratio` = sex_ratio_class,
        `Age ratio (p)` = age_ratio,
        `Class. of age ratio` = age_ratio_class,
        `DPS weight (#)` = dps_wgt,
        `Class. DPS weight` = dps_wgt_class,
        `DPS height (#)` = dps_hgt,
        `Class. DPS height` = dps_hgt_class,
        `Standard Dev* (#)` = sd,
        `Class. of standard dev` = sd_class,
        `Skewness* (#)` = skew,
        `Class. of skewness` = skew_class,
        `Kurtosis* (#)` = kurt,
        `Class. of kurtosis` = kurt_class,
        `Overall score` = quality_score,
        `Overall quality` = quality_class
      )
  )
}

#'Get a beatified and presentable output table
#'
#'You may want to share the plausibility report in a table. You usually care for
#'a well formatted and beauty table, with values rounded, scientific notations
#'converted into conventional notations, etc. `return_pretty_ouput_crude_muac()` does
#'that for you so you already.
#'
#'@param df An output data frame returned by [check_plausibility_crude_muac()].
#'
#'@returns An output data frame of the same size as the input, but with values
#'formatted, columns renamed, and ready to share.
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
#'check_plausibility_crude_muac() |>
#'return_pretty_ouput_crude_muac()
#'
#'## View output ----
#'quality
#'
#'@export
#'
return_pretty_ouput_crude_muac <- function(df) {

  return(
    df |>
      mutate(
        flagged = flagged |>
          label_percent(accuracy = 0.1, suffix = "%", decimal.mark = ".")(),
        sex_ratio = sex_ratio  |>  scales::label_pvalue()(),
        sd = round(sd, digits = 2),
        dps = round(dps)
      ) |>
      rename(
        `Flagged data (%)` = flagged,
        `Class. of flagged data` = flagged_class,
        `Sex ratio (p)` = sex_ratio,
        `Class. of sex ratio` = sex_ratio_class,
        `DPS (#)` = dps,
        `Class. of DPS` = dps_class,
        `Standard Dev* (#)` = sd,
        `Class. of standard dev` = sd_class
      )
  )
}
