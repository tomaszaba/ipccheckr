#'
#' Identify and flag outliers in WHZ, MFAZ, and crude MUAC datasets
#'
#' Outliers are extreme values that far away from the mean, that are unlikely to
#' be correct measurements. `flag_outliers()` helps you to identify any extreme
#' values in your dataset in two different ways. Outliers in WHZ are identified
#' based on the [SMART Methodology.](https://smartmethodology.org/).
#' MFAZ follows the same approach, while crude MUAC's approach is based on a
#' fixed range (<100mm and >200mm), based a multicountry research findings by
#' [Bilukha, O., & Kianian, B. (2023).](https://doi.org/10.1111/mcn.13478)
#'
#' @param x A numeric value from the variable storing either WHZ or MFAZ or crude
#' MUAC's observations in the dataset, as applicable.
#'
#' @param type The method you wish `flag_outliers()` to identify flags on.
#' A choice between "zscore" and "crude". If you wish to get flags for WHZ or
#' MFAZ, set `method = "zscore"`. Alternatively, if your wish to get flags for
#' crude MUAC, set `method = "crude"`. The default is "zscore". If by mistake
#' a different option is supplied, an error will be thrown with a message
#' guiding you what to do.
#'
#' @return A vector of two values: 1 and 0, where 1 signifies flagged value and
#' 0 not flagged.
#'
#' @examples
#'
#' # Sample data of crude MUAC ----
#' x <- c(90, 110, 140, 200, 119, 235)
#'
#' # Apply `flag_outliers()` with type set to "crude" ----
#' flag_outliers(x, type = "crude")
#'
#' # Sample data of MFAZ ----
#' x <- c(-2.265, -5.275, -0.72, -2.261, -2.264, -4.451, -2.261, -1.828)
#'
#' # Apply `flag_outliers()` with type set to "zscore" ----
#' flag_outliers(x, type = "zscore")
#'
#' @export
#'
flag_outliers <- function(x, type = c("zscore", "crude")) {
  type <- match.arg(type)

  if (type == "zscore") {
    mean_zscore <- mean(x, na.rm = TRUE)
    flags <- ifelse((x < (mean_zscore - 3) | x > (mean_zscore + 3)), 1, 0)
    flags <- ifelse(is.na(x), NA, flags)
    flags

  } else {
    flags <- ifelse(x < 100 | x > 200, 1, 0)
    flags <- ifelse(is.na(x), NA, flags)
    flags
  }
}

#'
#' Recode age variable from months to days
#'
#' @param x A numeric vector containing values of age in months.
#'
#' @returns A numeric vector with values corresponding to age in days
#'
compute_month_to_days <- function(x) {
  x * (365.25 / 12)
}

#'
#' Get age in months from birth-date and the data when data was collected.
#'
#' `compute_age_in_months()` works inside [dplyr::mutate()] or [base::transform()]
#' It helps you to compute age in months from a pair of birth date and survey date.
#'
#' @param surv_date,birth_date Vectors containing dates. `surv_date` refers to the day,
#' month and year when the data was collected; while `birth_date` refers to the date
#' when the child was born.
#'
#' @returns A vector of name `age` storing age in months, a mix of double and
#' integer and `NA` for missing value if any of the processed age in months is
#' < 6 or > 59.99 months.
#'
#'
compute_age_in_months <- function (surv_date, birth_date) {
  avg_day <- 365.25 / 12
  int <- surv_date - birth_date
  age_mo <- round(int / avg_day, digits = 2)
  age_mo <- ifelse(age_mo < 6.0 | age_mo >= 60.0, NA, age_mo)
}

#'
#' Transform age in months and age in days with a data frame
#'
#' `process_age()` helps you get the variable age in the right format and ready
#' to be used for downstream workflow, i.e., get z-scores, as well as exclude
#' age values that are out-of-range.
#'
#' @param df The input data frame.
#'
#' @param svdate,birdate Vectors containing dates. `svdate` refers to the day, month
#' and year when the data was collected; while `birdate` refers to the date when the
#' child was born (birth-date). By default, both arguments are `NULL`. This is
#' makes `process_age()` work even in data sets where either survey date or birth-
#' data is not available, so the `process_age()` works on already given age variable.
#'
#' @param age A numeric vector containing already given age in months, usually an
#' integer in the input data as it is estimated using local event calendars.
#' `age` will typically be available on a particular row when `birth_date` of
#' that same row is missing.
#'
#' @returns A data frame of the same length as the input data frame, but of a
#' different width. If `svdate` or `birdate` are available, two new vectors are added
#' to the data frame: `age` in months with two decimal places and `age_day` which
#' is age in days with decimal two decimal places.
#'
#' @examples
#'
#' # Have a sample data ----
#' df <- data.frame(
#' survy_date = as.Date(c(
#' "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01")),
#' birthdate = as.Date(c(
#' "2019-01-01", NA, "2018-03-20", "2019-11-05", "2021-04-25")),
#' age = c(NA, 36, NA, NA, NA)
#' )
#'
#' ## Apply function ----
#' df |>
#' process_age(svdate = "survy_date", birdate = "birthdate", age = age)
#'
#' @export
#'

process_age <- function(df, svdate = NULL, birdate = NULL, age) {
  if (!is.null({{ birdate }}) || !is.null({{ svdate }})) {
    df <- df |>
      mutate(
        age = ifelse(
          is.na({{ age }}),
          compute_age_in_months(
            birth_date = !!sym({{ birdate }}), surv_date = !!sym({{ svdate }})
          ), {{ age }}),
        age_days = round(age * 30.44, 2)
      )

  } else {
    df <- df |>
      mutate(
        age_days = round({{ age }} * 30.44, 2)
      )
  }
  df
}

#'
#' Recode crude MUAC variable into either centimeters or millimeters
#'
#' Sometimes, a vector containing MUAC values may be in centimeters or in
#' millimeters. You may want to get in the right format to use with
#' [zscorer::addWGSR] or [nipnTK::digitPreference()]. `recode_muac()` helps you
#' getting the vector in the right format for the job! It works inside works
#' inside [dplyr::mutate()] or [base::transform()].
#'
#' @param muac A numeric vector storing values for MUAC that can be in centimeters
#' or in millimeters.
#'
#' @param unit A choice of the units to which you wish to convert your MUAC
#' variable into.
#'
#' @returns A transformed vector into the unit you wish to have.
#'
recode_muac <- function(muac, unit = c("cm", "mm")) {

  ## Check if unit's arguments match ----
  stopifnot(unit %in% c("cm", "mm"))

  ## Recode muac conditionally ----
  switch(
    unit,

    ### Recode to millimeters ----
    "mm" = {
      muac <- muac * 10
      },

    ### Recode to centimeters ----
    "cm" = {
      muac <- muac / 10
      },

    stop("Invalid 'units' argument. Please choose either 'cm' or 'mm'.")
  )
}

#'
#' Process MUAC data
#'
#' `process_muac_data()` gets your input data ready for downstream MUAC related
#' analysis.
#'
#' @param df The input data frame with variables sex, age and MUAC.
#'
#' @param sex A vector storing values about whether the child is a boy or a girl.
#'
#' @param muac A vector storing crude MUAC values.
#'
#' @param age A vector storing values about child's age in months.
#'
#' @param .recode_sex Logical. It asks whether you should recode your sex variable
#' to the required shape to use in `process_muac_data()`. The default values for
#' sex are 1 for boys and 2 for girls. Setting `.recode_sex = TRUE` works on "m"
#' and "f" values. If your vector is in any different shape, you should put it in
#' "m" and "f" or right away to 1 or 2. If you are using data exported from ENA for
#' SMART software, then you should leave `.recode_sex` at its default: `TRUE`.
#'
#' @param .recode_muac Logical. Choose between `TRUE` or `FALSE` if you wish or
#' not to recode the MUAC variable into the required format to work on.
#'
#' @param unit A choice of the units to which you wish to convert your MUAC
#' variable into.
#'
#' @returns A data frame of the same length as the input data, but with a
#' different width as explained:When `age` is available in the input data and
#' supplied, `process_muac_data` will return as output a data frame with two
#' new variables `mfaz` and `flags`. `mfaz` stores MUAC-for-age z-score (MFAZ)
#' values and `flags` tells you whether a given z-score is an outlier or not.
#' This job is done by [flag_outliers()]. If age is not available in the input
#' data, therefore not possible to supply in this function, `process_muac_data`
#' will only return `flags`. This will refer to flags based on crude MUAC.
#'
#' @examples
#'
#' ## Have a sample data ----
#'
#' df <- data.frame(
#'  survey_date = as.Date(c(
#'  "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01")),
#'  birthdate = as.Date(c(
#'  "2019-01-01", NA, "2018-03-20", "2019-11-05", "2021-04-25")),
#'  age = c(NA, 36, NA, NA, NA),
#'  sex = c("m", "f", "m", "m", "f"),
#'  muac = c(110, 130, 300, 123, 125)
#'  )
#'
#'  ## Apply function ----
#'  df |>
#'  process_age(
#'  svdate = "survey_date",
#'  birdate = "birthdate",
#'  age = age
#'  ) |>
#'  process_muac_data(
#'  sex = sex,
#'  age = "age",
#'  muac = muac,
#'  .recode_sex = TRUE,
#'  .recode_muac = TRUE,
#'  unit = "cm"
#'  )
#'
#'
#' @export
#'
process_muac_data <- function(df,
                              sex, muac, age = NULL,
                              .recode_sex = TRUE,
                              .recode_muac = TRUE,
                              unit = c("cm", "mm", "none")) {
  unit <- match.arg(unit)

  recode_sex <- quote(
    if (.recode_sex) {
      sex <- ifelse({{ sex }} == "m", 1, 2)
    } else {
      {{ sex }}
    }
  )

  rec_muac <- quote(
    if (.recode_muac && unit == "cm") {
      muac <- recode_muac({{ muac }}, unit = "cm")
    } else if (.recode_muac && unit == "mm") {
      muac <- recode_muac({{ muac }}, unit = "mm")
    } else {
      {{ muac }}
    }
  )

  if (!is.null({{ age }})) {
    df <- df |>
      mutate(
        muac = !!rec_muac,
        sex = !!recode_sex,
      ) |>
      addWGSR(
        sex = "sex",
        firstPart = "muac",
        secondPart = "age_days",
        index = "mfa",
        digits = 3
      )|>
      mutate(
        flags = do.call(flag_outliers, list(.data$mfaz, type = "zscore"))
      )
  } else {
    df <- df |>
      mutate(
        sex = !!recode_sex,
        flags = do.call(flag_outliers, list({{ muac }}, type = "crude"))
      )
  }
 df
}

#'
#' Process Weight-for-Height data
#'
#' `process_whz_data()` gets your input data ready for downstream WHZ related
#' analysis.
#'
#' @param df The input data frame with variables sex, age and MUAC.
#'
#' @param sex A vector storing values about whether the child is a boy or a girl.
#'
#' @param weight,height Vectors storing weight values in kilograms and height
#' values in centimeters, respectively.
#'
#' @param .recode_sex Logical. It asks whether you should recode your sex variable
#' to the required shape to use in `process_whz_data()`. The default values for
#' sex are 1 = boys and 2 = girls. Setting `.recode_sex = TRUE` works on "m"
#' and "f" values. If your vector is in any different shape, you should put it in
#' "m" and "f" or right away to 1 or 2. If you are using data exported from ENA for
#' SMART software, then you should leave `.recode_sex` at its default: `TRUE`.
#'
#' @returns A data frame of the same length as the input data, but with a different
#' width: two new variables `wfhz` and `flags`. `wfhz` stores weight-for-height
#' z-score values with three decimal places. `flags` tells you whether a given
#' z-score is an outlier or not. This job is done by [flag_outliers()].
#'
#' @examples
#' ## Have a sample data ----
#' anthro_data |>
#' process_whz_data(
#' sex = sex,
#' weight = weight,
#' height = height,
#' .recode_sex = TRUE
#' )
#'
#' @export
#'
process_whz_data <- function(df, sex, weight, height, .recode_sex = TRUE) {

  recode_sex <- quote(
    if (.recode_sex) {
      sex <- ifelse({{ sex }} == "m", 1, 2)
    } else {
      {{ sex }}
    }
  )

    df <- df |>
      mutate(
        sex = !!recode_sex
      ) |>
      addWGSR(
        sex = "sex",
        firstPart = "weight",
        secondPart = "height",
        index = "wfh",
        digits = 3
      ) |>
      mutate(
        flags = do.call(flag_outliers, list(.data$wfhz, type = "zscore"))
      )
    df
}

#'
#' Age ratio test
#'
#' @description
#'
#' As documented in [nipnTK::ageRatioTest()], age ratio test is an age-related
#' test of survey data quality. This includes other assessments as screenings,
#' sentinel sites, etc. Different to [nipnTK::ageRatioTest()], in `age_ratio_test()`
#' the ratio of children is calculate from children 6-23 months to the number of
#' children age 24-59 months. The ratio is then compared to the expected ratio
#' (set at 0.66). Then the difference between the observed ratio is compared to
#' the expected using a Chi-squared test.
#'
#' `age_ratio_test()` should only be used for MUAC checks. This particularly
#' useful as allows you to determine if downstream your analysis you should
#' consider adjusting your MUAC prevalence, should there be more younger children
#' than older children in your survey, screening or sentinel site data. If you
#' wish to get the age ratio for children 6-29/30-59 like in SMART Methodology,
#' then you should use [nipnTK::ageRatioTest()] NOT `age_ratio_test()`.
#'
#' @param age A vector storing values about child's age in months.
#'
#' @param .expectedP The expected proportion of children aged 24-59 months over
#' children aged 6-29 months, considered to be of 0.66 according to the
#' [SMART MUAC tool](https://smartmethodology.org/survey-planning-tools/updated-muac-tool/).
#'
#' @returns A list three statistics: `p` for p-value, `observedR` for observed ratio
#' from your data, `observedP` for observed proportion of children 24-59 months
#' over the universe of your sample data.
#'
#' @examples
#'
#' ## Have a sample data ----
#' age <- seq(6,59) |> sample(300, replace = TRUE)
#'
#' ## Apply the function ----
#' age_ratio_test(age, .expectedP = 0.66)
#'
#' @export
#'
age_ratio_test <- function(age, .expectedP = 0.66) {

  x <- ifelse(age >= 24, 1, 2)
  sum_o24 <- sum(na.omit(x == 1))
  sum_u24 <- sum(na.omit(x == 2))
  total <- sum(table(na.omit(x)))
  ratio <- sum_o24 / sum_u24
  prop <- sum_o24 / total
  test <- prop.test(sum_o24, total, p = .expectedP, correct = FALSE)

    list(
      p = test$p.value,
      observedR = ratio,
      observedP = prop
    )
}

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
