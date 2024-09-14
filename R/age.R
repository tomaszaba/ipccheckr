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
  tibble::as_tibble(df)
}

#'
#' Age ratio test on children aged 6:23 over 24:59 months
#'
#' @description
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
#' children aged 6-23 months, considered to be of 0.66 according to the
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
