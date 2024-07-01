# Function to detect outliers --------------------------------------------------

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

# Function to remove detected outliers -----------------------------------------

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


# Function to recode MUAC variables into desired units -------------------------

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
#' @examples
#' # Have an input data with muac in mm ----
#' muac <- seq(90, 250, by = 4)
#'
#' # Apply recode ----
#' recode_muac(muac, unit = "cm")
#'
#' # Have an input data with muac in mm ----
#' muac <- seq(9.0, 25.0, by = 0.2)
#'
#' # Apply recode ----
#' recode_muac(muac, unit = "mm")
#'
#' @export
#'
recode_muac <- function(muac, unit = c("cm", "mm")) {

  ## Check if unit's arguments match ----
  stopifnot(unit %in% c("cm", "mm"))

  ## Recode muac conditionally ----
  switch(
    unit,
    ### Recode to millimeters ----
    "mm" = {muac <- muac * 10},
    ### Recode to centimeters ----
    "cm" = {muac <- muac / 10},
    stop("Invalid 'units' argument. Please choose either 'cm' or 'mm'.")
  )
}


# Function to process MUAC data ------------------------------------------------

#'
#' Process MUAC data a get it ready for analyses
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
        flag_mfaz = do.call(flag_outliers, list(.data$mfaz, type = "zscore"))
      )
  } else {
    df <- df |>
      mutate(
        sex = !!recode_sex,
        flag_muac = do.call(flag_outliers, list({{ muac }}, type = "crude"))
      )
  }
  dplyr::as_tibble(df)
}


# Function to process Weight-for-height data -----------------------------------

#'
#' Process Weight-for-Height data get it ready for analyses
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
#' anthro.01 |>
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
      sex = {{ "sex" }},
      firstPart = {{ "weight" }},
      secondPart = {{ "height" }},
      index = "wfh",
      digits = 3
    ) |>
    mutate(
      flag_wfhz = do.call(flag_outliers, list(.data$wfhz, type = "zscore"))
    )
  dplyr::as_tibble(df)
}
