#'
#' Compute a MUAC-for-age z-score based prevalence estimates of data collected from a two-stage
#' cluster survey sample design, with the first stage sampling done with Probability
#' Proportional to the size of population
#'
#' @description
#' Create a survey design object using the [srvyr::as_survey_design()] and then calculate
#'  the survey means as well the sum of positive cases.
#'
#' @param df A data frame object returned by [process_muac_data()].
#'  this will contain the wrangled vectors that are read inside the function.
#'
#' @param .wt A numeric vector containing survey weights. If set to NULL (default) and
#'  the function will assume self weighted, like in ENA for SMART, otherwise if given, the
#'  weighted analysis will be computed with weighted population returned.
#'
#' @param .edema A character vector containing child's status on edema with "n" for no
#'  edema, "y" = yes edema. Should you data be coded differently, re-code it to aforementioned
#'  codes.
#'
#' @param .summary_by A character vector containing data on the geographical areas where
#'  the data was collected. This is to group the survey design object into different
#'  geographical areas in the data and allow for summaries to be computed for each of them.
#'
#'  @returns A tibble of size depending on the number of groups of the vector given to
#'  `.summary_by` or if set to NULL, and of length 17.
#'
#'
compute_pps_based_mfaz_prevalence <- function(df,
                                              .wt = NULL,
                                              .edema = NULL,
                                              .summary_by) {

  ## Add acute malnutrition case-definitions to the data frame ----
  df <- with(
    df,
    define_wasting(
      df,
      zscore = .data$mfaz,
      edema = {{ .edema }},
      base = "wfhz"
    )
  )
  ## Create a survey object ----
  if (!is.null(.wt)) {
    srvy <- df |>
      as_survey_design(
        ids = .data$cluster,
        pps = "brewer",
        variance = "YG",
        weights = {{ .wt }}
      )
  } else {
    srvy <- df |>
      as_survey_design(
        ids = .data$cluster,
        pps = "brewer",
        variance = "YG"
      )
  }

  ## Summarise prevalence ----
  p <- srvy |>
    group_by({{ .summary_by }}) |>
    filter(.data$flag_mfaz == 0) |>
    summarise(
      across(
        c(.data$gam:.data$mam),
        list(
          n = \(.)sum(., na.rm = TRUE),
          p = \(.)survey_mean(.,
                 vartype = "ci",
                   level = 0.95,
                    deff = TRUE,
                  na.rm = TRUE
          )
        )
      ),
      wt_pop = round(sum(srvyr::cur_svy_wts()))
    )
  p
}

#'
#' Compute acute malnutrition prevalence based on MUAC-for-age z-scores (MFAZ)
#'
#' @description
#' `compute_mfaz_prevalence()` is a handy function designed to dynamically compute acute
#' malnutrition's prevalence using WFHZ. Under the hood, it first checks the status of
#' WFHZ's standard deviation (SD) after removing flags, and then it decides on the
#' appropriate prevalence analysis approach to follow: if SD is anything between excellent
#' and acceptable, a complex sample-based prevalence analysis (for a two-stage  PPS
#' cluster sampling) is computed, otherwise, a re-calculated prevalence using PROBIT method
#' with a sample mean and a SD = 1 is computed. On the former analysis approach, the function
#' was also designed to work around survey weights.
#' The function also super handy to work on large data sets with multiple survey areas. For
#' this, the aforementioned conditionals are checked for each survey area in a summarized
#' data frame and prevalence get computed according to each row's scenario.
#'
#' @param df A data frame object returned by [process_muac_data()].
#'
#' @param .wt A numeric vector containing survey weights. If set to NULL (default) and
#' the function will assume self weighted, like in ENA for SMART, otherwise if given, the
#' weighted analysis will be computed with weighted population returned.
#'
#' @param .edema A character vector containing child's status on edema with "n" for no
#' edema, "y" = yes edema. Should you data be coded differently, re-code it to aforementioned
#' codes.
#'
#' @param .summary_by A character vector containing data on the geographical areas where
#' the data was collected. If you are working on a single survey data, set
#' .summary_by = NULL (default).
#'
#' @returns A tibble. The length vary depending on .summary_by. If set to NULL, a tibble of
#' 1 x 16 is returned, otherwise, a tibble of n rows (depending on the number of geographical
#' areas in the data set) x 17.
#'
#' @examples
#'
#' ## When .summary_by = NULL ----
#' compute_mfaz_prevalence(
#' df = anthro.04,
#' .wt = NULL,
#' .edema = edema,
#' .summary_by = NULL
#' )
#'
#' ## When .summary_by is not set to NULL ----
#' compute_mfaz_prevalence(
#' df = anthro.04,
#' .wt = NULL,
#' .edema = edema,
#' .summary_by = province
#' )
#'
#' ## When a weighted analysis is needed ----
#' ### This example uses a different data set with survey weights ----
#' compute_mfaz_prevalence(
#' df = anthro.02,
#' .wt = "wtfactor",
#' .edema = edema,
#' .summary_by = province
#' )
#'
#' @export
#'
compute_mfaz_prevalence <- function(df,
                                    .wt = NULL,
                                    .edema = NULL,
                                    .summary_by = NULL) {

  ## Difuse argument .summary_by ----
  .summary_by <- rlang::enquo(.summary_by)

  ## An empty vector type list ----
  results <- list()

  if (!rlang::quo_is_null(.summary_by)) {
    ## Grouped summary of standard deviation classification ----
    x <- summarise(
      df,
      std = classify_sd(sd(remove_flags(.data$mfaz, "zscore"), na.rm = TRUE)),
      .by = !!.summary_by
    )
  } else {
    ## Non-grouped summary ----
    x <- summarise(
      df,
      std = classify_sd(sd(remove_flags(.data$mfaz, "zscore"), na.rm = TRUE))
    )
  }

  ## Iterate over data frame to compute prevalence according to the SD ----
  for (i in seq_len(nrow(x))) {
    if (!rlang::quo_is_null(.summary_by)) {
      area <- dplyr::pull(x, !!.summary_by)[i]
      data <- filter(df, !!sym(rlang::quo_name(.summary_by)) == !!area)
    } else {
      data <- df
    }

    std <- x$std[i]
    if (std != "Problematic") {
      ### Compute standard complex sample based prevalence analysis ----
      result <- compute_pps_based_mfaz_prevalence(data, {{ .wt }}, {{ .edema }}, !!.summary_by)
    } else {
      ### Compute grouped PROBIT based prevalence ----
      if (!rlang::quo_is_null(.summary_by)) {
        result <- compute_probit_prevalence(data, !!.summary_by, .for = "mfaz")
      } else {
        ### Compute non-grouped PROBIT based prevalence ----
        result <- compute_probit_prevalence(data, .for = "mfaz")
      }
    }
    results[[i]] <- result
  }
  dplyr::bind_rows(results) |>
    dplyr::relocate(.data$gam_p, .after = .data$gam_n) |>
    dplyr::relocate(.data$sam_p, .after = .data$sam_n) |>
    dplyr::relocate(.data$mam_p, .after = .data$mam_n)
}

