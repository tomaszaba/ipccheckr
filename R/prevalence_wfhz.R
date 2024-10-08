#'
#' Compute a weight-for-height based prevalence estimates of data collected from a two-stage
#' cluster survey sample design, with the first stage sampling done with Probability
#' Proportional to the size of population
#'
#' @description
#' Create a survey design object using the [srvyr::as_survey_design()] and then calculate
#'  the survey means as well the sum of positive cases.
#'
#' @param df A data frame object returned by [process_wfhz_data()].
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
compute_pps_based_wfhz_prevalence <- function(df,
                                              .wt = NULL,
                                              .edema = NULL,
                                              .summary_by) {

  ## Add acute malnutrition case-definitions to the data frame ----
  df <- with(
    df,
    define_wasting(
      df,
      zscore = .data$wfhz,
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
    filter(.data$flag_wfhz == 0) |>
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
#'
#' Compute global, severe and moderate acute malnutrition prevalence using PROBIT approach.
#'
#' @description
#' This approach is only applied for when WFHZ standard deviation's is problematic. The
#' PROBIT approach estimates the prevalence of acute malnutrition indirectly by computing
#' the area under the tail of the curve from negative infinitive to the given threshold
#' through the cumulative normal distribution function using the mean and standard deviation.
#'
#' @param x A double vector containing the z-score values
#' @param .status A choice on the nutritional status you wish to apply the PROBIT approach
#'  on. Default is "gam" for global acute malnutrition.
#'
#' @returns A numeric value (double) corresponding to the point prevalence estimate.
#'
#'
apply_probit_approach <- function(x, .status = c("gam", "sam")) {
  .status <- match.arg(.status)
  mean <- mean(remove_flags(x, "zscore"), na.rm = TRUE)
  ## Return GAM and SAM prevalence with a SD = 1
  switch(
    .status,
    "gam" = {pnorm(q = -2, mean = mean, sd = 1, lower.tail = TRUE, log.p = FALSE)},
    "sam" = {pnorm(q = -3, mean = mean, sd = 1, lower.tail = TRUE, log.p = FALSE)}
  )
}


#'
#'
#' Compute global, severe and moderate acute malnutrition prevalence using PROBIT approach
#'
#' @description
#' This function is a helper function used inside [compute_wfhz_prevalence()] and
#' [compute_mfaz_prevalence()]. It is used to compute PROBIT based prevalence depending
#' on the status of standard deviation. For more details, check the documentation of the
#' aforementioned functions.
#'
#' @param df A data frame object returned by [process_wfhz_data()] or by [process_muac_data()]
#' They will contain the wrangled vectors that are read inside the function.
#'
#' @param .summary_by A character vector containing data on the geographical areas where
#'  the data was collected. This is to group the survey design object into different
#'  geographical areas in the data and allow for summaries to be computed for each of them.
#'  Default is NULL.
#'
#' @param .for A choice between "wfhz" and "mfaz" for the anthropometric index you wish
#'  to compute PROBIT prevalence on.
#'
#' @returns A tibble with the PROBIT based point prevalence for global, severe and moderate
#'  acute malnutrition.
#'
#'
compute_probit_prevalence <- function(df,
                                      .summary_by = NULL,
                                      .for = c("wfhz", "mfaz")) {
  ## Difuse argument ----
  .summary_by <- rlang::enquo(.summary_by)
  ## Match argument ----
  .for <- match.arg(.for)

  switch(
    .for,
    "wfhz" = {
      if(!is.null(.summary_by)) {
        df <- summarise(
          df,
          gam = apply_probit_approach(.data$wfhz, .status = "gam"),
          sam = apply_probit_approach(.data$wfhz, .status = "sam"),
          mam = .data$gam - .data$sam,
          .by = !!.summary_by
        ) |>
          mutate(
            gam_p = .data$gam, sam_p = .data$sam, mam_p = .data$mam,
            gam = NA, sam = NA, mam = NA
          ) |>
          dplyr::select(!2:4) ## To make it fit in the tibble structure from the main function
      } else {
        df <- summarise(
          df,
          gam = apply_probit_approach(.data$wfhz, .status = "gam"),
          sam = apply_probit_approach(.data$wfhz, .status = "sam"),
          mam = .data$gam - .data$sam
        ) |>
          mutate(
            gam_p = .data$gam, sam_p = .data$sam, mam_p = .data$mam,
            gam = NA, sam = NA, mam = NA
          ) |>
          dplyr::select(!2:4) ## To make it fit in the tibble structure from the main function
      }
      df
    },
    "mfaz" = {
      if(!is.null(.summary_by)) {
        df <- summarise(
          df,
          gam = apply_probit_approach(.data$mfaz, .status = "gam"),
          sam = apply_probit_approach(.data$mfaz, .status = "sam"),
          mam = .data$gam - .data$sam,
          .by = !!.summary_by
        ) |>
          mutate(
            gam_p = .data$gam, sam_p = .data$sam, mam_p = .data$mam,
            gam = NA, sam = NA, mam = NA
          ) |>
          dplyr::select(!2:4) ## To make it fit in the tibble structure from the main function
      } else {
        df <- summarise(
          df,
          gam = apply_probit_approach(.data$mfaz, .status = "gam"),
          sam = apply_probit_approach(.data$mfaz, .status = "sam"),
          mam = .data$gam - .data$sam
        ) |>
          mutate(
            gam_p = .data$gam, sam_p = .data$sam, mam_p = .data$mam,
            gam = NA, sam = NA, mam = NA
          ) |>
          dplyr::select(!2:4) ## To make it fit in the tibble structure from the main function
      }
      df
    }
  )
}

#'
#' Compute acute malnutrition prevalence based on weight-for-height z-scores (WFHZ),
#' MUAC-for-age z-scores (MFAZ), MUAC and combined
#'
#' @description
#' `compute_wfhz_prevalence()` is a handy function designed to dynamically compute acute
#' malnutrition's prevalence using WFHZ. Under the hood, it first checks the status of
#' WFHZ's standard deviation (SD) after removing flags, and then it decides on the
#' appropriate prevalence analysis approach to follow: if SD is anything between excellent
#' and acceptable, a complex sample-based prevalence analysis (for a two-stage  PPS
#' cluster sampling) is computed, otherwise, a re-calculated prevalence using PROBIT method
#' with a sample mean and a SD = 1 is computed. On the former analysis approach, the function
#' was also designed to work around survey weights.
#' The function also super handy to work on large data sets with multiple survey areas. For
#' this, the aforementioned conditionals are checked for each survey areas in a summarized
#' data frame and prevalence get computed according to each row's scenario.
#'
#' @param df A data frame object returned by [process_wfhz_data()].
#'
#' @param .wt A numeric vector containing survey weights. If set to NULL (default) and
#' the function will assume self weighted, like in ENA for SMART, otherwise if given, the
#' weighted analysis will be computed with weighted population returned.
#'
#' @param .edema A character vector containing child's status on edema with "n" for no
#' edema, "y" = yes edema. Should you data be coded differently, re-code it to aforementioned
#' codes.
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
#' anthro.03 |>
#' process_wfhz_data(
#' sex = sex,
#' weight = weight,
#' height = height,
#' .recode_sex = TRUE
#' ) |>
#' compute_wfhz_prevalence(
#' .wt = NULL,
#' .edema = edema,
#' .summary_by = NULL
#' )
#'
#' ## When .summary_by is not set to NULL ----
#' anthro.03 |>
#' process_wfhz_data(
#' sex = sex,
#' weight = weight,
#' height = height,
#' .recode_sex = TRUE
#' ) |>
#' compute_wfhz_prevalence(
#' .wt = NULL,
#' .edema = edema,
#' .summary_by = district
#' )
#'
#' ## When a weighted analysis is needed ----
#' anthro.02 |>
#' compute_wfhz_prevalence(
#' .wt = "wtfactor",
#' .edema = edema,
#' .summary_by = province
#' )
#'
#' @export
#'
compute_wfhz_prevalence <- function(df,
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
      std = classify_sd(sd(remove_flags(.data$wfhz, "zscore"), na.rm = TRUE)),
      .by = !!.summary_by
    )
  } else {
    ## Non-grouped summary ----
    x <- summarise(
      df,
      std = classify_sd(sd(remove_flags(.data$wfhz, "zscore"), na.rm = TRUE))
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
      result <- compute_pps_based_wfhz_prevalence(data, {{ .wt }}, {{ .edema }}, !!.summary_by)
    } else {
      ### Compute grouped PROBIT based prevalence ----
      if (!rlang::quo_is_null(.summary_by)) {
        result <- compute_probit_prevalence(data, !!.summary_by, .for = "wfhz")
      } else {
        ### Compute non-grouped PROBIT based prevalence ----
        result <- compute_probit_prevalence(data, .for = "wfhz")
      }
    }
    results[[i]] <- result
  }
  dplyr::bind_rows(results) |>
    dplyr::relocate(.data$gam_p, .after = .data$gam_n) |>
    dplyr::relocate(.data$sam_p, .after = .data$sam_n) |>
    dplyr::relocate(.data$mam_p, .after = .data$mam_n)
}

