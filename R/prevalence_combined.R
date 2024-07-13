#'
#' Compute combined prevalence of acute malnutrition
#'
#' @rdname combined_prevalence
#'
compute_pps_based_combined_prevalence <- function(df,
                                              .wt = NULL,
                                              .edema = NULL,
                                              .summary_by) {
  ## Case definition ----
  df <- with(
    df,
    define_wasting(df,
      zscore = .data$wfhz,
      muac = .data$muac,
      edema = {{ .edema }},
      base = "combined"
    ) |>
      mutate(
        cflags = ifelse(.data$flag_wfhz == 1 | .data$flag_mfaz == 1, 1, 0)
      )
  )
  ## Create survey object ----
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
    filter(.data$cflags == 0) |>
    summarise(
      across(
        c(.data$cgam:.data$cmam),
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
      wt_pop = sum(srvyr::cur_svy_wts())
    )
  p
}



#'
#'
#' Compute combined prevalence of acute malnutrition
#'
#' @description
#' `compute_combined_prevalence()` is handy function to compute the combined prevalence of
#' acute malnutrition using the WFHZ and the absolute values of MUAC and edema for case
#' definition. Under the hood, before prevalence computations begin, it first evaluates the
#' status of WFHZ, MFAZ's standard deviation and age ratio test, as documented in
#' [compute_wfhz_prevalence()] and [compute_muac_prevalence()]. Then, it decides on the
#' appropriate analysis approach to employ depending on the outcome of the aforementioned
#' checks: (i) if either WFHZ, MFAZ standard deviation as well as age ratio test are not
#' simultaneously problematic, a complex sample-based prevalence analysis (for a two-stage
#' PPS cluster sampling) is computed; (ii) all other possibilities will involve either one
#' of the z-scores or the age ratio test being problematic, thus NA (for Not Applicable)
#' get thrown to output table.
#'
#' A concept of "combined flags" is introduced here. This consists on creating a new vector
#' (cflags) of the same length as the input vectors (wfhz_flags and mfaz_flags) and assesses
#' if any element of either input vector is a flag (1), then that element is labelled as
#' flag (1) in the "cflags" vector, otherwise is not flag (0). This ensures that all
#' flagged observations in the WFHZ data and in MFAZ data are excluded for the combined
#' prevalence analysis.
#'
#' @param df A data frame object returned by [process_muac_data()] and [process_wfhz_data()].
#' The process_***_data function will have to used both to prepare the input data to be used
#' in the `compute_combined_prevalence()`. The order of which comes first does not matter,
#' however, since the muac data processor transforms MUAC values into centimeters, those
#' need to be put back into millimeter. This can be achieved my using [recode_muac()] inside
#' [dplyr::mutate()] or [base::transform()] (see example number 3 below).
#'
#' @param .wt A numeric vector containing survey weights. If set to NULL (default)
#' the function will assume self weights, like in ENA for SMART, if otherwise given, the
#' weighted analysis will be computed.
#'
#' @param .edema A character vector containing child's status on edema with "n" for no
#' edema, "y" = yes edema. Should you data be coded differently, re-code it to aforementioned
#' codes.
#' @param .summary_by A character vector containing data on the geographical areas where
#' the data was collected. If you are working on a single survey data, set
#' .summary_by = NULL (default). If this argument is not used, the function will error.
#'
#' @returns A tibble. The length vary depending on .summary_by. If set to NULL, a tibble of
#' 1 x 16 is returned, otherwise, a tibble of n rows (depending on the number of geographical
#' areas in the data set) x 17.
#'
#'
#' @examples
#'
#' ## When .summary_by and .wt are set to NULL ----
#' p <- compute_combined_prevalence(
#' df = anthro.02,
#' .wt = NULL,
#' .edema = edema,
#' .summary_by = NULL
#' )
#'
#' print(p)
#'
#' ## When .wt is not set to NULL ----
#' x <- compute_combined_prevalence(
#' df = anthro.02,
#' .wt = "wtfactor",
#' .edema = edema,
#' .summary_by = NULL
#' )
#'
#' print(x)
#'
#' ## When working on data frame with multiple survey areas ----
#' s <- anthro.03 |>
#' process_age(age = age) |>
#' process_muac_data(
#' sex = sex,
#' muac = muac,
#' age = "age",
#' .recode_sex = TRUE,
#' .recode_muac = TRUE,
#' unit = "cm"
#' ) |>
#' dplyr::mutate(muac = recode_muac(muac, unit = "mm")) |>
#' process_wfhz_data(
#' sex = sex,
#' weight = weight,
#' height = height,
#' .recode_sex = TRUE) |>
#' compute_combined_prevalence(
#' .edema = edema,
#' .summary_by = district
#' )
#'
#' print(s)
#'
#' @export
#'
#' @rdname combined_prevalence
#'
compute_combined_prevalence <- function(df,
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
      std_wfhz = classify_sd(sd(remove_flags(as.numeric(.data$wfhz), "zscore"), na.rm = TRUE)),
      age_ratio = classify_age_sex_ratio(age_ratio_test(.data$age, .expectedP = 0.66)$p),
      std_mfaz = classify_sd(sd(remove_flags(as.numeric(.data$mfaz), "zscore"), na.rm = TRUE)),
      muac_analysis_approach = tell_muac_analysis_strategy(.data$age_ratio, .data$std_mfaz),
      .by = !!.summary_by
    )
  } else {
    ## Non-grouped summary ----
    x <- summarise(
      df,
      std_wfhz = classify_sd(sd(remove_flags(as.numeric(.data$wfhz), "zscore"), na.rm = TRUE)),
      age_ratio = classify_age_sex_ratio(age_ratio_test(.data$age, .expectedP = 0.66)$p),
      std_mfaz = classify_sd(sd(remove_flags(as.numeric(.data$mfaz), "zscore"), na.rm = TRUE)),
      muac_analysis_approach = tell_muac_analysis_strategy(.data$age_ratio, .data$std_mfaz),
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

    std_wfhz <- x$std_wfhz[i]
    muac_analysis_approach <- x$muac_analysis_approach[i]

    if (std_wfhz != "Problematic" && muac_analysis_approach == "unweighted") {
      ### Compute standard complex sample based prevalence analysis ----
      output <- compute_pps_based_combined_prevalence(
        data,
        .wt = {{ .wt }},
        .edema = {{ .edema }},
        .summary_by = !!.summary_by
      )
    } else {
      ## Add grouped NA's ----
      if (!rlang::quo_is_null(.summary_by)) {
        output <- summarise(
          data,
          cgam_p = NA_real_,
          csam_p = NA_real_,
          cmam_p = NA_real_,
          .by = !!.summary_by
        )
      } else {
        ## Add non-grouped NA's ----
        output <- tibble::tibble(
          cgam_p = NA_real_,
          csam_p = NA_real_,
          cmam_p = NA_real_
        )
      }
    }
    results[[i]] <- output
  }
  ### Ensure that all geographical areas are added to the tibble ----
  if (!rlang::quo_is_null(.summary_by)) {
    results <- dplyr::bind_rows(results) |>
      dplyr::relocate(.data$cgam_p, .after = .data$cgam_n) |>
      dplyr::relocate(.data$csam_p, .after = .data$csam_n) |>
      dplyr::relocate(.data$cmam_p, .after = .data$cmam_n)
  } else {
    ## Non-grouped results
    results <- dplyr::bind_rows(results)
  }
  results
}
