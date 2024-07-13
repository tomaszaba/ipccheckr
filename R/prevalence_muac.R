#'
#' A helper function to tell how to go about MUAC prevalence analysis based on
#' on the output of age ratio and standard deviation test results
#'
#' @param age_ratio_class,sd_class Character vectors storing age ratio's p-values
#' and standard deviation's classification, respectively.
#'
#' @returns A character vector of the same length containing the indication of
#' what to do for the MUAC prevalence analysis: "weighted", "unweighted" and
#' "missing". If "weighted", the CDC weighting approach is applied to correct for
#' age bias. If "unweighted" a normal complex sample analysis is applied, and for
#' the latter, NA are thrown.
#'
#'
tell_muac_analysis_strategy <- function(age_ratio_class, sd_class) {
  case_when(
    age_ratio_class == "Problematic" & sd_class != "Problematic" ~ "weighted",
    age_ratio_class != "Problematic" & sd_class == "Problematic" ~ "missing",
    age_ratio_class == "Problematic" & sd_class == "Problematic" ~ "missing",
    .default = "unweighted"
  )
}


#'
#'
#' Correct the observed MUAC prevalence when there is an unbalanced sample
#' between children under 2 and over two years old
#'
#' @description
#' As documented in the SMART MUAC tool and in the literature, MUAC shows a known
#' bias towards younger children. In a balanced sample, it is expected to have
#' nearly two thirds of the sample to be of children over two years old. If too
#' few older children are included in the sample, the weighted tool should be used.
#'
#' `apply_cdc_age_weighting()` does that. It takes the proportion of children
#' under 2 and adds to the product of 2 times the proportion of children over two,
#' then divided by 3. The use of this function is informed by the output of
#' [age_ratio_test()]. There is difference between this function and that in the
#' SMART plausibility check. Consider reading the documentation before use.
#'
#' @param muac An integer vector containing MUAC measurements in mm.
#'
#' @param age A double vector containing age in months with at least 2 decimal
#' places.
#'
#' @param .edema Optional. If given, it should be a character vector of "y" = Yes,
#' "n" = No bilateral edema.
#'
#' @param status If you wish to get the prevalence/proportions of severe or
#' moderate acute malnutrition. Set `status = "sam" or status = "mam"` for the
#' former or latter, respectively.
#'
#' @returns A numeric vector of length and size 1.
#'
apply_cdc_age_weighting <- function(muac, age,
                                    .edema = NULL, status = c("sam", "mam")) {

  ## Match arguments ----
  status <- match.arg(status)

  if (!is.null(.edema)) {
    ### Define cases ----
    nut_status <- classify_wasting_for_cdc_approach(muac = muac, .edema = {{ .edema }})

    ### Compute age weighted prevalence ----
    age_group <- ifelse(age < 24, "under_2", "over_2")
    nut_U2 <- ifelse(age_group == "under_2" & nut_status == status, 1, 0)
    nut_O2 <- ifelse(age_group == "over_2" & nut_status == status, 1, 0)
    p <- mean(nut_U2, na.rm = TRUE) + (2 * mean(nut_O2, na.rm = TRUE)) / 3

  } else {
    ### Define cases ----
    nut_status <- classify_wasting_for_cdc_approach(muac)

    ### Compute age weighted prevalence ----
    age_group <- ifelse(age < 24, "under_2", "over_2")
    nut_U2 <- ifelse(age_group == "under_2" & nut_status == status, 1, 0)
    nut_O2 <- ifelse(age_group == "over_2" & nut_status == status, 1, 0)
    p <- mean(nut_U2, na.rm = TRUE) + (2 * mean(nut_O2, na.rm = TRUE)) / 3
  }
  p
}



#'
#'
#' A wrapper function to compute of `apply_cdc_age_weighting()` that allows to work on
#' a data frame
#'
#' @description
#' `compute_weighted_prevalence()` is the main function use to compute age adjusted MUAC
#' prevalence where there are excess of children 6:23 over 24:59 months. It allows the
#' computations to be done on a data frame. The function is used inside the main and
#' exported function to compute MUAC based prevalence. Before computing the prevalence,
#' the function first removed the flagged data so the computations are performed on
#' non-flagged observations.
#'
#' @param df A data frame object returned by [process_muac_data()] this will contain the
#' wrangled vectors that are read inside the function.
#'
#' @param .edema A character vector containing child's status on edema with "n" for no
#'  edema, "y" = yes edema. Should you data be coded differently, re-code it to aforementioned
#'  codes.
#' @param .summary_by A character vector containing data on the geographical areas where
#'  the data was collected. This is to group the survey design object into different
#'  geographical areas in the data and allow for summaries to be computed for each of them.
#'
#' @returns A tibble with length and size varying according to use of `.summary_by`.
#' If set to NULL, a tibble of 1 x 3 is returned, otherwise the size of the tibble with be
#' corresponding to the number of groups/areas in the vector given to `.summary_by`, but
#' with the same length.
#'
#'
compute_weighted_prevalence <- function(df, .edema=NULL, .summary_by = NULL) {
  .summary_by <- rlang::enquo(.summary_by)

  if (!is.null(.summary_by)) {
    df <- df |>
      filter(.data$flag_mfaz == 0) |>
      #mutate(muac = recode_muac(.data$muac, unit = "cm")) |>
      summarise(
        sam = apply_cdc_age_weighting(.data$muac, .data$age, {{ .edema }}, status = "sam"),
        mam = apply_cdc_age_weighting(.data$muac, .data$age, {{ .edema }}, status = "mam"),
        gam = sum(.data$sam, .data$mam),
        .by = !!.summary_by
      ) |>
      dplyr::rename(
        gam_p = .data$gam, sam_p = .data$sam, mam_p = .data$mam
      )
  } else {
    df <- df |>
      filter(.data$flag_mfaz == 0) |>
      mutate(muac = recode_muac(.data$muac, unit = "mm")) |>
      summarise(
        sam = apply_cdc_age_weighting(.data$muac, .data$age, {{ .edema }}, status = "sam"),
        mam = apply_cdc_age_weighting(.data$muac, .data$age, {{ .edema }}, status = "mam"),
        gam = sum(.data$sam, .data$mam)
      ) |>
      dplyr::rename(
        gam_p = .data$gam, sam_p = .data$sam, mam_p = .data$mam
      )
  }
  df
}



#'
#'
#' Compute MUAC based prevalence estimates of data collected from a two-stage cluster
#' survey sample design, with the first stage sampling done with Probability Proportional
#' to the size of population
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
#' @param .summary_by A character vector containing data on the geographical areas where
#'  the data was collected. This is to group the survey design object into different
#'  geographical areas in the data and allow for summaries to be computed for each of them.
#'
#'  @returns A tibble of size depending on the number of groups of the vector given to
#'  `.summary_by` or if set to NULL, and of length 17.
#'
#'
#'
compute_pps_based_muac_prevalence <- function(df,
                                              .wt = NULL,
                                              .edema = NULL,
                                              .summary_by = NULL) {
  df <- df |>
    define_wasting(muac = .data$muac, edema = {{ .edema }}, base = "muac")

  ### Weighted survey analysis ----
  if (!is.null(.wt)) {
    srvy <- df |>
      as_survey_design(
        ids = .data$cluster,
        pps = "brewer",
        variance = "YG",
        weights = {{ .wt }}
      )
  } else {
    ### Unweighted: typical SMART survey analysis ----
    srvy <- df |>
      as_survey_design(
        ids = .data$cluster,
        pps = "brewer",
        variance = "YG"
      )
  }
  #### Summarise prevalence ----
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
      wt_pop = sum(srvyr::cur_svy_wts())
    )
  p
}


#'
#'
#'
#' Compute acute malnutrition prevalence based on MUAC (the absolute values)
#'
#' @description
#' `compute_muac_prevalence()` is a handy function designed to dynamically compute acute
#' malnutrition's prevalence using the absolute values of MUAC, however using the MFAZ for
#' quality checks before advancing to prevalence computations. Under the hood, the function
#' first checks the status of MFAZ's standard deviation (SD) after removing flags, and
#' the status of age ratio among children aged 6:23 vs 24:59 months. Then it decides on the
#' appropriate prevalence analysis approach to follow: (i) if SD & age ratio are both not
#' problematic, a complex sample-based prevalence analysis (for a two-stage  PPS
#' cluster sampling) is computed; (ii) if MFAZ's SD is not problematic, but age ratio test
#' is, the CDC/SMART MUAC tool weighting approach is used to compute the prevalence; (iii)
#' lastly, if MFAZ's SD is problematic even if age ratio test is not, no prevalence
#' analysis is computed and NA (of Not Applicable) are thrown.
#' The function also super handy to work on large data sets with multiple survey areas. For
#' this, the aforementioned conditionals are checked for each survey areas in a summarized
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
#' @param .summary_by A character vector containing data on the geographical areas where
#' the data was collected. If you are working on a single survey data, set
#' .summary_by = NULL (default). If this argument is not used, the function will error.
#'
#' @returns A tibble. The length vary depending on .summary_by. If set to NULL, a tibble of
#' 1 x 16 is returned, otherwise, a tibble of n rows (depending on the number of geographical
#' areas in the data set) x 17.
#'
#' @examples
#' ## When .summary.by = NULL ----
#'
#' x <- compute_muac_prevalence(
#' df = anthro.04,
#' .wt = NULL,
#' .edema = edema,
#' .summary_by = NULL
#' )
#'
#' print(x)
#'
#' ## When .summary_by is not set to NULL ----
#'
#' p <- compute_muac_prevalence(
#' df = anthro.04,
#' .wt = NULL,
#' .edema = edema,
#' .summary_by = province
#' )
#'
#' print(p)
#'
#'
#' @export
#'
compute_muac_prevalence <- function(df,
                                    .wt = NULL,
                                    .edema = NULL,
                                    .summary_by = NULL) {

  ## Difuse argument .summary_by ----
  .summary_by <- rlang::enquo(.summary_by)

  ## An empty vector type list ----
  results <- list()

  if (!rlang::quo_is_null(.summary_by)) {
    ## Grouped summary of analysis approach ----
    x <- df |>
      group_by(!!.summary_by) |>
      summarise(
        age_ratio = classify_age_sex_ratio(age_ratio_test(.data$age, .expectedP = 0.66)$p),
        std = classify_sd(sd(remove_flags(as.numeric(.data$mfaz), "zscore"), na.rm = TRUE)),
        analysis_approach = tell_muac_analysis_strategy(.data$age_ratio, .data$std),
        .groups = "drop"
      )
  } else {
    ## Non-grouped summary of analysis approach ----
    x <- df |>
      summarise(
        age_ratio = classify_age_sex_ratio(age_ratio_test(.data$age, .expectedP = 0.66)$p),
        std = classify_sd(sd(remove_flags(as.numeric(.data$mfaz), "zscore"), na.rm = TRUE)),
        analysis_approach = tell_muac_analysis_strategy(.data$age_ratio, .data$std)
      )
  }

  ## Iterate over data frame to compute prevalence according to analysis_approach ----
  for (i in seq_len(nrow(x))) {
    if (!rlang::quo_is_null(.summary_by)) {
      area <- dplyr::pull(x, !!.summary_by)[i]
      data <- filter(df, !!sym(rlang::quo_name(.summary_by)) == area)
    } else {
      data <- df
    }

    analysis_approach <- x$analysis_approach[i]

    if (analysis_approach == "unweighted") {
      ### Compute standard complex sample based prevalence analysis ----
      output <- compute_pps_based_muac_prevalence(data, {{ .wt }}, {{ .edema }}, !!.summary_by)
    } else if (analysis_approach == "weighted") {
      ### Compute grouped weighted prevalence ----
      if (!rlang::quo_is_null(.summary_by)) {
        output <- compute_weighted_prevalence(data, .edema = {{ .edema }}, !!.summary_by)
      } else {
        ### Compute grouped weighted prevalence ----
        output <- compute_weighted_prevalence(data, .edema = {{ .edema }})
      }
    } else {
      ## Add grouped NA's ----
      if (!rlang::quo_is_null(.summary_by)) {
        output <- summarise(
          data,
          gam_p = NA_real_,
          sam_p = NA_real_,
          mam_p = NA_real_,
          .by = !!.summary_by
        )
      } else {
        ## Add non-grouped NA's ----
        output <- tibble::tibble(
          gam_p = NA_real_,
          sam_p = NA_real_,
          mam_p = NA_real_
        )
      }
    }
    results[[i]] <- output
  }
  ### Ensure that all geographical areas are added to the tibble ----
  if (!rlang::quo_is_null(.summary_by)) {
    results <- dplyr::bind_rows(results) |>
      dplyr::relocate(.data$gam_p, .after = .data$gam_n) |>
      dplyr::relocate(.data$sam_p, .after = .data$sam_n) |>
      dplyr::relocate(.data$mam_p, .after = .data$mam_n)
  } else {
    ## Non-grouped results
    results <- dplyr::bind_rows(results)
  }
  results
}
