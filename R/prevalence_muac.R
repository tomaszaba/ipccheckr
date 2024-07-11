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

# The CDC approach to correct for age bias in MUAC -----------------------------

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
#' @param age A double vector containing age in months with at least 2 decimal
#' places.
#' @param .edema Optional. If given, it should be a character vector of "y" = Yes,
#' "n" = No bilateral edema.
#' @param status If you wish to get the prevalence/proportions of severe or
#' moderate acute malnutrition. Set `status = "sam" or status = "mam"` for the
#' former or latter, respectively.
#'
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
#'
compute_pps_based_muac_prevalence <- function(df,
                                          .wt=NULL,
                                          .edema=NULL,
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
compute_muac_prevalence <- function(df,
                                    .wt = NULL,
                                    .edema = NULL,
                                    .summary_by = NULL) {
  .summary_by <- rlang::enquo(.summary_by)
  results <- list()

  ## Get and classify age ratio and standard deviation ----
  if (!rlang::quo_is_null(.summary_by)) {
    x <- df |>
      group_by(!!.summary_by) |>
      summarise(
        age_ratio = classify_age_sex_ratio(age_ratio_test(.data$age, .expectedP = 0.66)$p),
        std = classify_sd(sd(remove_flags(as.numeric(.data$mfaz), "zscore"), na.rm = TRUE)),
        analysis_approach = tell_muac_analysis_strategy(.data$age_ratio, .data$std),
        .groups = "drop"
      )
  } else {
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
      if (!rlang::quo_is_null(.summary_by)) {
        ### Compute weighted prevalence summarized at .summary_by ----
        output <- compute_weighted_prevalence(data, .edema = {{ .edema }}, !!.summary_by)
      } else {
        ### Compute non-summarized weighted prevalence ----
        output <- compute_weighted_prevalence(data, .edema = {{ .edema }})
      }
    } else {
      if (!rlang::quo_is_null(.summary_by)) {
        ### Add NA's to the summarized tibble accordingly ----
        output <- summarise(
          data,
          gam_p = NA_real_,
          sam_p = NA_real_,
          mam_p = NA_real_,
          .by = !!.summary_by
        )
      } else {
        ### Return a non-summarised tibble ----
        output <- tibble::tibble(
          gam_p = NA_real_,
          sam_p = NA_real_,
          mam_p = NA_real_
        )
      }
    }
    results[[i]] <- output
  }
  ### Ensure that all geographical aras are added to the tibble ----
  if (!rlang::quo_is_null(.summary_by)) {
    results <- dplyr::bind_rows(results) |>
      dplyr::relocate(.data$gam_p, .after = .data$gam_n) |>
      dplyr::relocate(.data$sam_p, .after = .data$sam_n) |>
      dplyr::relocate(.data$mam_p, .after = .data$mam_n)
  } else {
    results <- dplyr::bind_rows(results)
  }
  results
}
