# Function to classify nutritional status  -------------------------------------

#'
#' A helper function to classify nutritional status into SAM, MAM or not wasted
#'
#' `classify_wasting_for_cdc_approach()` is used a helper inside
#' [apply_cdc_age_weighting()] to classify nutritional status into "sam", "mam"
#' or "not wasted" and then the vector returned is used downstream to calculate
#' the proportions of children with severe and moderate acute malnutrition.
#'
#' @param muac An integer vector containing MUAC values. They should be in
#' millimeters.
#'
#' @param .edema Optional. Its a vector containing data on bilateral pitting
#' edema coded as "y" for yes and "n" for no.
#'
#'
classify_wasting_for_cdc_approach <- function(muac, .edema = NULL) {
  if (!is.null(.edema)) {
    #edema <- ifelse(edema == 1, "y", "n")
    x <- case_when(
      muac < 115 | {{ .edema }} == "y" ~ "sam",
      muac >= 115 & muac < 125 & {{ .edema }} == "n" ~ "mam",
      .default = "not wasted"
    )
  } else {
    x <- case_when(
      muac < 115 ~ "sam",
      muac >= 115 & muac < 125 ~ "mam",
      .default = "not wasted"
    )
  }
  x
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
      mutate(muac = recode_muac(.data$muac, unit = "mm")) |>
      group_by(!!.summary_by) |>
      summarise(
        sam = apply_cdc_age_weighting(.data$muac, .data$age, {{ .edema }}, status = "sam"),
        mam = apply_cdc_age_weighting(.data$muac, .data$age, {{ .edema }}, status = "mam"),
        gam = sum(.data$sam, .data$mam),
        .groups = "drop"
      ) |>
      mutate(
        gam_p = gam, sam_p = sam, mam_p = mam,
        gam = NA, sam = NA, mam = NA
      ) |> dplyr::select(!2:4)
  } else {
    df <- df |>
      filter(.data$flag_mfaz == 0) |>
      mutate(muac = recode_muac(.data$muac, unit = "mm")) |>
      summarise(
        sam = apply_cdc_age_weighting(.data$muac, .data$age, {{ .edema }}, status = "sam"),
        mam = apply_cdc_age_weighting(.data$muac, .data$age, {{ .edema }}, status = "mam"),
        gam = sum(.data$sam, .data$mam)
      ) |>
      mutate(
        gam_p = gam, sam_p = sam, mam_p = mam,
        gam = NA, sam = NA, mam = NA
      ) |> dplyr::select(!2:4)
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
    x <- group_by(df, !!.summary_by) |>
      summarise(
        age_ratio = classify_age_sex_ratio(age_ratio_test(.data$age, .expectedP = 0.66)$p),
        std = classify_sd(sd(remove_flags(as.numeric(.data$mfaz), "zscore"), na.rm = TRUE)),
        analysis_approach = tell_muac_analysis_strategy(age_ratio, std),
        .groups = "drop"
      )
  } else {
    x <- summarise(
      df,
      age_ratio = classify_age_sex_ratio(age_ratio_test(.data$age, .expectedP = 0.66)$p),
      std = classify_sd(sd(remove_flags(as.numeric(.data$mfaz), "zscore"), na.rm = TRUE)),
      analysis_approach = tell_muac_analysis_strategy(age_ratio, std)
    )
  }

  for (i in seq_along(nrow(x))) {
    if (!rlang::quo_is_null(.summary_by)) {
      area <- dplyr::pull(x, !!.summary_by)[i]
      data <- filter(df, !!sym(rlang::quo_name(.summary_by)) == !!area)
    } else {
      data <- df
    }

    analysis_approach <- x$analysis_approach[i]

    if (analysis_approach == "unweighted") {
      output <- compute_pps_based_muac_prevalence(data, {{ .wt }}, {{ .edema }}, !!.summary_by)
    } else if (analysis_approach == "weighted") {
      if (!rlang::quo_is_null(.summary_by)) {
        output <- compute_weighted_prevalence(data, .edema = {{ .edema }}, !!.summary_by)
      } else {
        output <- compute_weighted_prevalence(data, .edema = {{ .edema }})
      }
    } else {
      output <- tibble::tibble(
        gam_n = NA,
        sam_n = NA,
        mam_n = NA,
        gam_p = NA,
        sam_p = NA,
        mam_p = NA,
        gam_p_low = NA,
        gam_p_upp = NA,
        sam_p_low = NA,
        sam_p_upp = NA,
        mam_p_low = NA,
        mam_p_upp = NA,
        gam_p_deff = NA,
        sam_p_deff = NA,
        mam_p_deff = NA
      )
    }
    results[[i]] <- output
  }
  dplyr::bind_rows(results)
}
