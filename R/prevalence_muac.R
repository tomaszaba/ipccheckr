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
compute_weighted_prevalence <- function(df, .edema=NULL) {
  df |>
    filter(.data$flag_mfaz == 0) |>
    mutate(muac = recode_muac(.data$muac, unit = "mm")) |>
    summarise(
      sam = apply_cdc_age_weighting(
        muac = .data$muac,
        age = .data$age,
        .edema = {{ .edema }},
        status = "sam"
      ),
      mam = apply_cdc_age_weighting(
        muac = .data$muac,
        age = .data$age,
        .edema = {{ .edema }},
        status = "mam"
      ),
      gam = sum(.data$sam, .data$mam)
    )
}

#'
#'
#'
get_muac_prevalence_estimates <- function(df, .wt=NULL, .edema=NULL, .summary_by) {
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
compute_muac_prevalence <- function(df, .wt = NULL, .edema = NULL, .summary_by) {
  ## Get and classify age ratio and standard deviation ----
  a <- df[["age"]]
  age_ratio <- classify_age_sex_ratio(age_ratio_test(a, .expectedP = 0.66)$p)
  zs <- df[["mfaz"]]
  std <- classify_sd(sd(remove_flags(as.numeric(zs), "zscore"), na.rm = TRUE))

  ## Check the appropriate analysis strategy to follow ----
  muac_analysis <- tell_muac_analysis_strategy(age_ratio, std)

  if (muac_analysis == "unweighted") {
    p <- df |>
      get_muac_prevalence_estimates(
        .wt = {{ .wt }},
        .summary_by = {{ .summary_by }},
        .edema = {{ .edema }}
      )
  }
  if (muac_analysis == "weighted") {
    p <- df |>
      group_by({{ .summary_by }}) |>
      compute_weighted_prevalence(.edema = {{ .edema }})
  }
  if (muac_analysis == "missing") {
    p <- NA_real_
  }
  p
}
