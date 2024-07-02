# Function to compute WFHZ-based prevalence ------------------------------------

#'
#'
#'
compute_wfhz_prevalence <- function(df, .wt = NULL, edema = NULL, .summary_by) {
  ## Get and classify standard deviation ----
  x <- df[["wfhz"]]
  std <- classify_sd(sd(remove_flags(x, "zscore"), na.rm = TRUE))

  if (std != "Problematic") {
    ### Compute observed prevalence ----
    df <- with(df,
               define_wasting(df,
                              zscore = .data$wfhz,
                              edema = {{ edema }},
                              base = "wfhz"))
    #### Create a survey object ----
    if (!is.null(.wt)) {
      srvy <- df |>
        as_survey_design(
          ids = .data$cluster,
          pps = "brewer",
          variance = "YG",
          weights = {{ .wt }}
          ) |>
        group_by({{ .summary_by }})
      #### Summarise prevalence ----
      p <- srvy |>
        filter(.data$flag_wfhz == 0) |>
        summarise(
          across(
            c(.data$gam:.data$mam),
            \(x) survey_mean(x,
              vartype = "ci",
              level = 0.95,
              deff = TRUE,
              na.rm = TRUE
            )
          )
        )
    } else {
      srvy <- df |>
        as_survey_design(
          ids = .data$cluster,
          pps = "brewer",
          variance = "YG"
        ) |>
        group_by( {{ .summary_by }})
      #### Summarise prevalence ----
      p <- srvy |>
        filter(.data$flag_wfhz == 0) |>
        summarise(
          across(
            c(.data$gam:.data$mam),
            \(x) survey_mean(x,
              vartype = "ci",
              level = 0.95,
              deff = TRUE,
              na.rm = TRUE
            )
          )
        )
      }
    p
  }

  if (std == "Problematic") {
    ### Compute prevalence with a normalized zscores ----
    df <- df |>
      mutate(wfhz = normalize_zscore(.data$wfhz)) |>
      define_wasting(zscore = .data$wfhz, edema = {{ edema }}, base = "wfhz")

    if (!is.null(.wt)) {
      srvy <- df |>
        as_survey_design(
          ids = .data$cluster,
          pps = "brewer",
          variance = "YG",
          weights = {{ .wt }}
        ) |>
        group_by({{ .summary_by }})
      #### Summarise prevalence ----
      p <- srvy |>
        filter(.data$flag_wfhz == 0) |>
        summarise(
          across(
            c(.data$gam:.data$mam),
            \(x) survey_mean(x,
              vartype = "ci",
              level = 0.95,
              deff = TRUE,
              na.rm = TRUE
            )
          )
        )
    } else {
      srvy <- df |>
        as_survey_design(
          ids = .data$cluster,
          pps = "brewer",
          variance = "YG"
        ) |>
        group_by({{ .summary_by }})
      #### Summarise prevalence ----
      p <- srvy |>
        filter(.data$flag_wfhz == 0) |>
        summarise(
          across(
            c(.data$gam:.data$mam),
            \(x) survey_mean(x,
              vartype = "ci",
              level = 0.95,
              deff = TRUE,
              na.rm = TRUE
            )
          )
        )
      }
    }
  p
}

# Function to compute MUAC-based prevalence ------------------------------------
#'
#'
#'
compute_muac_prevalence <- function(df, .wt = NULL, edema = NULL, .summary_by) {
  ## Get and classify age ratio and standard deviation ----
  a <- df[["age"]]
  age_ratio <- classify_age_sex_ratio(age_ratio_test(a, .expectedP = 0.66)$p)
  zs <- df[["mfaz"]]
  std <- classify_sd(sd(remove_flags(as.numeric(zs), "zscore"), na.rm = TRUE))

  ## Check the appropriate analysis strategy to follow ----
  muac_analysis <- tell_muac_analysis_strategy(age_ratio, std)

  if (muac_analysis == "unweighted") {
    ## Compute observed prevalence ----
    df <- with(
      df,
      define_wasting(df,
        muac = .data$muac,
        edema = {{ edema }},
        base = "muac"
      )
    )

    ### Weighted survey analysis ----
    if (!is.null(.wt)) {
      srvy <- df |>
        as_survey_design(
          ids = .data$cluster,
          pps = "brewer",
          variance = "YG",
          weights = {{ .wt }}
        ) |>
        group_by({{ .summary_by }})
      #### Summarize prevalence ----
      p <- srvy |>
        filter(.data$flag_mfaz == 0) |>
        summarise(
          across(
            c(.data$gam:.data$mam),
            \(x) survey_mean(x,
              vartype = "ci",
              level = 0.95,
              deff = TRUE,
              na.rm = TRUE
            )
          )
        )
    } else {
      ### Unweighted: typical SMART survey analysis ----
      srvy <- df |>
        as_survey_design(
          ids = .data$cluster,
          pps = "brewer",
          variance = "YG"
        ) |>
        group_by({{ .summary_by }})
      #### Summarise prevalence ----
      p <- srvy |>
        filter(.data$flag_mfaz == 0) |>
        summarise(
          across(
            c(.data$gam:.data$mam),
            \(x) survey_mean(x,
              vartype = "ci",
              level = 0.95,
              deff = TRUE,
              na.rm = TRUE
            )
          )
        )
      }
    p
  }

  if (muac_analysis == "weighted") {
    #### Apply CDC/SMART MUAC tool weighting for age bias ----
    p <- df |>
      filter(.data$flag_mfaz == 0) |>
      mutate(muac = recode_muac(.data$muac, unit = "mm")) |>
      summarise(
        sam = cdc_apply_age_weighting(
          muac = .data$muac,
          age = .data$age,
          edema = .data$edema,
          status = "sam"
        ),
        mam = cdc_apply_age_weighting(
          muac = .data$muac,
          age = .data$age,
          edema = .data$edema,
          status = "mam"
        ),
        gam = sum(.data$sam, .data$mam)
      )
    }
  p

  if (muac_analysis == "missing") {
    p <- NA_real_
  }
  p
}

# Function to compute combined prevalence --------------------------------------
#'
#'
compute_combined_prevalence <- function(df,
                                        .wt = NULL, edema = NULL, .summary_by) {
  ## Get WHZ's standard deviation and classify it ----
  x <- df[["wfhz"]]
  std_wfhz <- classify_sd(sd(remove_flags(x, "zscore"), na.rm = TRUE))

  ## Check the appropriate MUAC's analysis strategy to follow ----
  a <- df[["age"]]
  age_ratio <- classify_age_sex_ratio(age_ratio_test(a, .expectedP = 0.66)$p)
  zs <- df[["mfaz"]]
  std_mfaz <- classify_sd(sd(remove_flags(zs, "zscore"), na.rm = TRUE))
  muac_analysis <- tell_muac_analysis_strategy(age_ratio, std_mfaz)

  if (std_wfhz != "Problematic" && muac_analysis == "unweighted") {
    ### Compute observed prevalence ----
    df <- with(
      df,
      define_wasting(df,
        zscore = .data$wfhz,
        muac = .data$muac,
        edema = {{ edema }},
        base = "combined"
      ) |>
        mutate(
          cflags = ifelse(.data$flag_wfhz == 1 | .data$flag_mfaz == 1, 1, 0)
        )
    )

    #### Create survey object ----
    if (!is.null(.wt)) {
      srvy <- df |>
        as_survey_design(
          ids = .data$cluster,
          pps = "brewer",
          variance = "YG",
          weights = {{ .wt }}
        ) |>
        group_by({{ .summary_by }})
      #### Summarise prevalence ----
      p <- srvy |>
        filter(.data$cflags == 0) |>
        summarise(
          across(
            c(.data$cgam:.data$cmam),
            \(x) survey_mean(x,
              vartype = "ci",
              level = 0.95,
              deff = TRUE,
              na.rm = TRUE
            )
          )
        )
    } else {
      #### Unweighted analysis: A typical SMART survey analysis ----
      srvy <- df |>
        as_survey_design(
          ids = .data$cluster,
          pps = "brewer",
          variance = "YG"
        ) |>
        group_by({{ .summary_by }})
      #### Summarise prevalence ----
      p <- srvy |>
        filter(.data$cflags == 0) |>
        summarise(
          across(
            c(.data$cgam:.data$cmam),
            \(x) survey_mean(x,
              vartype = "ci",
              level = 0.95,
              deff = TRUE,
              na.rm = TRUE
            )
          )
        )
      }
    p
  }

  if (std_wfhz == "Problematic" && muac_analysis == "unweighted") {
    ### Compute prevalence with normalized zscores ----
    #### Normalize zscores, define combined cases and combined flags ----
    df <- with(
      df,
      mutate(wfhz = normalize_zscore(.data$wfhz)) |>
        define_wasting(
          zscore = .data$wfhz,
          muac = .data$muac,
          edema = {{ edema }},
          base = "combined"
        ) |>
        mutate(cflags = ifelse(.data$flag_wfhz == 1 | .data$flag_mfaz == 1, 1, 0))
    )

    #### Create survey object ----
    if (!is.null(.wt)) {
      srvy <- df |>
        as_survey_design(
          ids = .data$cluster,
          pps = "brewer",
          variance = "YG",
          weights = {{ .wt }}
        ) |>
        group_by({{ .summary_by }})
      #### Summarise prevalence ----
      p <- srvy |>
        filter(.data$cflags == 0) |>
        summarise(
          across(
            c(.data$cgam:.data$cmam),
            \(x) survey_mean(x,
              vartype = "ci",
              level = 0.95,
              deff = TRUE,
              na.rm = TRUE
            )
          )
        )
    } else {
      #### Unweighted analysis: A typical SMART survey analysis ----
      srvy <- df |>
        as_survey_design(
          ids = .data$cluster,
          pps = "brewer",
          variance = "YG"
        ) |>
        group_by({{ .summary_by }})
      #### Summarise prevalence ----
      p <- srvy |>
        filter(.data$cflags == 0) |>
        summarise(
          across(
            c(.data$cgam:.data$cmam),
            \(x) survey_mean(x,
              vartype = "ci",
              level = 0.95,
              deff = TRUE,
              na.rm = TRUE
            )
          )
        )
      }
    p
  }

  if (std_wfhz == "Problematic" && muac_analysis == "weighted") {
    p <- NA_real_
  }
  p
}
