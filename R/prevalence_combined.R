get_combined_prevalence_estimates <- function(df,
                                              .wt = NULL,
                                              .edema = NULL,
                                              .summary_by
                                              ) {
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
#### Create survey object ----
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
#### Summarise prevalence ----
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


# Function to compute combined prevalence --------------------------------------
#'
#'
compute_combined_prevalence <- function(df,
                                        .wt = NULL, .edema = NULL, .summary_by) {
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
    p <- get_combined_prevalence_estimates(
      df,
      .wt = {{ .wt }},
      .edema = {{ .edema }},
      .summary_by = {{ .summary_by }}
    )
  } else if (std_wfhz == "Problematic" && muac_analysis == "unweighted") {
    ### Compute prevalence with normalized zscores ----
    #### Normalize zscores, define combined cases and combined flags ----
  } else {
    p <- NA_real_
  }
  p
}

