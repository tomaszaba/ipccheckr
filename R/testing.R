compute_muac_Prevalence <- function(df, .wt = NULL, .edema = NULL, .summary_by) {
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
