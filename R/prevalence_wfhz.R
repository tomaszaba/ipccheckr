get_wfhz_prevalence_estimates <- function(df,
                         .wt = NULL,
                         .edema = NULL,
                         .summary_by) {
  df <- with(
    df,
    define_wasting(
      df,
      zscore = .data$wfhz,
      edema = {{ .edema }},
      base = "wfhz"
    )
  )
  #### Create a survey object ----
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
compute_wfhz_prevalence <- function(df,
                                    .wt = NULL,
                                    .edema = NULL,
                                    .summary_by) {

  ## Get and classify standard deviation ----
  x <- df[["wfhz"]]
  std <- classify_sd(sd(remove_flags(x, "zscore"), na.rm = TRUE))

  if (std != "Problematic") {
    ### Compute observed prevalence ----
    p <- df |>
      get_wfhz_prevalence_estimates(
        .wt = {{ .wt }},
        .edema = {{ .edema }},
        .summary_by = {{.summary_by}}
        )
  } else {
    ### Compute prevalence with a normalized zscores ----
  }
  p
}
