## Create a vector of values with known flagging status ----
### MFAZ-based values ----

local(
  {
    mfaz <- c(
      -2.265, -3.716, -0.658, -5.275, -0.72, -2.261, -2.264, -2.118,
      -4.451, -3.603, -0.663, -2.112, -2.107, -2.261, -1.828, 3.5
    )

    expected_results <- c(
      0, 1, 0, 1, 0, 0, 0, 0,
      1, 1, 0, 0, 0, 0, 0, 1
    )

    flags_mfaz <- add_flags(mfaz, method = "mfaz")

    testthat::test_that(
      "add_flags returns expected output",
      {
        expect_vector(
          flags_mfaz,
          size = 16
        )
        expect_equal(flags_mfaz, expected_results)
      }
    )
  }
)

## Test flagging function against expected SMART flags using {nipnTK} package

svy <- nipnTK::flag.ex03


## Function to apply SMART flagging approach as described in https://smartmethodology.org/survey-planning-tools/smart-methodology/plausibility-check/?doing_wp_cron=1715286761.8427860736846923828125

smart_flag_zscore <- function(df, z) {
  z_cutoffs <- mean(df[[z]], na.rm = TRUE) |>
    (\(x) c(x - 3, x + 3))()

  z_flag <- ifelse(df[[z]] < z_cutoffs[1] | df[[z]] > z_cutoffs[2], 1, 0)

  z_flag
}


testthat::test_that(
  "add_flags returns expected output", {
    expect_equal(
      add_flags(x = svy$haz, method = "mfaz"),
      split(svy, svy$state) |>
        lapply(FUN = smart_flag_zscore, z = "haz") |>
        unlist()
    )
  }
)
