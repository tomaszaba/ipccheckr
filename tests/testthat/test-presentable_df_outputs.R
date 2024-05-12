# Test checks: Pretty outputers ------------------------------------------------

## Test check: return_pretty_ouput_crude_muac() ----

local(
  {
    quality <- data.01 |>
      process_muac_data(
      sex = "sex",
      muac = "muac",
      .recode_sex = TRUE,
      .recode_muac = FALSE,
      unit = "none"
      ) |>
      check_plausibility_crude_muac()|>
      return_pretty_ouput_crude_muac()

    ### The test ----
    test_that(
      "return_pretty_ouput_crude_muac() works",
      {
        testthat::expect_s3_class(quality, "tbl_df")
        testthat::expect_equal(ncol(quality), 9)
        testthat::expect_equal(nrow(quality), 1)
        testthat::expect_true(
          all(c("n", "Flagged data (%)", "Class. of flagged data",
                "Sex ratio (p)", "Class. of sex ratio", "DPS (#)",
                "Class. of DPS", "Standard Dev* (#)", "Class. of standard dev"
          ) %in% names(quality)

          )
        )
      }
    )
  }
)

## Test check: return_pretty_ouput_mfaz() ----
local(
  {
    quality <- data.01 |>
      process_age() |>
      process_muac_data(
        sex = "sex",
        age = "age",
        muac = "muac",
        .recode_sex = TRUE,
        .recode_muac = TRUE,
        unit = "cm"
      ) |>
      check_plausibility_mfaz(
        flags = "flags",
        sex = "sex",
        muac = "muac",
        age = "age",
        area = area
      ) |>
      return_pretty_ouput_mfaz()

    ### The test ----
    ### The test ----
    test_that(
      "return_pretty_ouput_mfaz() works",
      {
        testthat::expect_s3_class(quality, "tbl_df")
        testthat::expect_equal(ncol(quality), 18)
        testthat::expect_equal(nrow(quality), 11)
        testthat::expect_true(
          all(c("area", "n", "Flagged data (%)",
                "Class. of flagged data", "Sex ratio (p)", "Class. of sex ratio",
                "Age ratio (p)", "Class. of age ratio", "DPS (#)",
                "Class. of DPS", "Standard Dev* (#)", "Class. of standard dev",
                "Skewness* (#)", "Class. of skewness", "Kurtosis* (#)",
                "Class. of kurtosis", "Overall score", "Overall quality"
          ) %in% names(quality)

          )
        )
      }
    )
  }
)

## Test check: return_pretty_ouput_whz() ----

### To be updated :-)
