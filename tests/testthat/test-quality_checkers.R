# Tests checks: Quality Checkers------------------------------------------------
## Test check: check_plausibility_mfaz() ----

local(
  {
    ### Input data ----
    df <- anthro.01 |>
      process_age(
        svdate = "dos",
        birdate = "dob",
        age = age
      ) |>
      process_muac_data(
        sex = sex,
        muac = muac,
        age = "age",
        .recode_sex = TRUE,
        .recode_muac = TRUE,
        unit = "cm"
      ) |>
      check_plausibility_mfaz(
        flags = flag_mfaz,
        sex = sex,
        muac = muac,
        age = age,
        area = area
      )

    ### The test ----
    testthat::test_that(
      "evaluate_quality_mfaz() return a df with expected lentgh and width",
      {
        testthat::expect_s3_class(df, "tbl_df")
        testthat::expect_vector(df)
        testthat::expect_equal(ncol(df), 18)
        testthat::expect_equal(nrow(df), 2)
        testthat::expect_true(
          all(c(
            "area", "n", "flagged", "flagged_class", "sex_ratio",
            "sex_ratio_class", "age_ratio", "age_ratio_class",
            "dps", "dps_class", "sd", "sd_class", "skew", "skew_class",
            "kurt", "kurt_class", "quality_score", "quality_class"
          ) %in% names(df)

          )
        )
      }
    )
  }
)

## Test check: check_plausibility_whz() ----

local(
  {
    ### Input data ----
    df <- anthro.01 |>
      process_age(
        svdate = "dos",
        birdate = "dob",
        age = age
      ) |>
      process_wfhz_data(
        sex = sex,
        weight = weight,
        height = height,
        .recode_sex = TRUE
      ) |>
      check_plausibility_wfhz(
        sex = sex,
        age = age,
        weight = weight,
        height = height,
        flags = flag_wfhz,
        area = area
      )

    ### The test ----
    testthat::test_that(
      "check_plausibility_whz() return a df with expected lentgh and columns",
      {
        testthat::expect_s3_class(df, "tbl_df")
        testthat::expect_vector(df)
        testthat::expect_equal(ncol(df), 20)
        testthat::expect_equal(nrow(df), 2)
        testthat::expect_true(
          all(c(
            "area", "n", "flagged", "flagged_class", "sex_ratio",
            "sex_ratio_class", "age_ratio", "age_ratio_class",
            "dps_wgt", "dps_wgt_class", "dps_hgt", "dps_hgt_class",
            "sd", "sd_class", "skew", "skew_class", "kurt", "kurt_class",
            "quality_score", "quality_class"
          ) %in% names(df)

          )
        )
      }
    )
  }
)

## Test check: check_plausibility_crude_muac() ----

local(
  {
    ### Input data ----
    df <- anthro.01 |>
      process_muac_data(
        sex = sex,
        muac = muac,
        age = NULL,
        .recode_sex = TRUE,
        .recode_muac = FALSE,
        unit = "none"
      ) |>
      check_plausibility_muac(
        sex = sex,
        muac = muac,
        flags = flag_muac
      )

    ### The test ----
    testthat::test_that(
      "check_plausibility_muac() return a df with expected lentgh and columns",
      {
        testthat::expect_s3_class(df, "data.frame")
        testthat::expect_vector(df)
        testthat::expect_equal(ncol(df), 9)
        testthat::expect_equal(nrow(df), 1)
        testthat::expect_true(
          all(c(
            "n", "flagged", "flagged_class", "sex_ratio",
            "sex_ratio_class", "dps", "dps_class",
            "sd", "sd_class"
          ) %in% names(df)

          )
        )
      }
    )
  }
)

