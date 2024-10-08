# Test checks: Pretty outputers ------------------------------------------------

## Test check: generate_pretty_table_muac() ----

local(
  {
    quality <- anthro.01 |>
      process_muac_data(
      sex = sex,
      muac = muac,
      .recode_sex = TRUE,
      .recode_muac = FALSE,
      unit = "none"
      ) |>
      check_plausibility_muac(
        flags = flag_muac,
        sex = sex,
        muac = muac
      )|>
      generate_pretty_table_muac()

    ### The test ----
    testthat::test_that(
      "generate_pretty_table_muac() works",
      {
        testthat::expect_s3_class(quality, "data.frame")
        testthat::expect_equal(ncol(quality), 9)
        testthat::expect_equal(nrow(quality), 1)
        testthat::expect_true(
          all(c(
            "Total children", "Flagged data (%)", "Class. of flagged data",
            "Sex ratio (p)", "Class. of sex ratio", "DPS(#)", "Class. of DPS",
            "Standard Dev* (#)", "Class. of standard dev"
            ) %in% names(quality)
          )
        )
      }
    )
  }
)

## Test check: generate_pretty_table_mfaz() ----
local(
  {
    quality <- anthro.01 |>
      process_age(
        svdate = "dos",
        birdate = "dob",
        age = age
      ) |>
      process_muac_data(
        sex = sex,
        age = "age",
        muac = muac,
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
      ) |>
      generate_pretty_table_mfaz()

    ### The test ----
    testthat::test_that(
      "generate_pretty_table_mfaz() works",
      {
        testthat::expect_s3_class(quality, "tbl_df")
        testthat::expect_equal(ncol(quality), 18)
        testthat::expect_equal(nrow(quality), 2)
        testthat::expect_true(
          all(c("Area", "Total children", "Flagged data (%)",
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

## Test check: generate_pretty_table_whz() ----

local(
  {
    quality <- anthro.01 |>
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
        flags = flag_wfhz,
        sex = sex,
        age = age,
        weight = weight,
        height = height,
        area = area
      ) |>
      generate_pretty_table_wfhz()

    ### The test ----
    testthat::test_that(
      "generate_pretty_table_whz() works",
      {
        testthat::expect_s3_class(quality, "tbl_df")
        testthat::expect_equal(ncol(quality), 20)
        testthat::expect_equal(nrow(quality), 2)
        testthat::expect_true(
          all(c("Area", "Total children", "Flagged data (%)",
                "Class. of flagged data", "Sex ratio (p)", "Class. of sex ratio",
                "Age ratio (p)", "Class. of age ratio", "DPS weight (#)",
                "Class. DPS weight", "DPS height (#)", "Class. DPS height",
                "Standard Dev* (#)", "Class. of standard dev",
                "Skewness* (#)", "Class. of skewness", "Kurtosis* (#)",
                "Class. of kurtosis", "Overall score", "Overall quality"
          ) %in% names(quality)

          )
        )
      }
    )
  }
)
