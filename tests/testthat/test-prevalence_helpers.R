# Test checks for prevalence helpers used in complex sample based prevalence ---

## Test check: define_wasting_cases_muac() ----
### With edema set to NULL
local({
  #### Sample data ----
  muac_values <- c(
    123, 129, 126, 113, 130, 122, 112, 124, 128,
    121, 120, 110, 114, 125, 119, 127, 117, 118, 111, 115
  )

  #### Expected results ----
  expected_gam <- c(1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1)
  expected_sam <- c(0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0)
  expected_mam <- c(1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1)

  #### Observed results ----
  observed_gam <- define_wasting_cases_muac(muac_values, cases = "gam")
  observed_sam <- define_wasting_cases_muac(muac_values, cases = "sam")
  observed_mam <- define_wasting_cases_muac(muac_values, cases = "mam")

  #### The test ----
  testthat::test_that(
    "define_wasting_cases_muac() defines cases properly",
    {
      testthat::expect_equal(observed_gam, expected_gam)
      testthat::expect_equal(observed_sam, expected_sam)
      testthat::expect_equal(observed_mam, expected_mam)
    }
  )
})

## Test check: define_wasting_cases_muac() ----
### With edema ----
local({
  #### Sample data ----
  muac_values <- c(
    123, 129, 126, 113, 130, 122, 112, 124, 128,
    121, 120, 110, 114, 125, 119, 127, 117, 118, 111, 115
  )

  edema <- c(
    "n", "n", "y", "n", "n", "n", "n", "n", "n", "n", "n", "n",
    "n", "n", "n", "n", "n", "y", "y", "n"
  )

  #### Expected results ----
  expected_gam <- c(1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1)
  expected_sam <- c(0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0)
  expected_mam <- c(1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1)

  #### Observed results ----
  observed_gam <- define_wasting_cases_muac(muac_values, edema, cases = "gam")
  observed_sam <- define_wasting_cases_muac(muac_values, edema, cases = "sam")
  observed_mam <- define_wasting_cases_muac(muac_values, edema, cases = "mam")

  #### The test ----
  testthat::test_that(
    "define_wasting_cases_muac() defines cases properly",
    {
      testthat::expect_equal(observed_gam, expected_gam)
      testthat::expect_equal(observed_sam, expected_sam)
      testthat::expect_equal(observed_mam, expected_mam)
    }
  )
})
## Test check: define_wasting_cases_whz() ----
### With edema ----
local({
  #### Sample data ----
  whz <- c(
    -0.958, -2.410, -0.232, -2.289, -3.015, -1.563, -2.773, -1.442,
    -2.652, -3.257, -2.531, -2.894, -0.595, -3.378, -1.321, -2.047,
    -0.353, -0.474, -1.200, -1.079
  )
  edema <- c(
    "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n",
    "n", "n", "n", "n", "n", "y", "y", "n"
  )

  #### Expected results ----
  expected_gam <- c(0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0)
  expected_sam <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0)
  expected_mam <- c(0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0)

  #### Observed results ----
  observed_gam <- define_wasting_cases_whz(whz, edema, cases = "gam")
  observed_sam <- define_wasting_cases_whz(whz, edema, cases = "sam")
  observed_mam <- define_wasting_cases_whz(whz, edema, cases = "mam")

  #### The test ----
  testthat::test_that(
    "define_wasting_cases_whz() defines cases properly",
    {
      testthat::expect_equal(observed_gam, expected_gam)
      testthat::expect_equal(observed_sam, expected_sam)
      testthat::expect_equal(observed_mam, expected_mam)
    }
  )
})

## Test check: define_wasting_cases_whz() ----
### With edema set to NULL ----
local({
  #### Sample data ----
  whz <- c(
    -0.958, -2.410, -0.232, -2.289, -3.015, -1.563, -2.773, -1.442,
    -2.652, -3.257, -2.531, -2.894, -0.595, -3.378, -1.321, -2.047,
    -0.353, -0.474, -1.200, -1.079
  )

  edema <- NULL

  #### Expected results ----
  expected_gam <- c(0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0)
  expected_sam <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
  expected_mam <- c(0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0)

  #### Observed results ----
  observed_gam <- define_wasting_cases_whz(whz, edema, cases = "gam")
  observed_sam <- define_wasting_cases_whz(whz, edema, cases = "sam")
  observed_mam <- define_wasting_cases_whz(whz, edema, cases = "mam")

  #### The test ----
  testthat::test_that(
    "define_wasting_cases_whz() defines cases properly",
    {
      testthat::expect_equal(observed_gam, expected_gam)
      testthat::expect_equal(observed_sam, expected_sam)
      testthat::expect_equal(observed_mam, expected_mam)
    }
  )
})

## Test check: define_wasting_cases_combined() ----
### With edema ----

local({
  #### Sample data ----
  whz <- c(
    -0.958, -2.410, -0.232, -2.289, -3.015, -1.563, -2.773, -1.442,
    -2.652, -3.257, -2.531, -2.894, -0.595, -3.378, -1.321, -2.047,
    -0.353, -0.474, -1.200, -1.079
  )
  muac_values <- c(
    123, 129, 126, 113, 130, 122, 112, 124, 128,
    121, 120, 110, 114, 125, 119, 127, 117, 118, 111, 115
  )
  edema <- c(
    "n", "n", "y", "n", "n", "n", "n", "n", "n", "n", "n", "n",
    "n", "n", "n", "n", "n", "y", "y", "n"
  )

  #### Expected results ----
  expected_cgam <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  expected_csam <- c(0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0)
  expected_cmam <- c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1)

  #### Observed results ----
  observed_cgam <- define_wasting_cases_combined(whz, muac_values, edema, cases = "cgam")
  observed_csam <- define_wasting_cases_combined(whz, muac_values, edema, cases = "csam")
  observed_cmam <- define_wasting_cases_combined(whz, muac_values, edema, cases = "cmam")

  #### The test ----
  testthat::test_that(
    "define_wasting_cases_combined() defines cases properly",
    {
      testthat::expect_equal(observed_cgam, expected_cgam)
      testthat::expect_equal(observed_csam, expected_csam)
      testthat::expect_equal(observed_cmam, expected_cmam)
    }
  )
})

### With edema set to NULL ----

local({
  #### Sample data ----
  whz <- c(
    -0.958, -2.410, -0.232, -2.289, -3.015, -1.563, -2.773, -1.442,
    -2.652, -3.257, -2.531, -2.894, -0.595, -3.378, -1.321, -2.047,
    -0.353, -0.474, -1.200, -1.079
  )
  muac_values <- c(
    123, 129, 126, 113, 130, 122, 112, 124, 128,
    121, 120, 110, 114, 125, 119, 127, 117, 118, 111, 115
  )
  edema <- NULL

  #### Expected results ----
  expected_cgam <- c(1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  expected_csam <- c(0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0)
  expected_cmam <- c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1)

  #### Observed results ----
  observed_cgam <- define_wasting_cases_combined(whz, muac_values, edema, cases = "cgam")
  observed_csam <- define_wasting_cases_combined(whz, muac_values, edema, cases = "csam")
  observed_cmam <- define_wasting_cases_combined(whz, muac_values, edema, cases = "cmam")

  #### The test ----
  testthat::test_that(
    "define_wasting_cases_combined() defines cases properly",
    {
      testthat::expect_equal(observed_cgam, expected_cgam)
      testthat::expect_equal(observed_csam, expected_csam)
      testthat::expect_equal(observed_cmam, expected_cmam)
    }
  )
})

### Test check: tell_muac_analysis_strategy() ----

local({
  ### Input data ----
  age_ratio_class_1 <- "Problematic"
  age_ratio_class_2 <- "Good"
  std_class_1 <- "Excellent"
  std_class_2 <- "Problematic"

  ### Expected results ----
  expected_1 <- "weighted"
  expected_2 <- "missing"
  expected_3 <- "unweighted"

  ### Observed results ----
  obs_1 <- tell_muac_analysis_strategy(age_ratio_class_1, std_class_1)
  obs_2 <- tell_muac_analysis_strategy(age_ratio_class_1, std_class_2)
  obs_3 <- tell_muac_analysis_strategy(age_ratio_class_2, std_class_1)

  ### The test ----
  testthat::test_that(
    "tell_muac_analysis_strategy() works",
    {
      testthat::expect_equal(obs_1, expected_1)
      testthat::expect_equal(obs_2, expected_2)
      testthat::expect_equal(obs_3, expected_3)
    }
  )
})


## Test check: define_wasting() ----
### Type set to "wfhz" ----
local(
  {
    #### Input data ----
    x <- wfhz.01 |>
      define_wasting(wfhz, edema, base = "wfhz") |>
      dplyr::select(gam, sam, mam)

    #### The test ----
    testthat::test_that(
      "define_wasting() executes job as expected",
      {
        testthat::expect_s3_class(x, "data.frame")
        testthat::expect_named(x, c("gam", "sam", "mam"))
        testthat::expect_vector(x$gam, size = 303)
        testthat::expect_vector(x$sam, size = 303)
        testthat::expect_vector(x$mam, size = 303)
      }
    )
  }
)

#### Type set to "muac ----
local(
  {
    #### Input data ----
    x <- mfaz.02 |>
      define_wasting(muac = muac, edema = edema, base = "muac") |>
      dplyr::select(gam, sam, mam)

    #### The test ----
    testthat::test_that(
      "define_wasting() executes job as expected",
      {
        testthat::expect_s3_class(x, "data.frame")
        testthat::expect_named(x, c("gam", "sam", "mam"))
        testthat::expect_vector(x$gam, size = 303)
        testthat::expect_vector(x$sam, size = 303)
        testthat::expect_vector(x$mam, size = 303)
      }
    )
  }
)

#### Type set to "combined" ----
local(
  {
    #### Input data ----
    x <- anthro.02 |>
      define_wasting(wfhz, muac, edema, base = "combined") |>
      dplyr::select(cgam, csam, cmam)

    #### The test ----
    testthat::test_that(
      "define_wasting() executes job as expected",
      {
        testthat::expect_s3_class(x, "data.frame")
        testthat::expect_named(x, c("cgam", "csam", "cmam"))
        testthat::expect_vector(x$cgam, size = 2267)
        testthat::expect_vector(x$csam, size = 2267)
        testthat::expect_vector(x$cmam, size = 2267)
      }
    )
  }
)
