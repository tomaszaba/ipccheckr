# Test checks for utils --------------------------------------------------------

## Test check: remove_flags() -----
### Test check: remove_flags() with method set to "mfaz"----
local({
  #### Observed results ----
  processed_df <- anthro_data |>
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
    )

  processed_df[["not_flag"]] <- remove_flags(processed_df[["mfaz"]], unit = "zscore")

  #### The test ----
  testthat::test_that(
    "remove_flags() assign NA's when flaggs are identified",
    {
      with(
        processed_df,
        testthat::expect_length(processed_df[["not_flag"]], 7740)
      )
    }
  )
})

### Test check: remove_flags() with method set to "crude"----
local({
  #### Observed results ----
  processed_df <- anthro_data |>
    process_age(
      svdate = "dos",
      birdate = "dob",
      age = age
    ) |>
    process_muac_data(
      sex = sex,
      muac = muac,
      age = NULL,
      .recode_sex = TRUE,
      .recode_muac = FALSE,
      unit = "none"
    )
  processed_df[["not_flag"]] <- remove_flags(processed_df[["muac"]], unit = "crude")

  #### The test ----
  testthat::test_that(
    "remove_flags() assign NA's when flaggs are identified",
    {
      with(
        processed_df,
        testthat::expect_length(processed_df[["not_flag"]], 7740)
      )
    }
  )
})

## Test check: wasting_cases_muac() ----
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
  observed_gam <- wasting_cases_muac(muac_values, cases = "gam")
  observed_sam <- wasting_cases_muac(muac_values, cases = "sam")
  observed_mam <- wasting_cases_muac(muac_values, cases = "mam")

  #### The test ----
  testthat::test_that(
    "wasting_cases_muac() defines cases properly",
    {
      testthat::expect_equal(observed_gam, expected_gam)
      testthat::expect_equal(observed_sam, expected_sam)
      testthat::expect_equal(observed_mam, expected_mam)
    }
  )
})

## Test check: wasting_cases_muac() ----
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
  observed_gam <- wasting_cases_muac(muac_values, edema, cases = "gam")
  observed_sam <- wasting_cases_muac(muac_values, edema, cases = "sam")
  observed_mam <- wasting_cases_muac(muac_values, edema, cases = "mam")

  #### The test ----
  testthat::test_that(
    "wasting_cases_muac() defines cases properly",
    {
      testthat::expect_equal(observed_gam, expected_gam)
      testthat::expect_equal(observed_sam, expected_sam)
      testthat::expect_equal(observed_mam, expected_mam)
    }
  )
})
## Test check: wasting_cases_whz() ----
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
  observed_gam <- wasting_cases_whz(whz, edema, cases = "gam")
  observed_sam <- wasting_cases_whz(whz, edema, cases = "sam")
  observed_mam <- wasting_cases_whz(whz, edema, cases = "mam")

  #### The test ----
  testthat::test_that(
    "wasting_cases_whz() defines cases properly",
    {
      testthat::expect_equal(observed_gam, expected_gam)
      testthat::expect_equal(observed_sam, expected_sam)
      testthat::expect_equal(observed_mam, expected_mam)
    }
  )
})

## Test check: wasting_cases_whz() ----
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
  observed_gam <- wasting_cases_whz(whz, edema, cases = "gam")
  observed_sam <- wasting_cases_whz(whz, edema, cases = "sam")
  observed_mam <- wasting_cases_whz(whz, edema, cases = "mam")

  #### The test ----
  testthat::test_that(
    "wasting_cases_whz() defines cases properly",
    {
      testthat::expect_equal(observed_gam, expected_gam)
      testthat::expect_equal(observed_sam, expected_sam)
      testthat::expect_equal(observed_mam, expected_mam)
    }
  )
})

## Test check: wasting_cases_combined() ----
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
  observed_cgam <- wasting_cases_combined(whz, muac_values, edema, cases = "cgam")
  observed_csam <- wasting_cases_combined(whz, muac_values, edema, cases = "csam")
  observed_cmam <- wasting_cases_combined(whz, muac_values, edema, cases = "cmam")

  #### The test ----
  testthat::test_that(
    "wasting_cases_combined() defines cases properly",
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
  observed_cgam <- wasting_cases_combined(whz, muac_values, edema, cases = "cgam")
  observed_csam <- wasting_cases_combined(whz, muac_values, edema, cases = "csam")
  observed_cmam <- wasting_cases_combined(whz, muac_values, edema, cases = "cmam")

  #### The test ----
  testthat::test_that(
    "wasting_cases_combined() defines cases properly",
    {
      testthat::expect_equal(observed_cgam, expected_cgam)
      testthat::expect_equal(observed_csam, expected_csam)
      testthat::expect_equal(observed_cmam, expected_cmam)
    }
  )
})

### Test check: normalize_zscores() -----

local({
  #### Input data ----
  zscores <- seq(-4.321, 1.123, 0.02)

  #### Mean and standard deviation of observed zscores (the input data) ----
  obs_mean_z <- mean(zscores)
  obs_std_z <- sd(zscores)

  #### Normalize zscores ----
  norm_zscores <- normalize_zscore(zscores)

  #### Mean and standard deviation of normalized zscores -----
  norm_mean_z <- mean(norm_zscores)
  norm_std_z <- sd(norm_zscores)

  #### The test ----
  testthat::test_that(
    "normalize_score() works as expected",
    {
      testthat::expect_error(expect_equal(norm_mean_z, obs_mean_z))
      testthat::expect_error(expect_equal(norm_std_z, obs_std_z))
      testthat::expect_equal(norm_mean_z, 0)
      testthat::expect_equal(norm_std_z, 1)
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

### Test check: classify_wasting() with edema available ----

local({
  #### Input data ----
  muac_values <- c(
    123, 129, 126, 113, 130, 122, 112, 124, 128,
    121, 120, 110, 114, 125, 119, 127, 117, 118, 111, 115
  )
  edema <- c(
    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0
  )

  #### Expected results ----
  expected <- c(
    "mam", "not wasted", "sam", "sam", "not wasted", "mam", "sam", "mam",
    "not wasted", "mam", "mam", "sam", "sam", "not wasted", "mam", "not wasted",
    "mam", "sam", "sam", "mam"
  )

  #### Observed results ----
  obs <- classify_wasting(muac = muac_values, edema = edema)

  #### The test ----
  testthat::test_that(
    "classify_wasting() does his job well",
    {
      testthat::expect_vector(obs, ptype = "character", size = 20)
      testthat::expect_equal(obs, expected)
    }
  )
})

### Test check: classify_wasting() with edema set to NULL ----

local({
  #### Input data ----
  muac_values <- c(
    123, 129, 126, 113, 130, 122, 112, 124, 128,
    121, 120, 110, 114, 125, 119, 127, 117, 118, 111, 115
  )
  edema <- NULL

  #### Expected results ----
  expected <- c(
    "mam", "not wasted", "not wasted", "sam", "not wasted", "mam", "sam", "mam",
    "not wasted", "mam", "mam", "sam", "sam", "not wasted", "mam", "not wasted",
    "mam", "mam", "sam", "mam"
  )

  #### Observed results ----
  obs <- classify_wasting(muac = muac_values, edema = edema)

  #### The test ----
  testthat::test_that(
    "classify_wasting() does his job well",
    {
      testthat::expect_vector(obs, ptype = "character", size = 20)
      testthat::expect_equal(obs, expected)
    }
  )
})
