
### Test check: cdc_classify_wasting() with edema available ----

local({
  #### Input data ----
  muac_values <- c(
    123, 129, 126, 113, 130, 122, 112, 124, 128,
    121, 120, 110, 114, 125, 119, 127, 117, 118, 111, 115
  )
  edema <- c(
    "n", "n", "y", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n"
    , "n", "n", "n", "y", "y", "n"
  )

  #### Expected results ----
  expected <- c(
    "mam", "not wasted", "sam", "sam", "not wasted", "mam", "sam", "mam",
    "not wasted", "mam", "mam", "sam", "sam", "not wasted", "mam", "not wasted",
    "mam", "sam", "sam", "mam"
  )

  #### Observed results ----
  obs <- cdc_classify_wasting(muac = muac_values, edema = edema)

  #### The test ----
  testthat::test_that(
    "cdc_classify_wasting() does his job well",
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
  obs <- cdc_classify_wasting(muac = muac_values, edema = edema)

  #### The test ----
  testthat::test_that(
    "cdc_classify_wasting() does his job well",
    {
      testthat::expect_vector(obs, ptype = "character", size = 20)
      testthat::expect_equal(obs, expected)
    }
  )
})


### Test check: cdc_apply_age_weighting() ----
#### Edema set to !NULL ----

local({
  #### Input data ----
  x <- mfaz.01 |>
    process_age(age = age) |>
    process_muac_data(
      sex = sex,
      muac = muac,
      age = "age",
      .recode_sex = TRUE,
      .recode_muac = TRUE,
      unit = "cm"
    ) |>
    subset(flag_mfaz == 0) |>
    dplyr::mutate(muac = recode_muac(muac, unit = "mm"))


  #### Expected results calculated in the CDC/SMART MUAC tool ----
  expect_sam <- 0.021
  expect_mam <- 0.081

  #### Observed results ----
  obs_sam <- with(x,
                  cdc_apply_age_weighting(
                    muac = muac,
                    edema = edema,
                    age = age,
                    status = "sam")
  )
  obs_mam <- with(x,
                  cdc_apply_age_weighting(
                    muac = muac,
                    edema = edema,
                    age = age,
                    status = "mam")
  )

  #### The test ----
  testthat::test_that(
    "cdc_apply_age_weighting() works amazing",
    {
      testthat::expect_vector(obs_sam, size = 1)
      testthat::expect_vector(obs_mam, size = 1)
      testthat::expect_equal(round(obs_sam, 3), expect_sam)
      testthat::expect_equal(round(obs_mam, 3), expect_mam)
    }
  )
})

### Edema set to NULL ----
local({
  #### Input data ----
  x <- mfaz.01 |>
    process_age(age = age) |>
    process_muac_data(
      sex = sex,
      muac = muac,
      age = "age",
      .recode_sex = TRUE,
      .recode_muac = TRUE,
      unit = "cm"
    ) |>
    subset(flag_mfaz == 0) |>
    dplyr::mutate(
      muac = recode_muac(muac, unit = "mm"))


  #### Expected results calculated in the CDC/SMART MUAC tool ----
  expect_sam <- 0.014
  expect_mam <- 0.080

  #### Observed results ----
  obs_sam <- with(x,
                  cdc_apply_age_weighting(
                    muac = muac,
                    age = age,
                    status = "sam")
  )
  obs_mam <- with(x,
                  cdc_apply_age_weighting(
                    muac = muac,
                    age = age,
                    status = "mam")
  )

  #### The test ----
  testthat::test_that(
    "cdc_apply_age_weighting() works amazing",
    {
      testthat::expect_vector(obs_sam, size = 1)
      testthat::expect_vector(obs_mam, size = 1)
      testthat::expect_equal(round(obs_sam, 3), expect_sam)
      testthat::expect_equal(round(obs_mam, 2), expect_mam)
    }
  )
})


