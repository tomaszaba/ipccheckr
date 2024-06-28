# Test checks: functions to perform prevalence calculations ----

## Test check: define_wasting() ----
### Type set to "wfhz" ----
local(
  {
    #### Input data ----
    x <- prev_data |>
      define_wasting(wfhz, edema, base = "wfhz") |>
      dplyr::select(gam, sam, mam)

    #### The test ----
    testthat::test_that(
      "define_wasting() executes job as expected",
      {
        testthat::expect_s3_class(x, "data.frame")
        testthat::expect_named(x, c("gam", "sam", "mam"))
        testthat::expect_vector(x$gam, size = 7740)
        testthat::expect_vector(x$sam, size = 7740)
        testthat::expect_vector(x$mam, size = 7740)
      }
    )
  }
)

#### Type set to "muac ----
local(
  {
    #### Input data ----
    x <- prev_data |>
      define_wasting(muac = muac, edema = edema, base = "muac") |>
      dplyr::select(gam, sam, mam)

    #### The test ----
    testthat::test_that(
      "define_wasting() executes job as expected",
      {
        testthat::expect_s3_class(x, "data.frame")
        testthat::expect_named(x, c("gam", "sam", "mam"))
        testthat::expect_vector(x$gam, size = 7740)
        testthat::expect_vector(x$sam, size = 7740)
        testthat::expect_vector(x$mam, size = 7740)
      }
    )
  }
)

#### Type set to "combined" ----
local(
  {
    #### Input data ----
    x <- prev_data |>
      define_wasting(wfhz, muac, edema, base = "combined") |>
      dplyr::select(cgam, csam, cmam)

    #### The test ----
    testthat::test_that(
      "define_wasting() executes job as expected",
      {
        testthat::expect_s3_class(x, "data.frame")
        testthat::expect_named(x, c("cgam", "csam", "cmam"))
        testthat::expect_vector(x$cgam, size = 7740)
        testthat::expect_vector(x$csam, size = 7740)
        testthat::expect_vector(x$cmam, size = 7740)
      }
    )
  }
)

### Test check: apply_CDC_age_weighting() ----
#### Edema set to !NULL ----

local({
  #### Input data ----
  x <- muac_data |>
    process_age(age = months) |>
    process_muac_data(
      sex = sex,
      muac = muac,
      age = "months",
      .recode_sex = TRUE,
      .recode_muac = TRUE,
      unit = "cm"
    ) |>
    subset(flags == 0) |>
    dplyr::mutate(muac = recode_muac(muac, unit = "mm"))


  #### Expected results calculated in the CDC/SMART MUAC tool ----
  expect_sam <- 0.023
  expect_mam <- 0.069

  #### Observed results ----
  obs_sam <- with(x,
                  apply_CDC_age_weighting(
                    muac = muac,
                    edema = edema,
                    age = months,
                    status = "sam")
  )
  obs_mam <- with(x,
                  apply_CDC_age_weighting(
                    muac = muac,
                    edema = edema,
                    age = months,
                    status = "mam")
  )

  #### The test ----
  testthat::test_that(
    "apply_CDC_age_weighting() works amazing",
    {
      testthat::expect_vector(obs_sam, size = 1)
      testthat::expect_vector(obs_mam, size = 1)
      testthat::expect_equal(round(obs_sam, 2), round(expect_sam, 2))
      testthat::expect_equal(round(obs_mam, 2), round(expect_mam, 2))
    }
  )
})

### Edema set to NULL ----
local({
  #### Input data ----
  x <- muac_data |>
    process_age(age = months) |>
    process_muac_data(
      sex = sex,
      muac = muac,
      age = "months",
      .recode_sex = TRUE,
      .recode_muac = TRUE,
      unit = "cm"
    ) |>
    subset(flags == 0) |>
    dplyr::mutate(
      muac = recode_muac(muac, unit = "mm"))


  #### Expected results calculated in the CDC/SMART MUAC tool ----
  expect_sam <- 0.013
  expect_mam <- 0.071

  #### Observed results ----
  obs_sam <- with(x,
                  apply_CDC_age_weighting(
                    muac = muac,
                    age = months,
                    status = "sam")
  )
  obs_mam <- with(x,
                  apply_CDC_age_weighting(
                    muac = muac,
                    age = months,
                    status = "mam")
  )

  #### The test ----
  testthat::test_that(
    "apply_CDC_age_weighting() works amazing",
    {
      testthat::expect_vector(obs_sam, size = 1)
      testthat::expect_vector(obs_mam, size = 1)
      testthat::expect_equal(round(obs_sam, 2), round(expect_sam, 2))
      testthat::expect_equal(round(obs_mam, 2), round(expect_mam, 2))
    }
  )
})
