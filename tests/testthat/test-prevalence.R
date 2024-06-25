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
