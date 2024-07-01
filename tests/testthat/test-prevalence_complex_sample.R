# Test checks for functions to compute complex sample bases estimates ----------

## Test check: compute_wfhz_prevalence() ----
### Standard deviation =! problematic & !is.null(survey_weights) ----
local({
  p <- anthro.02 |>
    compute_wfhz_prevalence(edema = edema, wt = "wtfactor")

  #### Expected results ----
  ##### GAM estimates and uncertainty ----
  gam <- 4.3
  gam_lci <- 3.2
  gam_uci <- 5.4
  deff <- 1.58

  ##### SAM estimates and uncertainty ----
  sam <- 0.8
  sam_lci <- 0.2
  sam_uci <- 1.3

  ##### MAM estimates and uncertainty ----
  mam <- 3.5
  mam_lci <- 2.6
  mam_uci <- 4.5

  #### The test ----
  testthat::test_that(
    "compute_wfhz_prevalence() yields correct estimates",
    {
      testthat::expect_equal(round(p[[2]][1]*100, 1), gam)
      testthat::expect_equal(round(p[[2]][2]*100, 1), gam_lci)
      testthat::expect_equal(round(p[[2]][3]*100, 1), gam_uci)
      testthat::expect_equal(round(p[[2]][4], 2), deff)
      testthat::expect_equal(round(p[[2]][5]*100, 1), sam)
      testthat::expect_equal(round(p[[2]][6]*100, 1), sam_lci)
      testthat::expect_equal(round(p[[2]][7]*100, 1), sam_uci)
      testthat::expect_equal(round(p[[2]][9]*100, 1), mam)
      testthat::expect_equal(round(p[[2]][10]*100, 1), mam_lci)
      testthat::expect_equal(round(p[[2]][11]*100, 1), mam_uci)
    }
  )
})


### Standard deviation != problematic & is.null(survey_weights) ----
local({

  #### Get the prevalence estimates ----
  w <- wfhz.01 |>
    compute_wfhz_prevalence(edema = edema)

  #### Expected results ----
  ##### GAM estimates and uncertainty ----
  gam <- 7.4
  gam_lci <- 3.5
  gam_uci <- 11.3

  ##### SAM estimates and uncertainty ----
  sam <- 0.3
  sam_lci <- -0.3
  sam_uci <- 1.0

  ##### MAM estimates and uncertainty ----
  mam <- 7.0
  mam_lci <- 3.1
  mam_uci <- 11.0

  #### The test ----
  testthat::test_that(
    "compute_wfhz_prevalence() yields correct estimates",
    {
      testthat::expect_equal(round(w[[2]][1]*100, 1), gam)
      testthat::expect_equal(round(w[[2]][2]*100, 1), gam_lci)
      testthat::expect_equal(round(w[[2]][3]*100, 1), gam_uci)
      testthat::expect_equal(round(w[[2]][5]*100, 1), sam)
      testthat::expect_equal(round(w[[2]][6]*100, 1), sam_lci)
      testthat::expect_equal(round(w[[2]][7]*100, 1), sam_uci)
      testthat::expect_equal(round(w[[2]][9]*100, 1), mam)
      testthat::expect_equal(round(w[[2]][10]*100, 1), mam_lci)
      testthat::expect_equal(round(w[[2]][11]*100, 1), mam_uci)
    }
  )
})


### Standard deviation == problematic & !is.null(survey_weights) ----



### Standard deviation == problematic & is.null(survey_weights) ----



## Test check: compute_muac_prevalence() ----
### Normal complex sample analysis ----

#### !is.null(survey_weights) ----
local({
  p <- anthro.02 |>
    compute_muac_prevalence(edema = edema, wt = "wtfactor")

  #### Expected results ----
  ##### GAM estimates and uncertainty ----
  gam <- 5.6
  gam_lci <- 4.3
  gam_uci <- 6.9
  deff <- 1.86

  ##### SAM estimates and uncertainty ----
  sam <- 1.7
  sam_lci <- 0.9
  sam_uci <- 2.4

  ##### MAM estimates and uncertainty ----
  mam <- 4.0
  mam_lci <- 3.0
  mam_uci <- 4.9

  #### The test ----
  testthat::test_that(
    "compute_wfhz_prevalence() yields correct estimates",
    {
      testthat::expect_equal(round(p[[2]][1]*100, 1), gam)
      testthat::expect_equal(round(p[[2]][2]*100, 1), gam_lci)
      testthat::expect_equal(round(p[[2]][3]*100, 1), gam_uci)
      testthat::expect_equal(round(p[[2]][4], 2), deff)
      testthat::expect_equal(round(p[[2]][5]*100, 1), sam)
      testthat::expect_equal(round(p[[2]][6]*100, 1), sam_lci)
      testthat::expect_equal(round(p[[2]][7]*100, 1), sam_uci)
      testthat::expect_equal(round(p[[2]][9]*100, 1), mam)
      testthat::expect_equal(round(p[[2]][10]*100, 1), mam_lci)
      testthat::expect_equal(round(p[[2]][11]*100, 1), mam_uci)
    }
  )
})


#### is.null(survey_weights) ----
local({

  ##### Get prevalence estimates ----
  p <- anthro.02 |>
    compute_muac_prevalence(edema = edema)

  #### Expected results ----
  ##### GAM estimates and uncertainty ----
  gam <- 5.4
  gam_lci <- 4.3
  gam_uci <- 6.5

  ##### SAM estimates and uncertainty ----
  sam <- 1.3
  sam_lci <- 0.8
  sam_uci <- 1.8

  ##### MAM estimates and uncertainty ----
  mam <- 4.1
  mam_lci <- 3.1
  mam_uci <- 5.0

  #### The test ----
  testthat::test_that(
    "compute_wfhz_prevalence() yields correct estimates",
    {
      testthat::expect_equal(round(p[[2]][1]*100, 1), gam)
      testthat::expect_equal(round(p[[2]][2]*100, 1), gam_lci)
      testthat::expect_equal(round(p[[2]][3]*100, 1), gam_uci)
      testthat::expect_equal(round(p[[2]][5]*100, 1), sam)
      testthat::expect_equal(round(p[[2]][6]*100, 1), sam_lci)
      testthat::expect_equal(round(p[[2]][7]*100, 1), sam_uci)
      testthat::expect_equal(round(p[[2]][9]*100, 1), mam)
      testthat::expect_equal(round(p[[2]][10]*100, 1), mam_lci)
      testthat::expect_equal(round(p[[2]][11]*100, 1), mam_uci)
    }
  )
})

## Test check: compute_combined_prevalence() ----
### When standard deviation != "Problematic" && muac_analysis == "unweighted"

#### When !is.null(survey weigths) ----
local({
  p <- anthro.02 |>
    compute_combined_prevalence(edema = edema, wt = "wtfactor")

  #### Expected results ----
  ##### combined GAM estimates and uncertainty ----
  cgam <- 7.1
  cgam_lci <- 5.6
  cgam_uci <- 8.5
  deff <- 1.72

  ##### combined SAM estimates and uncertainty ----
  csam <- 1.5
  csam_lci <- 0.8
  csam_uci <- 2.3

  ##### combined MAM estimates and uncertainty ----
  cmam <- 6.0
  cmam_lci <- 4.7
  cmam_uci <- 7.3

  #### The test ----
  testthat::test_that(
    "compute_wfhz_prevalence() yields correct estimates",
    {
      testthat::expect_equal(round(p[[2]][1]*100, 1), cgam)
      testthat::expect_equal(round(p[[2]][2]*100, 1), cgam_lci)
      testthat::expect_equal(round(p[[2]][3]*100, 1), cgam_uci)
      testthat::expect_equal(round(p[[2]][4], 2), deff)
      testthat::expect_equal(round(p[[2]][5]*100, 1), csam)
      testthat::expect_equal(round(p[[2]][6]*100, 1), csam_lci)
      testthat::expect_equal(round(p[[2]][7]*100, 1), csam_uci)
      testthat::expect_equal(round(p[[2]][9]*100, 1), cmam)
      testthat::expect_equal(round(p[[2]][10]*100, 1), cmam_lci)
      testthat::expect_equal(round(p[[2]][11]*100, 1), cmam_uci)
    }
  )
})

#### When is.null(survey weigths) ----
local({
  p <- anthro.02 |>
    compute_combined_prevalence(edema = edema)

  #### Expected results ----
  ##### combined GAM estimates and uncertainty ----
  cgam <- 6.8
  cgam_lci <- 5.7
  cgam_uci <- 8.0

  ##### combined SAM estimates and uncertainty ----
  csam <- 1.3
  csam_lci <- 0.8
  csam_uci <- 1.8

  ##### combined MAM estimates and uncertainty ----
  cmam <- 5.9
  cmam_lci <- 4.8
  cmam_uci <- 7.0

  #### The test ----
  testthat::test_that(
    "compute_wfhz_prevalence() yields correct estimates",
    {
      testthat::expect_equal(round(p[[2]][1]*100, 1), cgam)
      testthat::expect_equal(round(p[[2]][2]*100, 1), cgam_lci)
      testthat::expect_equal(round(p[[2]][3]*100, 1), cgam_uci)
      testthat::expect_equal(round(p[[2]][5]*100, 1), csam)
      testthat::expect_equal(round(p[[2]][6]*100, 1), csam_lci)
      testthat::expect_equal(round(p[[2]][7]*100, 1), csam_uci)
      testthat::expect_equal(round(p[[2]][9]*100, 1), cmam)
      testthat::expect_equal(round(p[[2]][10]*100, 1), cmam_lci)
      testthat::expect_equal(round(p[[2]][11]*100, 1), cmam_uci)
    }
  )
})



