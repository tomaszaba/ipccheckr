
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
  obs <- classify_wasting_for_cdc_approach(muac = muac_values, .edema = edema)

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
  obs <- classify_wasting_for_cdc_approach(muac = muac_values, .edema = edema)

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
                  apply_cdc_age_weighting(
                    muac = muac,
                    .edema = edema,
                    age = age,
                    status = "sam")
  )
  obs_mam <- with(x,
                  apply_cdc_age_weighting(
                    muac = muac,
                    .edema = edema,
                    age = age,
                    status = "mam")
  )

  #### The test ----
  testthat::test_that(
    "apply_cdc_age_weighting() works amazing",
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
                  apply_cdc_age_weighting(
                    muac = muac,
                    age = age,
                    status = "sam")
  )
  obs_mam <- with(x,
                  apply_cdc_age_weighting(
                    muac = muac,
                    age = age,
                    status = "mam")
  )

  #### The test ----
  testthat::test_that(
    "apply_cdc_age_weighting() works amazing",
    {
      testthat::expect_vector(obs_sam, size = 1)
      testthat::expect_vector(obs_mam, size = 1)
      testthat::expect_equal(round(obs_sam, 3), expect_sam)
      testthat::expect_equal(round(obs_mam, 2), expect_mam)
    }
  )
})

## Test check: compute_muac_prevalence() ----
#### When age_ratio & std != problematic & !is.null(.wt) & !is.null(.edema) ----
local({

  #### Get the prevalence estimates ----
  p <- anthro.02 |>
    compute_muac_prevalence(.edema = edema, .wt = "wtfactor")

  #### Expected results ----
  ##### GAM estimates and uncertainty ----
  n_gam <- 135
  p_gam <- 5.6
  p_gam_lci <- 4.3
  p_gam_uci <- 6.9
  deff <- 1.86

  ##### SAM estimates and uncertainty ----
  n_sam <- 46
  p_sam <- 1.7
  p_sam_lci <- 0.9
  p_sam_uci <- 2.4

  ##### MAM estimates and uncertainty ----
  n_mam <- 89
  p_mam <- 4.0
  p_mam_lci <- 3.0
  p_mam_uci <- 4.9

  #### The test ----
  testthat::test_that(
    "compute_muac_prevalence() yields correct estimates when edema and survey
    weights are supplied",
    {
      testthat::expect_equal(p[[1]][1], n_gam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), p_gam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), p_gam_lci)
      testthat::expect_equal(round(p[[4]][1]*100, 1), p_gam_uci)
      testthat::expect_equal(p[[6]][1], n_sam)
      testthat::expect_equal(round(p[[7]][1]*100, 1), p_sam)
      testthat::expect_equal(round(p[[8]][1]*100, 1), p_sam_lci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), p_sam_uci)
      testthat::expect_equal(p[[11]][1], n_mam)
      testthat::expect_equal(round(p[[12]][1]*100, 1), p_mam)
      testthat::expect_equal(round(p[[13]][1]*100, 1), p_mam_lci)
      testthat::expect_equal(round(p[[14]][1]*100, 1), p_mam_uci)
    }
  )
})

#### When age_ratio & std != problematic & !is.null(.wt) & !is.null(.edema) ----
local({

  #### Get the prevalence estimates ----
  p <- anthro.02 |>
    compute_muac_prevalence(.edema = NULL, .wt = "wtfactor")

  #### Expected results ----
  ##### GAM estimates and uncertainty ----
  n_gam <- 123
  p_gam <- 5.0
  p_gam_lci <- 3.8
  p_gam_uci <- 6.2
  deff <- 1.75

  ##### SAM estimates and uncertainty ----
  n_sam <- 33
  p_sam <- 0.9
  p_sam_lci <- 0.4
  p_sam_uci <- 1.5

  ##### MAM estimates and uncertainty ----
  n_mam <- 90
  p_mam <- 4.0
  p_mam_lci <- 3.1
  p_mam_uci <- 5.0

  #### The test ----
  testthat::test_that(
    "compute_muac_prevalence() yields correct estimates when edema is not
    supplied",
    {
      testthat::expect_equal(p[[1]][1], n_gam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), p_gam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), p_gam_lci)
      testthat::expect_equal(round(p[[4]][1]*100, 1), p_gam_uci)
      testthat::expect_equal(p[[6]][1], n_sam)
      testthat::expect_equal(round(p[[7]][1]*100, 1), p_sam)
      testthat::expect_equal(round(p[[8]][1]*100, 1), p_sam_lci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), p_sam_uci)
      testthat::expect_equal(p[[11]][1], n_mam)
      testthat::expect_equal(round(p[[12]][1]*100, 1), p_mam)
      testthat::expect_equal(round(p[[13]][1]*100, 1), p_mam_lci)
      testthat::expect_equal(round(p[[14]][1]*100, 1), p_mam_uci)
    }
  )
})



#### When age_ratio & std != problematic & is.null(.wt) ----
local({

  ##### Get prevalence estimates ----
  p <- anthro.02 |>
    compute_muac_prevalence(.edema = edema)

  #### Expected results ----
  ##### GAM estimates and uncertainty ----
  n_gam <- 135
  p_gam <- 5.4
  p_gam_lci <- 4.3
  p_gam_uci <- 6.5

  ##### SAM estimates and uncertainty ----
  n_sam <- 46
  p_sam <- 1.3
  p_sam_lci <- 0.8
  p_sam_uci <- 1.8

  ##### MAM estimates and uncertainty ----
  n_mam <- 89
  p_mam <- 4.1
  p_mam_lci <- 3.1
  p_mam_uci <- 5.0

  #### The test ----
  testthat::test_that(
    "compute_muac_prevalence() yields correct estimates when edema is supplied",
    {
      testthat::expect_equal(p[[1]][1], n_gam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), p_gam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), p_gam_lci)
      testthat::expect_equal(round(p[[4]][1]*100, 1), p_gam_uci)
      testthat::expect_equal(p[[6]][1], n_sam)
      testthat::expect_equal(round(p[[7]][1]*100, 1), p_sam)
      testthat::expect_equal(round(p[[8]][1]*100, 1), p_sam_lci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), p_sam_uci)
      testthat::expect_equal(p[[11]][1], n_mam)
      testthat::expect_equal(round(p[[12]][1]*100, 1), p_mam)
      testthat::expect_equal(round(p[[13]][1]*100, 1), p_mam_lci)
      testthat::expect_equal(round(p[[14]][1]*100, 1), p_mam_uci)
    }
  )
})

### When age_ratio & std != problematic & !is.null(.wt) with .summary_by = province
local({

  #### Get prevalence estimates ----
  p <- anthro.02 |>
    compute_muac_prevalence(
      .edema = edema,
      .wt = "wtfactor",
      .summary_by = province
    )

  #### Expected results for Zambezia province ----
  ##### GAM estimates and uncertainty ----
  n_gam <- 65
  p_gam <- 5.5
  p_gam_lci <- 3.8
  p_gam_uci <- 7.2
  deff <- 1.67

  ##### SAM estimates and uncertainty ----
  n_sam <- 18
  p_sam <- 1.3
  p_sam_lci <- 0.4
  p_sam_uci <- 2.2

  ##### MAM estimates and uncertainty ----
  n_mam <- 47
  p_mam <- 4.2
  p_mam_lci <- 3.0
  p_mam_uci <- 5.4

  ##### Sum of weigths ----
  sum_wt <- 880902

  #### The test ----
  testthat::test_that(
    "compute_muac_prevalence() yields correct estimates when .summary_by is
    used",
    {
      testthat::expect_equal(p[[2]][2], n_gam)
      testthat::expect_equal(round(p[[3]][2]*100, 1), p_gam)
      testthat::expect_equal(round(p[[4]][2]*100, 1), p_gam_lci)
      testthat::expect_equal(round(p[[5]][2]*100, 1), p_gam_uci)
      testthat::expect_equal(round(p[[6]][2], 2), deff)
      testthat::expect_equal(p[[7]][2], n_sam)
      testthat::expect_equal(round(p[[8]][2]*100, 1), p_sam)
      testthat::expect_equal(round(p[[9]][2]*100, 1), p_sam_lci)
      testthat::expect_equal(round(p[[10]][2]*100, 1), p_sam_uci)
      testthat::expect_equal(p[[12]][2], n_mam)
      testthat::expect_equal(round(p[[13]][2]*100, 1), p_mam)
      testthat::expect_equal(round(p[[14]][2]*100, 1), p_mam_lci)
      testthat::expect_equal(round(p[[15]][2]*100, 1), p_mam_uci)
      testthat::expect_equal(p[[17]][2], sum_wt)
    }
  )
})
