# Test check: compute_wfhz_prevalence() ----
## When std =! problematic & !is.null(.wt) & !is.null(.edema) ----
local({

  ### Get the prevalence estimates ----
  p <- anthro.02 |>
    compute_wfhz_prevalence(.edema = edema, .wt = "wtfactor")

  ### Expected results ----
  #### GAM estimates and uncertainty ----
  n_gam <- 121
  p_gam <- 4.3
  p_gam_lci <- 3.2
  p_gam_uci <- 5.4
  deff <- 1.58

  #### SAM estimates and uncertainty ----
  n_sam <- 43
  p_sam <- 0.8
  p_sam_lci <- 0.2
  p_sam_uci <- 1.3

  #### MAM estimates and uncertainty ----
  n_mam <- 78
  p_mam <- 3.5
  p_mam_lci <- 2.6
  p_mam_uci <- 4.5

  #### Sum of weigths ----
  sum_wt <- 1752680

  ### The test ----
  testthat::test_that(
    "compute_wfhz_prevalence() yields correct estimates",
    {
      testthat::expect_equal(p[[1]][1], n_gam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), p_gam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), p_gam_lci)
      testthat::expect_equal(round(p[[4]][1]*100, 1), p_gam_uci)
      testthat::expect_equal(round(p[[5]][1], 2), deff)
      testthat::expect_equal(p[[6]][1], n_sam)
      testthat::expect_equal(round(p[[7]][1]*100, 1), p_sam)
      testthat::expect_equal(round(p[[8]][1]*100, 1), p_sam_lci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), p_sam_uci)
      testthat::expect_equal(p[[11]][1], n_mam)
      testthat::expect_equal(round(p[[12]][1]*100, 1), p_mam)
      testthat::expect_equal(round(p[[13]][1]*100, 1), p_mam_lci)
      testthat::expect_equal(round(p[[14]][1]*100, 1), p_mam_uci)
      testthat::expect_equal(p[[16]][1], sum_wt)
    }
  )
})

## When std =! problematic & !is.null(.wt) & is.null(.edema) ----
local({

  ### Get the prevalence estimates ----
  p <- anthro.02 |>
    compute_wfhz_prevalence(.edema = NULL, .wt = "wtfactor")

  ### Expected results ----
  #### GAM estimates and uncertainty ----
  n_gam <- 107
  p_gam <- 3.5
  p_gam_lci <- 2.6
  p_gam_uci <- 4.5
  deff <- 1.43

  #### SAM estimates and uncertainty ----
  n_sam <- 29
  p_sam <- 0.0
  p_sam_lci <- 0.0
  p_sam_uci <- 0.0

  #### MAM estimates and uncertainty ----
  n_mam <- 78
  p_mam <- 3.5
  p_mam_lci <- 2.6
  p_mam_uci <- 4.5

  #### Sum of weigths ----
  sum_wt <- 1752680

  ### The test ----
  testthat::test_that(
    "compute_wfhz_prevalence() yields correct estimates when edema is NULL",
    {
      testthat::expect_equal(p[[1]][1], n_gam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), p_gam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), p_gam_lci)
      testthat::expect_equal(round(p[[4]][1]*100, 1), p_gam_uci)
      testthat::expect_equal(round(p[[5]][1], 2), deff)
      testthat::expect_equal(p[[6]][1], n_sam)
      testthat::expect_equal(round(p[[7]][1]*100, 1), p_sam)
      testthat::expect_equal(round(p[[8]][1]*100, 1), p_sam_lci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), p_sam_uci)
      testthat::expect_equal(p[[11]][1], n_mam)
      testthat::expect_equal(round(p[[12]][1]*100, 1), p_mam)
      testthat::expect_equal(round(p[[13]][1]*100, 1), p_mam_lci)
      testthat::expect_equal(round(p[[14]][1]*100, 1), p_mam_uci)
      testthat::expect_equal(p[[16]][1], sum_wt)
    }
  )
})


## When std != problematic & is.null(.wt) & !is.null(.edema) ----
local({

  ### Get the prevalence estimates ----
  p <- wfhz.01 |>
    compute_wfhz_prevalence(.edema = edema)

  ### Expected results ----
  #### GAM estimates and uncertainty ----
  n_gam <- 25
  p_gam <- 7.4
  p_gam_lci <- 3.5
  p_gam_uci <- 11.3

  #### SAM estimates and uncertainty ----
  n_sam <- 4
  p_sam <- 0.3
  p_sam_lci <- -0.3
  p_sam_uci <- 1.0

  #### MAM estimates and uncertainty ----
  n_mam <- 21
  p_mam <- 7.0
  p_mam_lci <- 3.1
  p_mam_uci <- 11.0

  ### The test ----
  testthat::test_that(
    "compute_wfhz_prevalence() yields correct estimates when survey weights is
    NULL",
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

## When std =! problematic & !is.null(.wt) with .summary_by = province ----
local({

  ### Get the prevalence estimates ----
  p <- anthro.02 |>
    compute_wfhz_prevalence(
      .edema = edema,
      .wt = "wtfactor",
      .summary_by = province
    )

  ### Expected results for Nampula province ----
  #### GAM estimates and uncertainty ----
  n_gam <- 80
  p_gam <- 5.9
  p_gam_lci <- 4.1
  p_gam_uci <- 7.8
  deff <- 1.51

  #### SAM estimates and uncertainty ----
  n_sam <- 33
  p_sam <- 1.3
  p_sam_lci <- 0.3
  p_sam_uci <- 2.3

  #### MAM estimates and uncertainty ----
  n_mam <- 47
  p_mam <- 4.7
  p_mam_lci <- 3.1
  p_mam_uci <- 6.2

  #### Sum of weigths ----
  sum_wt <- 878704

  ### The test ----
  testthat::test_that(
    "compute_wfhz_prevalence() yields correct estimates when .summary_by is
    used",
    {
      testthat::expect_equal(p[[2]][1], n_gam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), p_gam)
      testthat::expect_equal(round(p[[4]][1]*100, 1), p_gam_lci)
      testthat::expect_equal(round(p[[5]][1]*100, 1), p_gam_uci)
      testthat::expect_equal(round(p[[6]][1], 2), deff)
      testthat::expect_equal(p[[7]][1], n_sam)
      testthat::expect_equal(round(p[[8]][1]*100, 1), p_sam)
      testthat::expect_equal(round(p[[9]][1]*100, 1), p_sam_lci)
      testthat::expect_equal(round(p[[10]][1]*100, 1), p_sam_uci)
      testthat::expect_equal(p[[12]][1], n_mam)
      testthat::expect_equal(round(p[[13]][1]*100, 1), p_mam)
      testthat::expect_equal(round(p[[14]][1]*100, 1), p_mam_lci)
      testthat::expect_equal(round(p[[15]][1]*100, 1), p_mam_uci)
      testthat::expect_equal(p[[17]][1], sum_wt)
    }
  )
})

## When std == problematic & !is.null(.wt) ----



## When std == problematic & is.null(.wt) ----
