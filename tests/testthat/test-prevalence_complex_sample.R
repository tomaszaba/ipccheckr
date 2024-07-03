# Test checks for functions to compute complex sample bases estimates ----------

## Test check: compute_wfhz_prevalence() ----
### When std =! problematic & !is.null(.wt) & !is.null(.edema) ----
local({

  #### Get the prevalence estimates ----
  p <- anthro.02 |>
    compute_wfhz_prevalence(.edema = edema, .wt = "wtfactor")

  #### Expected results ----
  ##### GAM estimates and uncertainty ----
  n_gam <- 121
  p_gam <- 4.3
  p_gam_lci <- 3.2
  p_gam_uci <- 5.4
  deff <- 1.58

  ##### SAM estimates and uncertainty ----
  n_sam <- 43
  p_sam <- 0.8
  p_sam_lci <- 0.2
  p_sam_uci <- 1.3

  ##### MAM estimates and uncertainty ----
  n_mam <- 78
  p_mam <- 3.5
  p_mam_lci <- 2.6
  p_mam_uci <- 4.5

  ##### Sum of weigths ----
  sum_wt <- 1752680

  #### The test ----
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

### When std =! problematic & !is.null(.wt) & is.null(.edema) ----
local({

  #### Get the prevalence estimates ----
  p <- anthro.02 |>
    compute_wfhz_prevalence(.edema = NULL, .wt = "wtfactor")

  #### Expected results ----
  ##### GAM estimates and uncertainty ----
  n_gam <- 107
  p_gam <- 3.5
  p_gam_lci <- 2.6
  p_gam_uci <- 4.5
  deff <- 1.43

  ##### SAM estimates and uncertainty ----
  n_sam <- 29
  p_sam <- 0.0
  p_sam_lci <- 0.0
  p_sam_uci <- 0.0

  ##### MAM estimates and uncertainty ----
  n_mam <- 78
  p_mam <- 3.5
  p_mam_lci <- 2.6
  p_mam_uci <- 4.5

  ##### Sum of weigths ----
  sum_wt <- 1752680

  #### The test ----
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


### When std != problematic & is.null(.wt) & !is.null(.edema) ----
local({

  #### Get the prevalence estimates ----
  p <- wfhz.01 |>
    compute_wfhz_prevalence(.edema = edema)

  #### Expected results ----
  ##### GAM estimates and uncertainty ----
  n_gam <- 25
  p_gam <- 7.4
  p_gam_lci <- 3.5
  p_gam_uci <- 11.3

  ##### SAM estimates and uncertainty ----
  n_sam <- 4
  p_sam <- 0.3
  p_sam_lci <- -0.3
  p_sam_uci <- 1.0

  ##### MAM estimates and uncertainty ----
  n_mam <- 21
  p_mam <- 7.0
  p_mam_lci <- 3.1
  p_mam_uci <- 11.0

  #### The test ----
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

### When std =! problematic & !is.null(.wt) with .summary_by = province ----
local({

  #### Get the prevalence estimates ----
  p <- anthro.02 |>
    compute_wfhz_prevalence(
      .edema = edema,
      .wt = "wtfactor",
      .summary_by = province
    )

  #### Expected results for Nampula province ----
  ##### GAM estimates and uncertainty ----
  n_gam <- 80
  p_gam <- 5.9
  p_gam_lci <- 4.1
  p_gam_uci <- 7.8
  deff <- 1.51

  ##### SAM estimates and uncertainty ----
  n_sam <- 33
  p_sam <- 1.3
  p_sam_lci <- 0.3
  p_sam_uci <- 2.3

  ##### MAM estimates and uncertainty ----
  n_mam <- 47
  p_mam <- 4.7
  p_mam_lci <- 3.1
  p_mam_uci <- 6.2

  ##### Sum of weigths ----
  sum_wt <- 878704

  #### The test ----
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

### When std == problematic & !is.null(.wt) ----



### When std == problematic & is.null(.wt) ----



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

## Test check: compute_combined_prevalence() ----
### When std != problematic & muac_analysis == unweighted & !is.null(.wt) ----
local({

  #### Get prevalence estimates ----
  p <- anthro.02 |>
    compute_combined_prevalence(.edema = edema, .wt = "wtfactor")

  #### Expected results ----
  ##### combined GAM estimates and uncertainty ----
  n_cgam <- 199
  p_cgam <- 7.1
  p_cgam_lci <- 5.6
  p_cgam_uci <- 8.5
  deff <- 1.72

  ##### combined SAM estimates and uncertainty ----
  n_csam <- 68
  p_csam <- 1.5
  p_csam_lci <- 0.8
  p_csam_uci <- 2.3

  ##### combined MAM estimates and uncertainty ----
  n_cmam <- 145
  p_cmam <- 6.0
  p_cmam_lci <- 4.7
  p_cmam_uci <- 7.3

  ##### Sum of weights -----
  sum_wt <- 1738110

  #### The test ----
  testthat::test_that(
    "compute_combined_prevalence() yields correct estimates when edema and
    survey weights are supplied",
    {
      testthat::expect_equal(p[[1]][1], n_cgam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), p_cgam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), p_cgam_lci)
      testthat::expect_equal(round(p[[4]][1]*100, 1), p_cgam_uci)
      testthat::expect_equal(round(p[[5]][1], 2), deff)
      testthat::expect_equal(p[[6]][1], n_csam)
      testthat::expect_equal(round(p[[7]][1]*100, 1), p_csam)
      testthat::expect_equal(round(p[[8]][1]*100, 1), p_csam_lci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), p_csam_uci)
      testthat::expect_equal(p[[11]][1], n_cmam)
      testthat::expect_equal(round(p[[12]][1]*100, 1), p_cmam)
      testthat::expect_equal(round(p[[13]][1]*100, 1), p_cmam_lci)
      testthat::expect_equal(round(p[[14]][1]*100, 1), p_cmam_uci)
      testthat::expect_equal(round(p[[16]][1]), sum_wt)
    }
  )
})

### When std != problematic & muac_analysis == unweighted & !is.null(.wt) ----
local({

  #### Get prevalence estimates ----
  p <- anthro.02 |>
    compute_combined_prevalence(.edema = NULL, .wt = "wtfactor")

  #### Expected results ----
  ##### combined GAM estimates and uncertainty ----
  n_cgam <- 187
  p_cgam <- 6.4
  p_cgam_lci <- 5.0
  p_cgam_uci <- 7.8
  deff <- 1.67

  ##### combined SAM estimates and uncertainty ----
  n_csam <- 55
  p_csam <- 0.8
  p_csam_lci <- 0.3
  p_csam_uci <- 1.2

  ##### combined MAM estimates and uncertainty ----
  n_cmam <- 146
  p_cmam <- 6.1
  p_cmam_lci <- 4.8
  p_cmam_uci <- 7.4

  #### Sum of weights ----
  sum_wt <- 1738110

  #### The test ----
  testthat::test_that(
    "compute_combined_prevalence() yields correct estimates when edema is NULL",
    {
      testthat::expect_equal(p[[1]][1], n_cgam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), p_cgam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), p_cgam_lci)
      testthat::expect_equal(round(p[[4]][1]*100, 1), p_cgam_uci)
      testthat::expect_equal(p[[6]][1], n_csam)
      testthat::expect_equal(round(p[[7]][1]*100, 1), p_csam)
      testthat::expect_equal(round(p[[8]][1]*100, 1), p_csam_lci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), p_csam_uci)
      testthat::expect_equal(p[[11]][1], n_cmam)
      testthat::expect_equal(round(p[[12]][1]*100, 1), p_cmam)
      testthat::expect_equal(round(p[[13]][1]*100, 1), p_cmam_lci)
      testthat::expect_equal(round(p[[14]][1]*100, 1), p_cmam_uci)
      testthat::expect_equal(round(p[[16]][1]), sum_wt)
    }
  )
})


### When is.null(.wt) ----
local({

  #### Get prevalence estimates ----
  p <- anthro.02 |>
    compute_combined_prevalence(.edema = edema)

  #### Expected results ----
  ##### combined GAM estimates and uncertainty ----
  n_cgam <- 199
  p_cgam <- 6.8
  p_cgam_lci <- 5.7
  p_cgam_uci <- 8.0

  ##### combined SAM estimates and uncertainty ----
  n_csam <- 68
  p_csam <- 1.3
  p_csam_lci <- 0.8
  p_csam_uci <- 1.8

  ##### combined MAM estimates and uncertainty ----
  n_cmam <- 145
  p_cmam <- 5.9
  p_cmam_lci <- 4.8
  p_cmam_uci <- 7.0

  #### The test ----
  testthat::test_that(
    "compute_wfhz_prevalence() yields correct estimates",
    {
      testthat::expect_equal(p[[1]][1], n_cgam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), p_cgam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), p_cgam_lci)
      testthat::expect_equal(round(p[[4]][1]*100, 1), p_cgam_uci)
      testthat::expect_equal(p[[6]][1], n_csam)
      testthat::expect_equal(round(p[[7]][1]*100, 1), p_csam)
      testthat::expect_equal(round(p[[8]][1]*100, 1), p_csam_lci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), p_csam_uci)
      testthat::expect_equal(p[[11]][1], n_cmam)
      testthat::expect_equal(round(p[[12]][1]*100, 1), p_cmam)
      testthat::expect_equal(round(p[[13]][1]*100, 1), p_cmam_lci)
      testthat::expect_equal(round(p[[14]][1]*100, 1), p_cmam_uci)
    }
  )
})

### When !is.null(.wt) with .summary_by = province ----
local({

  #### Get prevalence estimates ----
  p <- anthro.02 |>
    compute_combined_prevalence(
      .edema = edema,
      .wt = "wtfactor",
      .summary_by = province
    )

  #### Expected results for Nampula province ----
  ##### GAM estimates and uncertainty ----
  n_cgam <- 121
  p_cgam <- 8.4
  p_cgam_lci <- 6.0
  p_cgam_uci <- 10.8
  deff <- 1.87

  ##### SAM estimates and uncertainty ----
  n_csam <- 47
  p_csam <- 2.0
  p_csam_lci <- 0.7
  p_csam_uci <- 3.3

  ##### MAM estimates and uncertainty ----
  n_cmam <- 80
  p_cmam <- 6.8
  p_cmam_lci <- 4.7
  p_cmam_uci <- 9.0

  ##### Sum of survey weights -----
  sum_wt <- 869504
  #### The test ----
  testthat::test_that(
    "compute_combined_prevalence() yields correct estimates when .summary_by is
    used",
    {
      testthat::expect_equal(p[[2]][1], n_cgam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), p_cgam)
      testthat::expect_equal(round(p[[4]][1]*100, 1), p_cgam_lci)
      testthat::expect_equal(round(p[[5]][1]*100, 1), p_cgam_uci)
      testthat::expect_equal(round(p[[6]][1], 2), deff)
      testthat::expect_equal(p[[7]][1], n_csam)
      testthat::expect_equal(round(p[[8]][1]*100, 1), p_csam)
      testthat::expect_equal(round(p[[9]][1]*100, 1), p_csam_lci)
      testthat::expect_equal(round(p[[10]][1]*100, 1), p_csam_uci)
      testthat::expect_equal(p[[12]][1], n_cmam)
      testthat::expect_equal(round(p[[13]][1]*100, 1), p_cmam)
      testthat::expect_equal(round(p[[14]][1]*100, 1), p_cmam_lci)
      testthat::expect_equal(round(p[[15]][1]*100, 1), p_cmam_uci)
      testthat::expect_equal(round(p[[17]][1]), sum_wt)
    }
  )
})

