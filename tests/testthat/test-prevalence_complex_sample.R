# Test checks for functions to compute complex sample bases estimates ----------

## Test check: compute_wfhz_prevalence() ----
### When std =! problematic & !is.null(.wt) ----
local({

  #### Get the prevalence estimates ----
  p <- anthro.02 |>
    compute_wfhz_prevalence(edema = edema, .wt = "wtfactor")

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
      testthat::expect_equal(round(p[[1]][1]*100, 1), gam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), gam_lci)
      testthat::expect_equal(round(p[[3]][1]*100, 1), gam_uci)
      testthat::expect_equal(round(p[[4]][1], 2), deff)
      testthat::expect_equal(round(p[[5]][1]*100, 1), sam)
      testthat::expect_equal(round(p[[6]][1]*100, 1), sam_lci)
      testthat::expect_equal(round(p[[7]][1]*100, 1), sam_uci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), mam)
      testthat::expect_equal(round(p[[10]][1]*100, 1), mam_lci)
      testthat::expect_equal(round(p[[11]][1]*100, 1), mam_uci)
    }
  )
})

### When std != problematic & is.null(.wt) ----
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
      testthat::expect_equal(round(w[[1]][1]*100, 1), gam)
      testthat::expect_equal(round(w[[2]][1]*100, 1), gam_lci)
      testthat::expect_equal(round(w[[3]][1]*100, 1), gam_uci)
      testthat::expect_equal(round(w[[5]][1]*100, 1), sam)
      testthat::expect_equal(round(w[[6]][1]*100, 1), sam_lci)
      testthat::expect_equal(round(w[[7]][1]*100, 1), sam_uci)
      testthat::expect_equal(round(w[[9]][1]*100, 1), mam)
      testthat::expect_equal(round(w[[10]][1]*100, 1), mam_lci)
      testthat::expect_equal(round(w[[11]][1]*100, 1), mam_uci)
    }
  )
})

### When std =! problematic & !is.null(.wt) with .summary_by = province ----
local({

  #### Get the prevalence estimates ----
  p <- anthro.02 |>
    compute_wfhz_prevalence(
      edema = edema,
      .wt = "wtfactor",
      .summary_by = province
    )

  #### Expected results for Nampula province ----
  ##### GAM estimates and uncertainty ----
  Nampula_gam <- 5.9
  Nampula_gam_lci <- 4.1
  Nampula_gam_uci <- 7.8
  Nampula_deff <- 1.51

  ##### SAM estimates and uncertainty ----
  Nampula_sam <- 1.3
  Nampula_sam_lci <- 0.3
  Nampula_sam_uci <- 2.3

  ##### MAM estimates and uncertainty ----
  Nampula_mam <- 4.7
  Nampula_mam_lci <- 3.1
  Nampula_mam_uci <- 6.2

  #### The test ----
  testthat::test_that(
    "compute_wfhz_prevalence() yields correct estimates when .summary_by is
    used",
    {
      testthat::expect_equal(round(p[[2]][1]*100, 1), Nampula_gam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), Nampula_gam_lci)
      testthat::expect_equal(round(p[[4]][1]*100, 1), Nampula_gam_uci)
      testthat::expect_equal(round(p[[5]][1], 2), Nampula_deff)
      testthat::expect_equal(round(p[[6]][1]*100, 1), Nampula_sam)
      testthat::expect_equal(round(p[[7]][1]*100, 1), Nampula_sam_lci)
      testthat::expect_equal(round(p[[8]][1]*100, 1), Nampula_sam_uci)
      testthat::expect_equal(round(p[[10]][1]*100, 1), Nampula_mam)
      testthat::expect_equal(round(p[[11]][1]*100, 1), Nampula_mam_lci)
      testthat::expect_equal(round(p[[12]][1]*100, 1), Nampula_mam_uci)
    }
  )
})

### When std == problematic & !is.null(.wt) ----



### When std == problematic & is.null(.wt) ----



## Test check: compute_muac_prevalence() ----
#### When age_ratio & std != problematic & !is.null(.wt) ----
local({

  #### Get the prevalence estimates ----
  p <- anthro.02 |>
    compute_muac_prevalence(edema = edema, .wt = "wtfactor")

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
      testthat::expect_equal(round(p[[1]][1]*100, 1), gam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), gam_lci)
      testthat::expect_equal(round(p[[3]][1]*100, 1), gam_uci)
      testthat::expect_equal(round(p[[4]][1], 2), deff)
      testthat::expect_equal(round(p[[5]][1]*100, 1), sam)
      testthat::expect_equal(round(p[[6]][1]*100, 1), sam_lci)
      testthat::expect_equal(round(p[[7]][1]*100, 1), sam_uci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), mam)
      testthat::expect_equal(round(p[[10]][1]*100, 1), mam_lci)
      testthat::expect_equal(round(p[[11]][1]*100, 1), mam_uci)
    }
  )
})


#### When age_ratio & std != problematic & is.null(.wt) ----
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
      testthat::expect_equal(round(p[[1]][1]*100, 1), gam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), gam_lci)
      testthat::expect_equal(round(p[[3]][1]*100, 1), gam_uci)
      testthat::expect_equal(round(p[[5]][1]*100, 1), sam)
      testthat::expect_equal(round(p[[6]][1]*100, 1), sam_lci)
      testthat::expect_equal(round(p[[7]][1]*100, 1), sam_uci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), mam)
      testthat::expect_equal(round(p[[10]][1]*100, 1), mam_lci)
      testthat::expect_equal(round(p[[11]][1]*100, 1), mam_uci)
    }
  )
})

### When age_ratio & std != problematic & !is.null(.wt) with .summary_by = province
local({

  #### Get prevalence estimates ----
  p <- anthro.02 |>
    compute_muac_prevalence(
      edema = edema,
      .wt = "wtfactor",
      .summary_by = province
    )

  #### Expected results for Zambezia province ----
  ##### GAM estimates and uncertainty ----
  Zambezia_gam <- 5.5
  Zambezia_gam_lci <- 3.8
  Zambezia_gam_uci <- 7.2
  Zambezia_deff <- 1.67

  ##### SAM estimates and uncertainty ----
  Zambezia_sam <- 1.3
  Zambezia_sam_lci <- 0.4
  Zambezia_sam_uci <- 2.2

  ##### MAM estimates and uncertainty ----
  Zambezia_mam <- 4.2
  Zambezia_mam_lci <- 3.0
  Zambezia_mam_uci <- 5.4


  #### The test ----
  testthat::test_that(
    "compute_muac_prevalence() yields correct estimates when .summary_by is
    used",
    {
      testthat::expect_equal(round(p[[2]][2]*100, 1), Zambezia_gam)
      testthat::expect_equal(round(p[[3]][2]*100, 1), Zambezia_gam_lci)
      testthat::expect_equal(round(p[[4]][2]*100, 1), Zambezia_gam_uci)
      testthat::expect_equal(round(p[[5]][2], 2), Zambezia_deff)
      testthat::expect_equal(round(p[[6]][2]*100, 1), Zambezia_sam)
      testthat::expect_equal(round(p[[7]][2]*100, 1), Zambezia_sam_lci)
      testthat::expect_equal(round(p[[8]][2]*100, 1), Zambezia_sam_uci)
      testthat::expect_equal(round(p[[10]][2]*100, 1), Zambezia_mam)
      testthat::expect_equal(round(p[[11]][2]*100, 1), Zambezia_mam_lci)
      testthat::expect_equal(round(p[[12]][2]*100, 1), Zambezia_mam_uci)
    }
  )
})

## Test check: compute_combined_prevalence() ----
### When std != problematic & muac_analysis == unweighted & !is.null(.wt) ----
local({

  #### Get prevalence estimates ----
  p <- anthro.02 |>
    compute_combined_prevalence(edema = edema, .wt = "wtfactor")

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
    "compute_combined_prevalence() yields correct estimates",
    {
      testthat::expect_equal(round(p[[1]][1]*100, 1), cgam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), cgam_lci)
      testthat::expect_equal(round(p[[3]][1]*100, 1), cgam_uci)
      testthat::expect_equal(round(p[[4]][1], 2), deff)
      testthat::expect_equal(round(p[[5]][1]*100, 1), csam)
      testthat::expect_equal(round(p[[6]][1]*100, 1), csam_lci)
      testthat::expect_equal(round(p[[7]][1]*100, 1), csam_uci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), cmam)
      testthat::expect_equal(round(p[[10]][1]*100, 1), cmam_lci)
      testthat::expect_equal(round(p[[11]][1]*100, 1), cmam_uci)
    }
  )
})

### When is.null(.wt) ----
local({

  #### Get prevalence estimates ----
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
      testthat::expect_equal(round(p[[1]][1]*100, 1), cgam)
      testthat::expect_equal(round(p[[2]][1]*100, 1), cgam_lci)
      testthat::expect_equal(round(p[[3]][1]*100, 1), cgam_uci)
      testthat::expect_equal(round(p[[5]][1]*100, 1), csam)
      testthat::expect_equal(round(p[[6]][1]*100, 1), csam_lci)
      testthat::expect_equal(round(p[[7]][1]*100, 1), csam_uci)
      testthat::expect_equal(round(p[[9]][1]*100, 1), cmam)
      testthat::expect_equal(round(p[[10]][1]*100, 1), cmam_lci)
      testthat::expect_equal(round(p[[11]][1]*100, 1), cmam_uci)
    }
  )
})

### When !is.null(.wt) with .summary_by = province ----
local({

  #### Get prevalence estimates ----
  p <- anthro.02 |>
    compute_combined_prevalence(
      edema = edema,
      .wt = "wtfactor",
      .summary_by = province
    )

  #### Expected results for Zambezia province ----
  ##### GAM estimates and uncertainty ----
  Nampula_cgam <- 8.4
  Nampula_cgam_lci <- 6.0
  Nampula_cgam_uci <- 10.8
  Nampula_deff <- 1.87

  ##### SAM estimates and uncertainty ----
  Nampula_csam <- 2.0
  Nampula_csam_lci <- 0.7
  Nampula_csam_uci <- 3.3

  ##### MAM estimates and uncertainty ----
  Nampula_cmam <- 6.8
  Nampula_cmam_lci <- 4.7
  Nampula_cmam_uci <- 9.0


  #### The test ----
  testthat::test_that(
    "compute_combined_prevalence() yields correct estimates when .summary_by is
    used",
    {
      testthat::expect_equal(round(p[[2]][1]*100, 1), Nampula_cgam)
      testthat::expect_equal(round(p[[3]][1]*100, 1), Nampula_cgam_lci)
      testthat::expect_equal(round(p[[4]][1]*100, 1), Nampula_cgam_uci)
      testthat::expect_equal(round(p[[5]][1], 2), Nampula_deff)
      testthat::expect_equal(round(p[[6]][1]*100, 1), Nampula_csam)
      testthat::expect_equal(round(p[[7]][1]*100, 1), Nampula_csam_lci)
      testthat::expect_equal(round(p[[8]][1]*100, 1), Nampula_csam_uci)
      testthat::expect_equal(round(p[[10]][1]*100, 1), Nampula_cmam)
      testthat::expect_equal(round(p[[11]][1]*100, 1), Nampula_cmam_lci)
      testthat::expect_equal(round(p[[12]][1]*100, 1), Nampula_cmam_uci)
    }
  )
})

