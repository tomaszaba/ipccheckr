# Test check: Sample Size Requirement Checker ----------------------------------

##

local({
  ## Observed results ----
  x <- check_sample_size(
    df = anthro.01,
    .group = cluster,
    .data_type = "survey"
  )

  ## The test ----
  testthat::test_that(
    "check_sample_size() returns a data frame object",
    {
      testthat::expect_s3_class(object = x, class = "tbl_df", exact = FALSE)
      testthat::expect_true(all(c("groups", "n_obs", "meet_ipc") %in% names(x)))
    }
  )
})

## Sentinel sites ----


## Screening -----

