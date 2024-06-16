# Test check: Sample Size Requirement Checker ----------------------------------

local(
  {

    ## Observed results ----
    observed <- check_sample_size(
      df = anthro_data,
      .group = "cluster",
      data_type = "survey"
      )

    ## The test ----
    testthat::test_that(
      "check_sample_size() returns a data frame object",
      {
        testthat::expect_s3_class(
          object = observed,
          class = "data.frame",
          exact = TRUE
        )
      }
    )
  }
)
