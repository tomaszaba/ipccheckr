# Test check: Sample Size Requirement Checker ----------------------------------

local(
  {

    ## Observed results ----
    observed <- check_sample_size_requirements(
      df = data.01,
      cluster = "cluster",
      method = "survey"
      )

    ## The test ----
    test_that(
      "check_sample_size_requirements() returns a data frame object",
      {
        expect_s3_class(
          object = observed,
          class = "data.frame",
          exact = TRUE
        )
      }
    )
  }
)
