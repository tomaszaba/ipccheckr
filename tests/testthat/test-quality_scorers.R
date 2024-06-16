# Tests check: Quality scorers -------------------------------------------------

## Test check: assign_penalty_points_flags_and_sd() ----

local(
  {
    ### Sample data ----
    class_sd_mfaz <- c(
      "Problematic", "Excellent", "Good", "Good", "Problematic",
      "Excellent", "Problematic", "Excellent", "Problematic", "Problematic",
      "Good", "Good", "Excellent", "Problematic", "Good"
    )

    ### Expected results ----
    expected_sd_mfaz_scores <- c(
      20, 0, 5, 5, 20, 0, 20,
      0, 20, 20, 5, 5, 0, 20, 5
      )

    ### Observed results ----
    scores_sd_mfaz <- assign_penalty_points_flags_and_sd(class_sd_mfaz)

    ### The test ----
    testthat::test_that(
      "scores are equal to expected",
      {
        testthat::expect_equal(scores_sd_mfaz, expected_sd_mfaz_scores)
        testthat::expect_type(scores_sd_mfaz, "double")
      }
    )
  }
)


## Test check: assign_penalty_points_skew_kurt() ----

local(
  {
    class_skew_kurt <- c(
      "Excellent", "Excellent", "Good", "Excellent", "Good",
      "Problematic", "Acceptable", "Excellent", "Problematic",
      "Good", "Problematic", "Problematic", "Problematic",
      "Excellent", "Problematic"
    )

    ### Expected results ----
    expected_skew_kurt_scores <- c(
      0, 0, 1, 0, 1, 5, 3,
      0, 5, 1, 5, 5, 5, 0, 5
      )

    ### Observed results ----
    scores_skew_kurt <- assign_penalty_points_skew_kurt(class_skew_kurt)

    ### The test ----
    testthat::test_that(
      "scores are equal to expected",
      {
        testthat::expect_equal(scores_skew_kurt, expected_skew_kurt_scores)
        testthat::expect_type(scores_skew_kurt, "double")
      }
    )
  }
)

## Test check: get_quality_score() ----
local(
  {
    ### Sample data ----
    df <- data.frame(
      flagged_class = "Problematic",
      sd_class = "Good",
      skew_class = "Excellent",
      kurt_class = "Excellent",
      dps_class = "Acceptable",
      age_ratio_class = "Good",
      sex_ratio_class = "Problematic"
    )

    ### Expected results ----
   expected_score <- 41

    ### Observed results ----
    overall_score <- compute_quality_score(df, type = "mfaz")

    ### The test ----
    testthat::test_that(
      "get_quality_score() return the correct values for a given classification",
      {
        testthat::expect_vector(overall_score)
        testthat::expect_equal(overall_score, expected_score)

      }
    )
  }
)
