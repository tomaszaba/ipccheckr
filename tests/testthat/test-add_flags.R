## Create a vector of values with known flagging status ----
### MFAZ-based values ----

local(
  {
    mfaz <- c(
      -2.265, -3.716, -0.658, -5.275, -0.72, -2.261, -2.264, -2.118,
      -4.451, -3.603, -0.663, -2.112, -2.107, -2.261, -1.828, 3.5
    )

    expected_results <- c(
      0, 1, 0, 1, 0, 0, 0, 0,
      1, 1, 0, 0, 0, 0, 0, 1
    )

    flags_mfaz <- add_flags(mfaz, method = "mfaz")

    testthat::test_that(
      "add_flags returns expected output",
      {
        expect_vector(
          flags_mfaz,
          size = 16
        )
        expect_equal(flags_mfaz, expected_results)
      }
    )
  }
)

