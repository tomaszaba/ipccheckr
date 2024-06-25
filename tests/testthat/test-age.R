### Test check: compute_month_to_days() ----
local(
  {
    #### Sample data ----
    age_mo <- seq(6,23)
    df <- data.frame(age_mo)

    #### Expected results ----
    df[["expected_results"]] <- c(
      182.6250, 213.0625, 243.5000, 273.9375, 304.3750, 334.8125, 365.2500,
      395.6875, 426.1250, 456.5625, 487.0000, 517.4375, 547.8750, 578.3125,
      608.7500, 639.1875, 669.6250, 700.0625
    )

    #### Observed results ----
    age_days <- compute_month_to_days(age_mo)
    df[["age_days"]] <- age_days

    #### The test ----
    testthat::test_that(
      "compute_month_to_days() does the job as expected",
      {
        testthat::expect_vector(df[["age_days"]], size = 18)
        testthat::expect_equal(df[["age_days"]], df[["expected_results"]])
      }
    )
  }
)

### Test check: compute_age_in_months() ----

local(
  {
    #### Sample data ----
    surv_date <- as.Date(c(
      "2024-01-05", "2024-01-05", "2024-01-05", "2024-01-08", "2024-01-08",
      "2024-01-08", "2024-01-10", "2024-01-10", "2024-01-10", "2024-01-11",
      "2024-01-11", "2024-01-11", "2024-01-12", "2024-01-12", "2024-01-12"
    ))
    bir_date <- as.Date(c(
      "2022-04-04", "2021-05-01", "2023-05-24", "2017-12-12", NA,
      "2020-12-12", "2022-04-04", "2021-05-01", "2023-05-24", "2020-12-12",
      "2021-05-01","2020-12-12", "2022-04-04", "2021-05-01", "2023-05-24"
    ))

    age <- NA_integer_
    df <- data.frame(surv_date, bir_date, age)

    #### Expected results ----
    expected_results <- c(
      21.06, 32.16, 7.43, NA, NA, 36.86, 21.22,
      32.33, 7.59, 36.96, 32.36, 36.96, 21.29,
      32.39, 7.66
    )

    #### Observed results ----
    df <- df |>
      mutate(
        age_mo = compute_age_in_months(surv_date = df[["surv_date"]],
                                       birth_date = df[["bir_date"]])
      )

    #### The test ----
    testthat::test_that(
      "compute_age_in_months() does the job as expected",
      {
        testthat::expect_vector(df[["age_mo"]], size = 15)
        testthat::expect_equal(df[["age_mo"]], expected_results)
      }
    )
  }
)

### Test check: process_age() ----

local(
  {
    #### Sample data ----
    df <- data.frame(
      survy_date = as.Date(c(
        "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01")),
      birthdate = as.Date(c(
        "2019-01-01", NA, "2018-03-20", "2019-11-05", "2021-04-25")),
      age = c(NA, 36, NA, NA, NA)
    )

    #### Expected results ----
    expected_results <- c(1461.12, 1095.84, 1748.17, 1153.07, 616.11)

    #### Observed results ----
    df <- df |>
      process_age(
        svdate = "survy_date",
        birdate = "birthdate",
        age = df$age
      )

    #### The test ----
    testthat::test_that(
      "process_age() does the right calculation for age in days",
      {
        testthat::expect_vector(df[["age_days"]], size = 5)
        testthat::expect_equal(df[["age_days"]], expected_results)
      }
    )
  }
)

### Test check: age_ratio_test() ----
local(
  {
    #### Observed results ----
    obsrv <- age_ratio_test(anthro_data[["age"]], .expectedP = 0.66)

    #### The test ----
    testthat::test_that(
      "age_ratio_test() returns a list",
      {
        testthat::expect_type(obsrv, "list")
        testthat::expect_vector(obsrv)
        testthat::expect_named(obsrv, c("p", "observedR", "observedP")
        )
      }
    )
  }
)
