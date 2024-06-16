# Tests for utils --------------------------------------------------------------

### Test check: "flag_outliers()" ----
### Test check: flag_outliers with 'type' set to "zscore" ----

local(
  {
    #### Sample data ----
    mfaz <- seq(-0.6, 0.9, by = 0.003) |>
      sample(size = 50, replace = TRUE)

    #### Mean of mfaz ----
    mean_mfaz <- mean(mfaz)

    #### Expected results ----
    expected_results <- ifelse(
      mfaz < (mean_mfaz - 3) | mfaz > (mean_mfaz + 3), 1, 0
      )

    #### Observed results ----
    flags_mfaz <- flag_outliers(mfaz, type = "zscore")

    #### The test ----
    testthat::test_that(
      "flag_outliers() returns expected output",
      {
        testthat::expect_vector(flags_mfaz, size = 50)
        testthat::expect_equal(flags_mfaz, expected_results)
      }
    )
  }
)

### Test check: flag_outliers() with 'type' set to "crude" ----
local(
  {
    #### Sample data ----
    crude <- seq(80, 270, by = 4) |>
      sample(size = 20, replace = TRUE)

    #### Expected results ----
    expected_results <- ifelse(crude < 100 | crude > 200, 1, 0)

    #### Observed results ----
    flags_crude <- flag_outliers(crude, type = "crude")

    #### The test ----
    testthat::test_that(
      "flag_outliers with 'type' set for 'crude' return the correct output",
      {
        testthat::expect_vector(flags_crude, size = 20)
        testthat::expect_equal(flags_crude, expected_results)
      }
    )
  }
)

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

### Test check: recode_muac() ----
local(
  {
    #### Sample data ----
    muac <- seq(90, 250, by = 4)

    #### Expected results ----
    expected_results <- muac / 10

    ### Observed results ----
    muac_cm <- recode_muac(muac, unit = "cm")

    #### The test ----
    testthat::test_that(
      "recode_muac() works well",
      {
        testthat::expect_vector(muac_cm, size = 41)
        testthat::expect_equal(muac_cm, expected_results)
      }
    )
  }
)

### Test check: recode_muac() ----
local(
  {
    #### Sample data ----
    muac <- seq(9.0, 25.0, by = 0.2)

    #### Expected results ----
    expected_results <- muac * 10

    ### Observed results ----
    muac_mm <- recode_muac(muac, unit = "mm")

    #### The test ----
    testthat::test_that(
      "recode_muac() works well",
      {
        testthat::expect_vector(muac_mm, size = 81)
        testthat::expect_equal(muac_mm, expected_results)
      }
    )
  }
)

### Test check: process_muac_data() ----

local(
  {
    #### Sample data ----
    df <- data.frame(
      sex = c(2, 2, 1, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1),
      muac = c(165, 222, 176, 150, 219, 193, 196, 203, 203,
               129, 97, 158, 156, 215, 214),
      age <- c(13, 56, 53, 23, 43, 55, 25,16, 44, 19, 45, 36, 11, 31,26)
    )

    #### Expected results ----
    expected_results <- c(
      1.757, 3.057, 0.902, 0.161, 3.786, 1.892, 3.249, 4.217,
      2.651, -1.484, -5.529, 0.117, 1.151, 4.151, 4.415
    )

    #### Observed results ----
    df <- df |>
      process_age(
        svdate = NULL,
        birdate = NULL,
        age = age
      ) |>
      process_muac_data(
        sex = sex,
        muac = muac,
        age = "age",
        .recode_sex = FALSE,
        .recode_muac = TRUE,
        unit = "cm"
    )

    #### The Test ----
    testthat::test_that(
      "process_muac_data() works well",
      {
        testthat::expect_vector(df[["mfaz"]], size = 15)
        testthat::expect_vector(df[["flags"]], size = 15)
        testthat::expect_equal(df[["mfaz"]], expected_results)
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

### Test check: remove_flags() with method set to "mfaz"----
local(
  {
    #### Observed results ----
    processed_df <- anthro_data |>
      process_age(
        svdate = "dos",
        birdate = "dob",
        age = age
      ) |>
      process_muac_data(
        sex = sex,
        muac = muac,
        age = "age",
        .recode_sex = TRUE,
        .recode_muac = TRUE,
        unit = "cm"
      )
    processed_df[["not_flag"]] <- remove_flags(processed_df[["mfaz"]])$zs

    #### The test ----
    testthat::test_that(
      "remove_flags() assign NA's when flaggs are identified",
      {
      with(
        processed_df,
        testthat::expect_length(processed_df[["not_flag"]], 7740)
        )
      }
    )
  }
)

### Test check: remove_flags() with method set to "crude"----
local(
  {
    #### Observed results ----
    processed_df <- anthro_data |>
      process_age(
        svdate = "dos",
        birdate = "dob",
        age = age
      ) |>
      process_muac_data(
        sex = sex,
        muac = muac,
        age = NULL,
        .recode_sex = TRUE,
        .recode_muac = FALSE,
        unit = "none"
      )
    processed_df[["not_flag"]] <- remove_flags(processed_df[["muac"]])$cr

    #### The test ----
    testthat::test_that(
      "remove_flags() assign NA's when flaggs are identified",
      {
        with(
          processed_df,
          testthat::expect_length(processed_df[["not_flag"]], 7740)
        )
      }
    )
  }
)
