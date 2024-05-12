# Tests for utils --------------------------------------------------------------

### Test check: "flag_outliers()" ----
### Test check: flag_outliers with method set to "zscore" ----

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
    flags_mfaz <- flag_outliers(mfaz, method = "zscore")

    #### The test ----
    test_that(
      "flag_outliers() returns expected output",
      {
        expect_vector(
          flags_mfaz,
          size = 50
        )
        expect_equal(flags_mfaz, expected_results)
      }
    )
  }
)

### Test check: flag_outliers() with methdod set to "crude" ----
local(
  {
    #### Sample data ----
    crude <- seq(80, 270, by = 4) |>
      sample(size = 20, replace = TRUE)

    #### Expected results ----
    expected_results <- ifelse(crude < 100 | crude > 200, 1, 0)

    #### Observed results ----
    flags_crude <- flag_outliers(crude, method = "crude")

    #### The test ----
    test_that(
      "flag_outliers with method set for 'crude' return the correct output",
      {
        expect_vector(
          flags_crude,
          size = 20
        )
        expect_equal(flags_crude, expected_results)
      }
    )
  }
)

### Test check: recode_month_to_day() ----
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
    age_days <- recode_month_to_day(age_mo)
    df[["age_days"]] <- age_days

    #### The test ----
    test_that(
      "recode_month_to_day() does the job as expected",
      {
        expect_vector(
          df[["age_days"]],
          size = 18
        )
        expect_equal(df[["age_days"]], df[["expected_results"]])
      }
    )
  }
)

### Test check: get_age_in_months() ----

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
      21.059548, 32.164271, 7.425051, NA, NA, 36.862423, 21.223819,
      32.328542, 7.589322, 36.960986, 32.361396, 36.960986, 21.289528,
      32.394251, 7.655031
    )

    #### Observed results ----
    df <- df |>
      mutate(
        age_mo = get_age_in_months(DoS = df[["surv_date"]],
                                         DoB = df[["bir_date"]],
                                         age = df[["age"]]
                                         )
      )

    #### The test ----
    test_that(
      "transform_age_in_months() does the job as expected",
      {
        expect_vector(
          df[["age_mo"]],
          size = 15
        )
        expect_equal(df[["age_mo"]], expected_results)
      }
    )
  }
)

### Test check: process_age() ----

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
      "2021-05-01", "2020-12-12", "2022-04-04", "2021-05-01", "2023-05-24"
    ))

    age <- NA_integer_
    df <- data.frame(surv_date, bir_date, age)

    #### Expected results ----
    expected_results <- c(
      641.01, 978.87, 226.15, NA, NA, 1121.93, 645.88,
      984.04, 231.02, 1124.97, 984.96, 1124.97, 648.01,
      985.87, 233.15
    )

    #### Observed results ----
    df <- df |>
      process_age(
        DoS = surv_date,
        DoB = bir_date,
        age = age
      )

    #### The test ----
    test_that(
      "process_age() does the right calculation for age in days",
      {
        expect_vector(
          df[["age_day"]],
          size = 15
        )
        expect_equal(df[["age_day"]], expected_results)
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
    test_that(
      "recode_muac() works well",
      {
        expect_vector(
          muac_cm,
          size = 41
        )
        expect_equal(muac_cm, expected_results)
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
    test_that(
      "recode_muac() works well",
      {
        expect_vector(
          muac_mm,
          size = 81
        )
        expect_equal(muac_mm, expected_results)
      }
    )
  }
)

### Test check: process_muac_data() ----

local(
  {
    #### Sample data ----
    sex <- c(2, 2, 1, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1)
    muac <- c(
      165, 222, 176, 150, 219, 193, 196,
      203, 203, 129, 97, 158, 156, 215, 214
    )
    age <- c(13, 56, 53, 23, 43, 55, 25,
             16, 44, 19, 45, 36, 11, 31,26)

    data <- data.frame(sex, muac, age)

    #### Expected results ----
    expected_results <- c(
      1.757, 3.059, 0.902, 0.161, 3.786, 1.892, 3.249,
      4.217, 2.651, -1.484, -5.529, 0.117, 1.151, 4.151, 4.415
    )

    #### Observed results ----
    df <- data |>
      process_muac_data(
        sex = "sex",
        age = "age",
        muac = "muac",
        .recode_sex = FALSE,
        .recode_muac = TRUE,
        unit = "cm"
    )

    #### The Test ----
    test_that(
      "process_muac_data() works well",
      {
        expect_vector(
          df[["mfaz"]],
          size = 15
        )
        expect_equal(df[["mfaz"]], expected_results)
      }
    )
  }
)

### Test check: age_ratio_test() ----
local(
  {
    #### Observed results ----
     obsrv <- age_ratio_test(data.01[["age"]], .expectedP = 0.66)

    #### The test ----
    test_that(
      "age_ratio_test() returns a list",
      {
        expect_type(obsrv, "list")
        expect_vector(obsrv)
        expect_named(obsrv, c("p", "observedR", "observedP")
        )
      }
    )
  }
)

### Test check: remove_flags() ----
local(
  {
    #### Observed results ----
    processed_df <- data.01 |>
      process_age() |>
      process_muac_data(
        sex = "sex",
        muac = muac,
        age = age,
        .recode_sex = TRUE,
        .recode_muac = TRUE,
        unit = "cm"
      )
    processed_df[["not_flag"]] <- remove_flags(processed_df[["flags"]])

    #### The test ----
    test_that(
      "remove_flags() assign NA's when flaggs are identified",
      {
      with(
        processed_df,
        testthat::show_failure(expect_setequal(flags, not_flag))
        )
      }
    )
  }
)
