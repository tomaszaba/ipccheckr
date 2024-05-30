#'
#' Check IPC AMN Sample Size Requirements
#'
#' @description
#' Evidence used in [IPC](https://www.ipcinfo.org/ipcinfo-website/resources/ipc-manual/en/)
#' comes from different sources, collected in different ways,
#' namely: representative surveys, screenings or even data from community-based
#' surveillance system - the sentinel sites. IPC AMN protocols have set minimum
#' sampling a sample size requirements for each. For cluster-based
#' representative surveys, there must be at least 25 primary sampling unit (PSUs).
#' On screening, there ware two ways: i. exhaustive screening (door-to-door) or
#' ii. sampled screening. For this, there should be at least three sites (i.e.,
#' villages or communities, etc). `check_sample_size()` checks the
#' on sampled screening.
#'
#' `check_sample_size()` helps you know if your data meets the at least
#' IPC AMN minimum requirements. This function should be used before proceeding
#' to checking the quality of measurements. Doing this saves you from avoid
#' working on data that do not meet the minimum requirements, as it will not be
#' used in any IPC analysis.
#'
#' @param df A data frame containing the required variables.
#'
#' @param .group A vector containing the ID's of the primary sampling unit.
#' Usually and ideally a numeric vector, but sometimes this variables may come as
#' a character vector. Either way, `check_sample_size()` will execute
#' the task accordingly.
#'
#' @param data_type The data collection method: survey, screening or sentinel sites.
#' If you wish to check IPC AMN requirements on surveys were met, set
#' method = "survey"; for screening set method = "screening" and for sentinel
#' sites set method = "ssite". If by mistake a different parameter is given,
#' an error will be thrown and the function will stop, but with a guidance on
#' how to go about.
#'
#' @returns `check_sample_size()` returns an output of the same type
#' as the input (data frame), but of a different size. By default, the function
#' returns a summary of length 1 (one row), but with three new columns added to
#' the input data frame: `groups` (for survey), or sites (for screening or sentinel
#' sites) `n_obs` and `sample_check`. The first will store the total number of PSUs
#' in the sample. `n_obs` will store the total number of rows/observations and
#' `sample_check` is a logical vector to say whether or not the IPC AMN minimum
#' criteria for sample size was met. This is flexible according to the method you
#' select with `data_type = " "`.
#'
#' @examples
#' # Have an input data frame ---------------------------------------------------
#'
#' ## A vector with survey or screening or sites areas ----
#' area <- c("area 1", "area 2", "area 1", "area 2", "area 2", "area 1",
#' "area 1", "area 1", "area 1", "area 1", "area 1", "area 1"
#' )
#'
#' ## A vector with the PSU ID's ----
#' cluster <- c(20, 20, 21, 21, 21, 1, 2, 3, 4, 40, 40, 9)
#'
#' ## Bind them into a data frame ----
#' data <- data.frame(area, cluster)
#'
#' ## Apply the function ----
#' ### Set data_type = "survey" ----
#' check_sample_size(data, .group = cluster, data_type = "survey")
#'
#' ### Set method = "screening"
#' check_sample_size(data, .group = cluster, data_type = "screening")
#'
#' ### Set method = "ssite" ----
#' check_sample_size(data, .group = cluster, data_type = "ssite")
#'
#' @export
#'
check_sample_size <- function(df,
                              .group,
                              data_type = c("survey", "screening", "ssite")) {

  ## Match arguments ----
  data_type <- match.arg(data_type)

  ## Summarize unique PSU's and total observations per PSU ----
  df <- df |>
    summarise(
      groups = n_distinct({{ .group }}),
      n_obs = n(),
      meet_ipc = case_when(
        data_type == "survey" & groups >= 25 ~ "yes",
        data_type == "screening" & groups >= 3 & n_obs >= 600 ~ "yes",
        data_type == "ssite" & groups >= 5 & n_obs >= 200 ~ "yes",
        .default = "no"
    )
  )
  df
}
