#'
#' Check IPC AMN Sample Size Requirements
#'
#' @description
#' Evidence used in [IPC](https://www.ipcinfo.org/ipcinfo-website/resources/ipc-manual/en/)
#'  comes from different sources, collected in different ways,
#'  namely: representative surveys, screenings or even data from community-based
#'  surveillance system - the sentinel sites. IPC AMN protocols have set minimum
#'  sampling a sample size requirements for each. For cluster-based
#'  representative surveys, there must be at least 25 primary sampling unit (PSUs).
#'  On screening, there ware two ways: i. exhaustive screening (door-to-door) or
#'  ii. sampled screening. For this, there should be at least three sites (i.e.,
#'  villages or communities, etc). `check_sample_size_requirements()` checks the
#'  on sampled screening.
#'
#'`check_sample_size_requirements()` helps you know if your data meets the at least
#'  IPC AMN minimum requirements. This function should be used before proceeding
#'  to checking the quality of measurements. Doing this saves you from avoid
#'  working on data that do not meet the minimum requirements, as it will not be
#'  used in any IPC analysis.
#'
#'@param df A data frame containing the required variables.
#'
#'@param cluster A vector containing the ID's of the primary sampling unit.
#'  Usually and ideally a numeric vector, but sometimes this variables may come as
#'  a character vector. Either way, `check_sample_size_requirements()` will execute
#'  the task accordingly.
#'
#'@param method The data collection method: survey, screening or sentinel sites.
#'  If you wish to check IPC AMN requirements on surveys were met, set
#'  method = "survey"; for screening set method = "screening" and for sentinel
#'  sites set method = "ssite". If by mistake a different parameter is given,
#'  an error will be thrown and the function will stop, but with a guidance on
#'  how to go about.
#'
#'@returns `check_sample_size_requirements()` returns an output of the same type
#'  as the input (data frame), but of a different size. By default, the function
#'  returns a summary of length 1 (one row), but with three new columns added to
#'  the input data frame: `psu` (for survey), or sites (for screening or sentinel
#'  sites) `observs` and `meets_ipc`. The first will store the total number of PSUs
#'  in the sample. `observs` will store the total number of rows/observations and
#'  `meets_ipc` is a logical vector to say whether or not the IPC AMN minimum
#'  criteria for sample size was met. This is flexible according to the method you
#'  select with `method = " "`.
#'
#'@examples
#'# Have an input data frame ---------------------------------------------------
#'
#'## A vector with survey or screening or sites areas ----
#'area <- c("area 1", "area 2", "area 1", "area 2", "area 2", "area 1",
#'"area 1", "area 1", "area 1", "area 1", "area 1", "area 1"
#')
#'
#'## A vector with the PSU ID's ----
#'cluster <- c(20, 20, 21, 21, 21, 1, 2, 3, 4, 40, 40, 9)
#'
#'## Bind them into a data frame ----
#'data <- data.frame(area, cluster)
#'
#'## Apply the function ----
#'### Set method = "survey" ----
#'check_sample_size_requirements(data, cluster, method = "survey")
#'
#'### Set method = "screening"
#'check_sample_size_requirements(data, cluster, method = "screening")
#'
#'### Set method = "ssite" ----
#'check_sample_size_requirements(data, cluster, method = "ssite")
#'
#'@export
#'
check_sample_size_requirements <- function(
    df,
    cluster,
    method = c("survey", "screening", "ssite")) {

  if (method == "survey") {

    ## Check if sample size requirements for surveys are met ----
    return(
      df |>
        summarise(
          psu = n_distinct(cluster),
          observs = n(),
          meets_ipc = ifelse(psu >= 25, "yes", "no")
        )
    )
  }
  if (method == "screening") {

    ## Check if sample size requirements for screenings are met ----
    return(
      df |>
        summarise(
          sites = n_distinct(cluster),
          observs = n(),
          meets_ipc = ifelse((sites >= 3 & observs >= 600), "yes", "no")
        )
    )
  }
  if (method == "ssite") {

    ## Check if sample size requirements for sentinel sites are met ----
    return(
      df |>
        summarise(
          sites = n_distinct(cluster),
          observs = n(),
          meets_ipc = ifelse((sites >= 5 & observs >= 200), "yes", "no")
        )
    )
  } else {
    stop("Oops!! You have selected a wrong method. Please pick
         between 'survey', 'screening' or 'ssite'")
  }
}
