% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/case_definitions.R
\name{define_wasting_cases_muac}
\alias{define_wasting_cases_muac}
\alias{define_wasting_cases_whz}
\alias{define_wasting_cases_combined}
\title{Case-Definition: is an observation acutely malnourished?}
\usage{
define_wasting_cases_muac(muac, edema = NULL, cases = c("gam", "sam", "mam"))

define_wasting_cases_whz(zscore, edema = NULL, cases = c("gam", "sam", "mam"))

define_wasting_cases_combined(
  zscore,
  muac,
  edema = NULL,
  cases = c("cgam", "csam", "cmam")
)
}
\arguments{
\item{muac}{An integer vector containing MUAC measurements in mm.}

\item{edema}{A character vector of "y" = Yes, "n" = No bilateral edema.
Default is NULL.}

\item{cases}{A choice of wasting case definition you wish to apply. For combined
acute malnutrition with \code{\link[=define_wasting_cases_combined]{define_wasting_cases_combined()}} cases options are:
c("cgam", "csam", "cmam").}

\item{zscore}{A double vector containing weight-for-height zscores with 3
decimal places.}
}
\value{
A numeric vector of the same size as the input vector, with values ranging
between 1=Yes and 0=No.
}
\description{
\code{\link[=define_wasting_cases_muac]{define_wasting_cases_muac()}}, \code{\link[=define_wasting_cases_whz]{define_wasting_cases_whz()}} and
\code{\link[=define_wasting_cases_combined]{define_wasting_cases_combined()}} help you get through with your wasting
case-definition for each observation. It should be used inside dplyr::mutate()
or base::transform(). It was designed to be used inside \code{\link[=define_wasting]{define_wasting()}}.
}
