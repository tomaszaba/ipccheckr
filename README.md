
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `ipccheckr`::Toolkit for Performing IPC Acute Malnutrition-related Data Checks

<!-- badges: start -->

[![R-CMD-check](https://github.com/tomaszaba/ipccheckr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tomaszaba/ipccheckr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tomaszaba/ipccheckr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tomaszaba/ipccheckr?branch=main)
<!-- badges: end -->

The goal of ipccheckr is to …

## Installation

`ipccheckr` is not yet on CRAN but you can install the development
version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tomaszaba/ipccheckr")
```

Then load to in memory with

``` r
library(ipccheckr)
## basic example code
```

## What does `ipccheckr` do?

Please note that `ipccheckr` is still highly experimental and is
undergoing a lot of development. Hence, any functionalities described
below have a high likelihood of changing interface or approach as we aim
for a stable working version.

As of now, `ipccheckr` provides utility functions to be used in
different, but interdependent, data analysis workflow, as described
below:

### Sampling and sample size

`ipccheckr` provides useful utility functions for verifying if the
minimum sampling and sample size requirements for surveys, screenings
and community-based sentinel sites were met.

### The *de facto* quality checks

For data that passes the above described checks, `ipccheckr` provides
utility functions for performing the *de facto* quality checks according
to the source of data and the type of method used to measure the
condition, either weight-for-height or mid upper-arm circumference
(MUAC). For the latter, the array of utility functions goes from crude
MUAC to muac-for-age zscore (MFAZ), based on IPC parameters that are
mostly supported by the SMART methodology.

### Prevalence calculations

Useful set of functions for calculating acute malnutrition prevalence
based on WHZ, with its respective descriptive statistics such as design
effect, Green’s Index to tell if acute malnutrition cases are randomly
distributed or clumped together, calculated prevalence when SD is \>1.2,
including the recalculated prevalence of SAM and MAM

For MUAC, as known and extensively documented in the literature, MUAC is
biased towards younger children (6-24 months), so that in an imbalanced
sample size, composed of too many younger children compared to older
children (24-59 months), the prevalence is likely to be overestimated
and in such situations, the observed prevalence should be corrected to
the expected population distribution, where applicable. `ipccheckr`
provides utilities to address this situation in a dynamic and automated
way, where applicable.

### Visualization of checks results

`ipccheckr` provides useful utility functions that produces presentable
output results in a table, that is, summarized for each survey area,
save-able in Excel spreadsheet and use plots to visualize the
distribution of some test results and prevalence.

## Useful workflow with `ipccheckr`

The proposed `ipccheckr` workflow is presented below:

*To be updated with a diagram*
