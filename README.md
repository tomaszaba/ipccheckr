
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `ipccheckr`:Toolkit for Performing IPC Acute Malnutrition-related Data Checks

<!-- badges: start -->

[![R-CMD-check](https://github.com/tomaszaba/ipccheckr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tomaszaba/ipccheckr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tomaszaba/ipccheckr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tomaszaba/ipccheckr?branch=main)
<!-- badges: end -->

## Background

IPC AMN is a global tool that uses a set o protocols to classify
geographical areas into a 5-phases of severity based on the prevalence
of acute malnutrition among children aged 6-59 months. Its main
objective is to provide actionable information for decision-makers. It
is not a data collection method *per se*, therefore it relies on
evidence generated through other methods (representative surveys,
screening, sentinel sites, etc). Before any evidence is used an IPC AMN
analysis, checks must performed to ascertain the quality of data,
following the minimum quality standards set in the protocol, hence its
reliability. As such, only evidence that passes this check are allowed
to be used in an IPC analysis exercise.

## Why `ipccheckr`?

The workflow for performing such checks overly tiring as the data
“checker” needs to travel from software to softwares in this workflow:
SPSS software (mostly) to process data and export to Excel or CSV to
then import it into Emergency Nutrition Assessments (ENA) for SMART
software for the standardized data quality checks through the
plausibility check/report. This stage in the workflow need to be done
for every survey area one-by-one, then transfer the results into an
Excel spreadsheet. Then, run the prevalence analysis and transfer the
results to a spreadsheet and so on and so forth. Sometimes, a fourth
tool is used to correct for likely overestimation of acute malnutrition
prevention when using MUAC and there is an imbalanced population
distribution in the sample. This is tedious and time-consuming
especially when you are tasked to check a huge dataset.

## What does `ipccheckr` do?

`ipccheckr` comes to make your checks a joyful experience while you
navigate throughout the aforementioned analysis workflow thanks to its
array of useful utility functions that going from:

1.  Checking sampling and sample size requirements for surveys, sentinel
    sites, screening - guidance

2.  The *de facto* quality checks of measurements. These come with
    useful functions for summarizing your check findings - guidance

3.  Prevalence analysis for:

    - Surveys based on WHZ, MUAC, including reporting the combined
      prevalence with design effect and index of dispersion  
    - Screening and sentinel sites based on MUAC

4.  Provides useful functions for visualization

**IMPORTANT**: Please note that `ipccheckr` is still highly experimental
and is undergoing a lot of development. Hence, any functionalities
described below have a high likelihood of changing interface or approach
as we aim for a stable working version.

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
```

## Useful workflow with `ipccheckr`

The proposed `ipccheckr` workflow is presented below:

*To be updated with a diagram*

# Citation

If you were enticed to using `ipccheckr` package and found it useful,
please cite using the suggested citation provided by a call to
`citation` function as follows:

``` r
citation("ipccheckr")
#> To cite package 'ipccheckr' in publications use:
#> 
#>   Tomás Zaba, Ernest Guevarra (2024). _ipccheckr: Toolkit for
#>   Performing IPC Acute Malnutrition-related Data Checks_. R package
#>   version 0.0.0.9000, <https://github.com/tomaszaba/ipccheckr>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {ipccheckr: Toolkit for Performing IPC Acute Malnutrition-related Data Checks},
#>     author = {{Tomás Zaba} and {Ernest Guevarra}},
#>     year = {2024},
#>     note = {R package version 0.0.0.9000},
#>     url = {https://github.com/tomaszaba/ipccheckr},
#>   }
```
