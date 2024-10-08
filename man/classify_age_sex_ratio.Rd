% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quality_classifiers.R
\name{classify_age_sex_ratio}
\alias{classify_age_sex_ratio}
\title{Classify how much high is the difference in age ration and in sex ratio}
\usage{
classify_age_sex_ratio(p)
}
\arguments{
\item{p}{A numeric vector containing the test p-values.}
}
\value{
A character vector with the correspondent classification.
}
\description{
\code{classify_age_sex_ratio()} works on the results yielded by \code{\link[nipnTK:ageRatioTest]{nipnTK::ageRatioTest()}}.
It helps you know how much high is the statistical difference between children
age 6-29 months of those age 30-59 months. Likewise, with regard to sex,
function works on the results yielded by \code{\link[nipnTK:sexRatioTest]{nipnTK::sexRatioTest()}} to know
how much high is the difference between boy and girls in your sample data.
}
\examples{

## Have a numeric vector storing p-values ----
pvalues <- c(0, 0, 0.01, 0.011, 0.2, 0.015, 0.016, 0.017,
0.05, 0.06,0.03, 0.03, 0.04, 0.000001, 0.07
)

## Apply the function ----
classify_age_sex_ratio(pvalues)

}
