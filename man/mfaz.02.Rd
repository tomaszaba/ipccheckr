% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{mfaz.02}
\alias{mfaz.02}
\title{A SMART survey data with MUAC}
\format{
A tibble with 303 rows and 7 columns.\tabular{ll}{
   \strong{Variable} \tab \strong{Description} \cr
   \emph{cluster} \tab Primary sampling unit \cr
   \emph{sex} \tab Sex, "m" = boys, "f" = girls \cr
   \emph{age} \tab calculated age in months with two decimal places \cr
   \emph{edema} \tab Edema, "n" = no, "y" = yes \cr
   \emph{mfaz} \tab MUAC-for-age z-scores with 3 decimal places \cr
   \emph{flag_mfaz} \tab Flagged observations. 1=flagged, 0=not flagged \cr
}
}
\usage{
mfaz.02
}
\description{
A SMART survey data collected in an anonymized location. This data has
mfaz standard deviation and age ratio within range for a normal prevalence
analysis. It is, thus, used to check if \code{compute_muac_prevalence()} performs
as designed.
}
\examples{
mfaz.02

}
\keyword{datasets}
