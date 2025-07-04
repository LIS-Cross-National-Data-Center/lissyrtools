% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformations.R
\name{implement_equivalise}
\alias{implement_equivalise}
\title{Apply equivalise by number of household members}
\usage{
implement_equivalise(file, file_name, variable, eq_scale = 0.5)
}
\arguments{
\item{file}{A LIS or LWS file.}

\item{file_name}{The name of the LIS or LWS file.}

\item{variable}{A string with the variable to which equivalisation should be applied.}

\item{eq_scale}{A real number. Defaults to 0.5. The variable will be equivalized using n_household_members^eq_scale}
}
\value{
A a file with the equivalised variable.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}}
Applies the equivalisation by number of household members in a file for a single variable.
}
\details{
To be used inside transform_equivalise().
}
\keyword{internal}
