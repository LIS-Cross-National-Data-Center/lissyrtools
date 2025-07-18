% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_subset.R
\name{implement_restrict_to_household_heads}
\alias{implement_restrict_to_household_heads}
\title{Apply 'transform_restrict_to_household_heads()' to a single file}
\usage{
implement_restrict_to_household_heads(file, file_name, variable)
}
\arguments{
\item{file}{A LIS or LWS file.}

\item{file_name}{The name of the LIS or LWS file.}

\item{variable}{A character vector of length one with the variable that
will be recoded to NA when relation is missing or not equal to 1000 (household head).}
}
\value{
A a file with the filtered age variable.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
Applies 'transform_restrict_to_household_heads()' to a single LIS/LWS file.
}
\keyword{internal}
