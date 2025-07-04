% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks_for_lissyuse.R
\name{check_invalid_vars}
\alias{check_invalid_vars}
\title{Checks for Invalid Vars}
\usage{
check_invalid_vars(vars, lws = FALSE)
}
\arguments{
\item{vars}{A character vector.}

\item{lws}{Logical.}
}
\value{
Stops if all characters in \code{vars} are invalid, and it warns if only some are invalid.
}
\description{
Internal helper to ensure the \code{vars} argument has no invalid variable names.
}
\keyword{internal}
