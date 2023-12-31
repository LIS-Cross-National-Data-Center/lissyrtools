% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformations.R
\name{implement_top_code_with_iqr_pfile}
\alias{implement_top_code_with_iqr_pfile}
\alias{implement_top_code_with_iqr_hfile}
\alias{implement_bottom_code_with_iqr_pfile}
\alias{implement_bottom_code_with_iqr_hfile}
\title{Apply top and bottom coding with log IQR to a level-defined file}
\usage{
implement_top_code_with_iqr_pfile(
  file,
  file_name,
  variable,
  times = 3,
  variable_level = NULL,
  weight = NULL
)

implement_top_code_with_iqr_hfile(
  file,
  file_name,
  variable,
  times = 3,
  weight = NULL
)

implement_bottom_code_with_iqr_pfile(
  file,
  file_name,
  variable,
  times = 3,
  variable_level = NULL,
  weight = NULL
)

implement_bottom_code_with_iqr_hfile(
  file,
  file_name,
  variable,
  times = 3,
  weight = NULL
)
}
\arguments{
\item{file}{A tibble or data.frame with a LIS or LWS file.}

\item{file_name}{A string with the name of the LIS or LWS file.}

\item{variable}{A string with the variable to which top coding should be applied.}

\item{times}{A numeric indicating the scale factor for IQR. Defaults to 3.}

\item{variable_level}{A string with the level of the variable. Should be either 'household' or 'person'.
If NULL (default), the function will try to guess the level of the variable.
This is done by comparing the value in 'variable' with pre-set lists of variables.}

\item{weight}{A string with the name of the variable in 'file' that should be
used as sample weights.}
}
\value{
A tibble containing the file with the recoded variable.
}
\description{
Lower-level functions used within 'implement_top_code_with_iqr()' and
'implement_bottom_code_with_iqr()'.
}
\keyword{internal}
