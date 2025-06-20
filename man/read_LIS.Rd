% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_lissy_files.R
\name{read_LIS}
\alias{read_LIS}
\title{Read LIS}
\usage{
read_LIS(LIS_DIR, file_name, col_select = NULL)
}
\arguments{
\item{LIS_DIR}{A string with the directory where the file to read is stored.}

\item{file_name}{A string with the name of the file to import in 'ccyydl' or 'ccyyyydl' format. E.g 'fr84ih', 'fr1984ih'.}

\item{col_select}{A string vector with the names of the variables to read. If NULL (default), all variables in the file are read.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}}
A modified version of the original read.LIS function.

Is used as a lower-level function to read single files in 'read_lissy_files'
and 'read_lissy_files_locally'.

Contains the following amendments to the original function:
\itemize{

\item used as a lower-level function that takes a single file name and reads
a single file at a time.
\item uses 'haven::read_dta()' function to read the data instead of 'readstata13::read.dta13()'.
\item incorporates the code from the previous lower-level function 'read.LIS.data' (modified).
\item more informative errors when the file name is passed is not correct.
\item more informative errors when there are multiple possible files to import.
\item 'subset' argument is eliminated as not so many users might be
selecting subsets of rows.
}
}
\keyword{internal}
