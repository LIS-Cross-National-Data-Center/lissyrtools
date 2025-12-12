# Read files locally

**\[superseded\]**

Reads multiple LIS or LWS files outside of the LISSY interface.

## Usage

``` r
read_lissy_files_locally(
  files,
  path_to_files,
  col_select = NULL,
  full_year_names = TRUE
)
```

## Arguments

- files:

  A character vector containing file names. These need to contain the
  country, year, database and level elements at the beginning of their
  name in 'ccyydl', or 'ccyyyydl' format. E.g. c("it14ih", "us16ih") or
  c("it14ih_modified.dta", "us16ih_mydata.dta")

- path_to_files:

  A character string with the directory in which the files can be found.
  The order should match that of the files passed in 'files'.

- col_select:

  A character vector with the name of the variables which should be
  selected from the files. E.g. c("hid", "dhi", "hifactor").

- full_year_names:

  A boolean. Should the name of the imported file be changed to
  'ccyyyyl' format.

## Value

A named list with the loaded files.

## Examples

``` r
if (FALSE) { # \dontrun{
read_lissy_files_locally(c("fr84h", "fr94h", "fr10h"), path_to_files = "/home/user/files/")
} # }
```
