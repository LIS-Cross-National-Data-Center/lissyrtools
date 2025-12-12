# Adjust aggregates by LIS PPPs

**\[superseded\]** Adjusts an aggregate by both the CPI and PPP.

For LWS income variables, it takes into account the reference year of
the dataset variables.

## Usage

``` r
transform_adjust_by_lisppp(
  lissy_files,
  variable,
  database = NULL,
  income_variable = NULL,
  path_to_ppp_file = "lissyrtools"
)
```

## Arguments

- lissy_files:

  A list of LIS or LWS files.

- variable:

  A character string with the name of the variable that should be
  adjusted.

- database:

  'lis' or 'lws' to specify which database the files belong to. If NULL
  (default) the function reads the 'database' attribute from the list in
  'lissy_files'.

- income_variable:

  It is only relevant for LWS files. If the file is LWS and
  'income_variable = TRUE', the function will retrieve the deflator for
  the year in which the income data was collected. This reference year
  might or might not be the same as the one when the wealth information
  was collected (i.e. the year of the file - 2010 for 'fr10wh'). The
  default NULL checks the name of the variable against the name of the
  income variables in LWS files. A vector containing the list of these
  can be found in lissyrtools::lws_income_variables. Setting the
  argument to FALSE forces the adjustment to use the same year as the
  year of the file regardless of the value passed to 'variable'.

- path_to_ppp_file:

  A character string indicating where the deflator values can be found.
  If the value is 'lissyrtools' (default), it will import the data from
  'lissyrtools'. These values are equivalent to the ones in:
  datacenter.org/resources/ppp-deflators/ . Specifying 'lissy' will read
  them from within the LISSY directory. Any other value requires the
  full path specification to the deflators file.

## Value

A list of tibbles with the adjusted variable.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
library(magrittr)
lissy_files <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
lissy_files %<>%
    transform_adjust_by_lisppp(variable = "dhi")
} # }
```
