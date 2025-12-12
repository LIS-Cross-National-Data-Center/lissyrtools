# Recode negative values to zero

**\[superseded\]** Recodes all negative values to zero for all files in
a list.

## Usage

``` r
transform_negative_values_to_zero(lissy_files, variable)
```

## Arguments

- lissy_files:

  A list of LIS or LWS files.

- variable:

  A character string with the name of the variable that should be
  transformed.

## Value

A list of tibbles with the recoded variable.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
library(magrittr)
lissy_files <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
lissy_files %<>%
    transform_negative_values_to_zero(variable = "dhi")
    } # }
```
