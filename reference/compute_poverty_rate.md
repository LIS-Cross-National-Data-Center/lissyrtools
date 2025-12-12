# Compute poverty rate.

**\[superseded\]**

Computes the weighted poverty rate for a LIS or LWS variable.

It uses 'pwgt' or 'hwgt' to weight the indicator.

## Usage

``` r
compute_poverty_rate(
  file,
  file_name,
  variable,
  times_median,
  weight = NULL,
  na.rm = FALSE
)
```

## Arguments

- file:

  A LIS or LWS file.

- file_name:

  The name of the LIS or LWS file.

- variable:

  A character vector of length one with the indicator that needs to be
  transformed.

- times_median:

  A number with the factor by which the median should be multiplied E.g.
  0.5 for a poverty rate of 50% the median.

- weight:

  A string with the name of the variable in 'file' that should be used
  as sample weights.

- na.rm:

  A boolean indicating if the computation should ignore missing values
  in 'variable' and 'weight'.

## Value

A tibble with two columns: 'percentile' and 'value'. The first contains
the label of the percentiles computed (e.g. '0.5' for median, '0.2' for
first quintile). The second contains the values of these in the
distribution of 'variable'.

## Examples

``` r
if (FALSE) { # \dontrun{
lissy_datasets <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
compute_poverty_rate(lissy_datasets[["fr1984h"]], file_name = "fr84h", variable = "dhi", times_median = 0.5, na.rm = TRUE)
} # }
```
