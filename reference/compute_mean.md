# Compute standard indicators.

**\[superseded\]**

Compute the weighted mean, median or percentile ratios for a variable.

## Usage

``` r
compute_mean(file, file_name, variable, weight = NULL, na.rm = FALSE)

compute_median(file, file_name, variable, weight = NULL, na.rm = FALSE)

compute_ratio(
  file,
  file_name,
  variable,
  ratio = c(0.9, 0.1),
  weight = NULL,
  na.rm = FALSE
)
```

## Arguments

- file:

  A tibble or data.frame with a LIS or LWS file.

- file_name:

  A string with the name of the LIS or LWS file.

- variable:

  A string with the name of the variable for which the indicator should
  be computed.

- weight:

  A string with the name of the variable in 'file' that should be used
  as sample weights.

- na.rm:

  A boolean. Indicates if NAs should be ignored. Defaults to FALSE.

- ratio:

  A vector of two numeric values between 0 and 1. Defines the
  percentiles in the numerator and denominator respectively. E.g. (0.9,
  0.1) computes the 90/10 ratio.

## Value

A numeric vector.
