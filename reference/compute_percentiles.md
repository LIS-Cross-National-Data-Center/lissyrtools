# Compute percentiles.

**\[superseded\]**

Compute the weighted percentages for a LIS or LWS variable.

It uses 'pwgt' or 'hwgt' to weight the indicator.

## Usage

``` r
compute_percentiles(
  file,
  file_name,
  variable,
  breaks = seq(0, 1, 0.1),
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

- breaks:

  A numeric vector specifying the percentiles that should be computed.
  Defaults to all deciles.

- weight:

  A string with the name of the variable in 'file' that should be used
  as sample weights.

- na.rm:

  A boolean indicating if the computation should ignore missing values
  in 'variable' and 'weight'.

## Value

A data.frame with two columns: 'percentile' and 'value'. The first
contains the label of the percentiles computed (e.g. '0.5' for median,
'0.2' for first quintile). The second contains the values of these in
the distribution of 'variable'.

## Examples

``` r
if (FALSE) { # \dontrun{
lissy_datasets <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
compute_percentiles(lissy_datasets[["fr1984h"]], variable = "dhi", na.rm = TRUE)
} # }
```
