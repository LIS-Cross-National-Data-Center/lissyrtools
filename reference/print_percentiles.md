# Print percentiles.

**\[superseded\]** Computes and displays the percentiles and cumulative
percentiles of a variable.

## Usage

``` r
print_percentiles(
  lissy_files,
  variable,
  breaks = seq(0, 1, 0.1),
  weight = NULL,
  na.rm = FALSE
)
```

## Arguments

- lissy_files:

  A list of LIS or LWS files.

- variable:

  A character vector of length one.

- breaks:

  A numeric vector with specifying the percentiles that should be
  computed. Defaults to deciles.

- weight:

  A string with the name of the variable in 'file' that should be used
  as sample weights.

- na.rm:

  A boolean indicating if missing values should be ignored. Defaults to
  FALSE.

## Value

A tibble with percentile absolute and cummulative values.

## Examples

``` r
if (FALSE) { # \dontrun{
lissy_files <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
print_percentiles(lissy_files = lissy_files, variable = "dhi")
} # }
```
