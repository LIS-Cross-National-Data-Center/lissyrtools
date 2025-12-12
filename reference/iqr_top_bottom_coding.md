# IQR-Based Top/Bottom Coding for a Single Variable

Internal utility that applies top and/or bottom coding to a numeric
vector using an IQR rule on the
[`log()`](https://rdrr.io/r/base/Log.html) transformation.

## Usage

``` r
iqr_top_bottom_coding(
  x,
  w = NULL,
  times = 3,
  type = c("type_4", "type_2"),
  one_sided = NULL
)
```

## Arguments

- x:

  Numeric vector. Variable to be top/bottom coded.

- w:

  Optional numeric vector of weights.

- times:

  Numeric. Multiplier for IQR to determine bounds (default is 3).

- type:

  Character. Quantile estimation type, passed to
  `compute_weighted_percentiles` function (e.g., `"type_4"`,
  `"type_2"`).

- one_sided:

  Character. `"top"`, `"bottom"`, or `NULL` (default) for two-sided
  coding.

## Value

A numeric vector with values above or below the threshold replaced (on
the original scale).

## Examples

``` r
if (FALSE) { # \dontrun{
library(lissyrtools)
library(dplyr)

data <- lissyrtools::lissyuse(data = "it16", vars = c("dhi"))
iqr_top_bottom_coding(data$it16h$dhi)
iqr_top_bottom_coding(data$it16h$dhi, data$it16h$hwgt, one_sided = "top")
} # }
```
