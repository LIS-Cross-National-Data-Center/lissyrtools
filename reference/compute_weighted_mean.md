# Compute (Weighted) Means for a Variable

Compute (Weighted) Means for a Variable

## Usage

``` r
compute_weighted_mean(var, wgt = NULL, na.rm = TRUE)
```

## Arguments

- var:

  A column refering to one of the variables in a LIS or LWS data frame.

- wgt:

  A numeric vector of weights (e.g., .x\$hpopwgt, .x\$pwgt). Must be the
  same length as `x`.

- na.rm:

  Logical; if `TRUE`, missing values in `x` and `w` are removed before
  computation. Default is `TRUE`.

## Value

A numeric vector.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- lissyrtools::lissyuse(data = "de20", vars = c("dhi", "age", "educ"))
compute_weighted_mean(data$de20$age, na.rm = TRUE)
compute_weighted_mean(data$de20$dhi, data$de20$hwgt)
} # }
```
