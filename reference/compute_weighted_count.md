# Compute (Weighted) Counts or Percentages for a Categorical Variable

Compute (Weighted) Counts or Percentages for a Categorical Variable

## Usage

``` r
compute_weighted_count(var, wgt = NULL, na.rm = FALSE, percent = FALSE)
```

## Arguments

- var:

  A column refering to one of the categorical variables in a LIS or LWS
  data frame.

- wgt:

  A numeric vector of weights (e.g., .x\$hpopwgt, .x\$pwgt). Must be the
  same length as `x`.

- na.rm:

  Logical; if `TRUE`, missing values in `x` and `w` are removed before
  computation. Default is `FALSE`.

- percent:

  Logical; if `TRUE`, computes weighted (or non-weighted) percentages.

## Value

A numeric vector, with category labels as names.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- lissyrtools::lissyuse(data = "de20", vars = c("dhi", "age", "educ"))
compute_weighted_count(data$de20$educ, na.rm = TRUE)
compute_weighted_count(data$de20$educ, percent = TRUE)
compute_weighted_count(data$de20$educ, na.rm = TRUE, percent = TRUE)
compute_weighted_count(data$de20$educ, data$de20$ppopwgt, na.rm = TRUE, percent = TRUE)
compute_weighted_count(data$de20$educ, data$de20$ppopwgt, percent = TRUE)
} # }
```
