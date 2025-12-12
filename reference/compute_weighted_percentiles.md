# Compute Weighted Percentiles or Share of Distribution

This function computes weighted percentiles of a numeric vector, or the
share of total value within specified percentile intervals.

## Usage

``` r
compute_weighted_percentiles(
  var,
  wgt = NULL,
  probs = seq(0, 1, 0.25),
  na.rm = TRUE,
  share = FALSE,
  average = FALSE,
  type = c("type_4", "type_2")
)
```

## Arguments

- var:

  A numeric vector of values (e.g., .x\$dhi, .x\$pi11).

- wgt:

  A numeric vector of weights (e.g., .x\$hpopwgt, .x\$pwgt). Must be the
  same length as `x`.

- probs:

  A numeric vector of probabilities between 0 and 1 indicating which
  percentiles to compute. Default is `seq(0, 1, 0.25)`.

- na.rm:

  Logical; if `TRUE`, missing values in `x` and `w` are removed before
  computation. Default is `TRUE`.

- share:

  Logical; if `TRUE`, computes the share of total value (e.g., .x\$dhi)
  within each interval defined by `probs`. If `FALSE`, returns the
  percentile values. Default is `FALSE`. Note: This option always uses
  `type = "type_4"`, and can not be used toghether with
  `type = "type_2"` or `average = "TRUE"`.

- average:

  Logical; if `TRUE`, computes the weighted mean of `var` (e.g.,
  .x\$dhi) within each interval defined by `probs`. If `FALSE`, returns
  the percentile values. Default is `FALSE`. Note: This option always
  uses `type = "type_4"`, and can not be used toghether with
  `type = "type_2"` or `share = "TRUE"`.

- type:

  A character string indicating which percentile definition to use.
  Either `"type_4"` (default, linear interpolation-based of the
  empirical cdf - continuous sample quantile) or `"type_2"` (used in
  Stata commands like collapse and \_pctile, inverse of empirical
  distribution function with averaging at discontinuities -
  discontinuous sample quantile).

## Value

A named numeric vector. If `share = FALSE`, returns weighted percentiles
with names corresponding to the percentiles (e.g., "25%"). If
`share = TRUE`, returns the share of the total value in each percentile
range (e.g., "0-25%").

## Examples

``` r
if (FALSE) { # \dontrun{
data <- lissyrtools::lissyuse(data = "es22", vars = c("dhi", "age"))
compute_weighted_percentiles(data$es22$dhi, data$es22$ppopwgt)
compute_weighted_percentiles(data$es22$dhi, data$es22$ppopwgt, probs = c(0.03, 0.72, 0.48, .01))
compute_weighted_percentiles(data$es22$dhi, data$es22$ppopwgt, probs = c(0.03, 0.72, 0.48, .01), share = TRUE)
compute_weighted_percentiles(data$es22$dhi, data$es22$ppopwgt, probs = c(0.03, 0.72, 0.48, .01), average = TRUE)
} # }
```
