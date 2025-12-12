# Compute Weighted Percentiles cross a List of Data Frames (with optional grouping)

Applies the
[`compute_weighted_percentiles()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/compute_weighted_percentiles.md)
function to each data frame in a named list, using a specified variable
and optional weight. Supports optional grouping via a categorical `by`
variable, and returns either percentile values or share values,
depending on the `share` argument.

## Usage

``` r
run_weighted_percentiles(
  data_list,
  var_name,
  wgt_name = NULL,
  probs = seq(0, 1, 0.25),
  type = c("type_4", "type_2"),
  share = FALSE,
  average = FALSE,
  na.rm = TRUE,
  by = NULL
)
```

## Arguments

- data_list:

  A named list of data frames, (e.g., across countries or years).

- var_name:

  A string specifying the variable name (e.g., "dhi", "pilabour") to
  compute percentiles or shares for.

- wgt_name:

  An optional string specifying the weight variable to be used. If
  `NULL`, equal weights are assumed.

- probs:

  A numeric vector of probabilities with values between 0 and 1,
  defining percentiles (if `share = FALSE`) or the brackets between
  which shares are computed (if `share = TRUE`).

- type:

  A character string indicating which percentile definition to use. Only
  used when `share = FALSE`.

  - `"type_4"`: default, linear interpolation-based of the empirical
    cdf - continuous sample quantile.

  - `"type_2"`: used in Stata commands like collapse and \_pctile,
    inverse of empirical distribution function with averaging at
    discontinuities - discontinuous sample quantile.

- share:

  Logical. If `TRUE`, returns `var_name` shares between percentile
  brackets instead of the percentile values. Default is `FALSE`. Note:
  This **always uses** `type = "type_4"` (interpolation), regardless of
  the `type` parameter. It cannot be combined with `type = "type_2"`.
  This cannot be set to `TRUE` if `average = TRUE`.

- average:

  Logical. If `TRUE`, returns `var_name` averages across percentile
  brackets instead of the percentile values. Default is `FALSE`. Note:
  This **always uses** `type = "type_4"` (interpolation), regardless of
  the `type` parameter. It cannot be combined with `type = "type_2"`.
  This cannot be set to `TRUE` if `share = TRUE`.

- na.rm:

  Logical. If `TRUE`, missing values in `var_name` or `wgt_name` are
  removed.

- by:

  Optional string giving the name of a categorical variable to split the
  data within each data frame before computing statistics.

## Value

A named list.

- If `by == NULL`, `share == FALSE` and `length(probs) == 1`: each list
  element is named by country and contains a named numeric vector, where
  the names are years and values are the computed statistics.

- If `by == NULL`, and either `share == TRUE` or `length(probs) > 1`:
  each list element is named by `ccyy` (country-year) identifiers and
  contains a named numeric vector, where the names represent the share
  intervals or the percentiles defined in `probs`.

- If `by != NULL`: the list has `ccyy` identifiers as keys. Each element
  is a sublist, named after the categories of the `by` variable. Each
  sublist contains a named numeric vector of computed statistics.

## Details

Percentiles are computed using weighted version of **quantile definition
4** from Hyndman and Fan (1996), by default, or **quantile definition
2** if specified. When `share = TRUE`, the function estimates Lorenz
ordinates by taking quantiles from the running sum of the ordered
outcome variable (divided by the total), according to the same
**quantile definition 4 only**.

## Examples

``` r
if (FALSE) { # \dontrun{  
library(lissyrtools)
library(purrr)
library(dplyr)

# Import data
my_data_list <- lissyuse(data = "es", from  = 2016)

# Retrieve the percentile estimates
percentiles_result <- my_data_list %>% 
  run_weighted_percentiles(
    var_name = "dhi",
    wgt_name = "hpopwgt",
    probs = seq(0.1, 0.9, 0.1),
    type = "type_2",
    na.rm = TRUE
)
print(percentiles_result)

# Compute the distribution shares
shares_result <- run_weighted_percentiles(
    data_list = my_data_list,
    var_name = "dhi",
    wgt_name = "hpopwgt",
    probs = seq(0, 1, 0.1),
    share = TRUE,
    na.rm = TRUE
)
print(shares_result)

# Compute averages for each distribution group 
averages_result <- run_weighted_percentiles(
    data_list = my_data_list,
    var_name = "dhi",
    wgt_name = "hpopwgt",
    probs = seq(0, 1, 0.1),
    average = TRUE,
    na.rm = TRUE
)
print(averages_result)

# Using the by option 
by_result_median <- run_weighted_percentiles(
    data_list = purrr::map(my_data_list[1:2], ~.x %>% filter(emp == 1)),
    var_name = "pi11",
    wgt_name = "ppopwgt",
    probs = 0.5,
    type = "type_4",
    na.rm = TRUE, 
    by = "region_c"
   
)
print(by_result_median)
} # }
```
