# Compute the Weighted Poverty Gap (Shortfall)

Calculates the average shortfall (gap) between individual/household
income and the relative poverty line, weighted by the population weights
and restricted to those below the poverty line.

## Usage

``` r
run_weighted_poverty_shortfall(
  data_list,
  var_name,
  wgt_name = NULL,
  times_median = 0.5,
  daily_poverty_line = NULL,
  type = c("type_4", "type_2"),
  percent = FALSE,
  na.rm = TRUE
)
```

## Arguments

- data_list:

  A named list of data frames.

- var_name:

  A string specifying the variable name (e.g., `"dhi"`, `"pilabour"`).

- wgt_name:

  A string (optional). The name of the weight variable. If `NULL`, equal
  weights are assumed.

- times_median:

  A numeric scalar. The multiple of the median used to define the
  poverty threshold (default is `0.5`).

- daily_poverty_line:

  A numeric scalar representing the **absolute poverty threshold per
  day**. (default is `NULL`).

- type:

  A character vector indicating the percentile estimation type (passed
  to `compute_weighted_percentiles`). Default is `"type_4"`.

- percent:

  Logical. If `TRUE`, returns the relative shortfall as a percentage of
  the poverty line. If `FALSE`, returns the absolute daily shortfall
  (default is `FALSE`).

- na.rm:

  Logical. Should missing values be removed before computation? Default
  is `TRUE`.

## Value

A named list. Each list element is named by country and contains a named
numeric vector, where the names are years and the values represent:

- The average daily shortfall in monetary units (if `percent = FALSE`),
  or

- The average relative shortfall in percentage terms (if
  `percent = TRUE`), among individuals or households below the poverty
  threshold.

## Details

When `daily_poverty_line` is specified, the function assumes the
threshold is expressed in daily units and converts it to an annual
threshold by multiplying it by 365. It is **critical** that the daily
poverty threshold be expressed in the **same monetary unit** as the
target income variable (`var_name`). For example, if income is reported
in PPP-adjusted USD, the threshold must also be in PPP-adjusted USD. The
function does not perform currency or PPP conversions.

## Examples

``` r
if (FALSE) { # \dontrun{
library(lissyrtools)
library(purrr)
library(dplyr)

datasets <- lissyrtools::lissyuse(data = c("de", "es", "uk"), vars = c("dhi"), from = 2016)

datasets <- datasets %>%
  map(~ .x %>% filter(!is.na(dhi))) %>%
  map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%
  apply_iqr_top_bottom_coding("dhi", "hwgt", type = "type_2") %>%
  apply_sqrt_equivalisation("dhi")

# Compute average poverty shortfall in daily monetary terms
avg_gap <- run_weighted_poverty_shortfall(
  data_list = datasets,
  var_name = "dhi",
  wgt_name = "new_wgt"
)

# Compute average poverty shortfall in percentage of the poverty line
percentage_gap <- run_weighted_poverty_shortfall(
  data_list = datasets,
  var_name = "dhi",
  wgt_name = "new_wgt",
  percent = TRUE
)

# Compute average poverty shortfall in daily monetary terms, converted to international dollars at 2017 prices. 
avg_gap_dollars <- datasets %>% 
  apply_ppp_adjustment("dhi", "lis", "lisppp") %>% 
  run_weighted_poverty_shortfall(
  var_name = "dhi",
  wgt_name = "new_wgt"
)

# Compute average poverty shortfall in daily monetary terms, from a specified poverty threshold in international dollars at 2017 prices. 
avg_gap_absolute_line <- datasets %>% 
  apply_ppp_adjustment("dhi", "lis", "lisppp") %>% 
  run_weighted_poverty_shortfall(
  var_name = "dhi",
  wgt_name = "new_wgt", 
  daily_poverty_line  = 10
)


print(avg_gap)
print(percentage_gap)
print(avg_gap_dollars)
print(avg_gap_absolute_line)
} # }
```
