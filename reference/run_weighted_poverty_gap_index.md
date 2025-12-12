# Compute the Poverty Gap Index

Calculates the Foster–Greer–Thorbecke poverty gap index (FGT1) for a
list of datasets. This index measures the intensity of poverty by
combining the relative poverty rate with the average income shortfall
(as a percentage of the poverty line) among the poor.

## Usage

``` r
run_weighted_poverty_gap_index(
  data_list,
  var_name,
  wgt_name,
  times_median = 0.5,
  daily_poverty_line = NULL,
  type = c("type_4", "type_2"),
  na.rm = TRUE
)
```

## Arguments

- data_list:

  A named list of data frames.

- var_name:

  A string specifying the variable name (e.g., `"dhi"`, `"pilabour"`).

- wgt_name:

  A string. The name of the weight variable.

- times_median:

  A numeric scalar. The multiple of the median used to define the
  poverty threshold (default is `0.5`).

- daily_poverty_line:

  A numeric scalar representing the **absolute poverty threshold per
  day**. (default is `NULL`).

- type:

  A character vector indicating the percentile estimation type (passed
  to `compute_weighted_percentiles`). Default is `"type_4"`.

- na.rm:

  Logical. Should missing values be removed before computation? Default
  is `TRUE`.

## Value

A named list. Each list element is named by country and contains a named
numeric vector, where the names are years and the values are the
computed poverty gap indices (bounded between 0 and 1).

## Details

This function multiplies the relative poverty rate by the average
relative poverty shortfall among the poor, resulting in the FGT1 poverty
gap index. The result represents the average poverty gap across the
entire population as a fraction of the poverty line. When
`daily_poverty_line ` is specified, the function automatically assumes,
the scalar is expressed in daily units. Because the LIS dataset reports
**annual income**, the function internally converts the provided daily
threshold into an **annual threshold**, multiplying it by `365`
(calendar year).

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
  apply_sqrt_equivalisation("dhi") %>%
  apply_ppp_adjustment("dhi", "lis", "lisppp")

# Compute the FGT1 poverty gap index
pgi <- run_weighted_poverty_gap_index(
  data_list = datasets,
  var_name = "dhi",
  wgt_name = "new_wgt"
)

# Compute the FGT1 poverty gap index, using an absolut threshold expressed in daily terms. 
pgi_daily_threshold <- run_weighted_poverty_gap_index(
  data_list = datasets,
  var_name = "dhi",
  wgt_name = "new_wgt", 
  daily_poverty_line = 10
)


print(pgi)
print(pgi_daily_threshold)
} # }
```
