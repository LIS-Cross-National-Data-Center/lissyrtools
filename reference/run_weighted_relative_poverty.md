# Compute Relative Poverty Rate

Compute Relative Poverty Rate

## Usage

``` r
run_weighted_relative_poverty(
  data_list,
  var_name,
  wgt_name = NULL,
  times_median = 0.5,
  type = c("type_4", "type_2"),
  na.rm = TRUE
)
```

## Arguments

- data_list:

  A named list of data frames.

- var_name:

  A string specifying the variable name (e.g., "dhi", "pilabour").

- wgt_name:

  A string (optional). The name of the weight variable. If `NULL`, equal
  weights are assumed.

- times_median:

  A numeric scalar. The multiple of the median used to define the
  poverty threshold (default is 0.5).

- type:

  A character vector indicating the percentile estimation type (passed
  to `compute_weighted_percentiles`). Default is `"type_4"`.

- na.rm:

  Logical. Should missing values be removed before computation? Default
  is `TRUE`.

## Value

A named list. Each list element is named by country and contains a named
numeric vector, where the names are years and the values are the
computed statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
library(lissyrtools)
library(purrr)
library(dplyr)

datasets <- lissyrtools::lissyuse(data = c("de", "es", "uk"), vars = c("dhi"), from = 2016)

# Poverty line is defined at 50%  of the median value by default. 

rel_pvt_rate_50 <- datasets %>% 
 map(~ .x %>% filter(!is.na(dhi))) %>%
 map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%
 apply_iqr_top_bottom_coding("dhi", "hwgt", type = "type_2") %>%
 apply_sqrt_equivalisation("dhi") %>% 
 run_weighted_relative_poverty("dhi", "new_wgt")
 
print(rel_pvt_rate_50)  
 
 # It can be defined at other values by specifying the argument `times_median`
 
rel_pvt_rate_40 <- datasets %>% 
 map(~ .x %>% filter(!is.na(dhi))) %>%
 map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%
 apply_iqr_top_bottom_coding("dhi", "hwgt", type = "type_2") %>%
 apply_sqrt_equivalisation("dhi") %>% 
 run_weighted_relative_poverty("dhi", "new_wgt", times_median = 0.4)
 
print(rel_pvt_rate_40)   
} # }
```
