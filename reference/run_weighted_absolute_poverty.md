# Compute Absolute Poverty Rate

Compute Absolute Poverty Rate

## Usage

``` r
run_weighted_absolute_poverty(
  data_list,
  var_name,
  wgt_name = NULL,
  daily_poverty_line = 2.15,
  days_in_year = 365,
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

- daily_poverty_line:

  A numeric scalar representing the **absolute poverty threshold per
  day**. **Note:** This value must be expressed in the **same monetary
  unit** as the target variable.

- days_in_year:

  Integer. Number of days used to convert the daily poverty line to an
  annual equivalent. Default is `365`.

- na.rm:

  Logical. Should missing values be removed before computation? Default
  is `TRUE`.

## Value

A named list. Each list element is named by country and contains a named
numeric vector, where the names are years and the values are the
computed statistics.

## Details

This function is intended to compute **absolute poverty** based on
**fixed international or national thresholds**. Because the LIS dataset
reports **annual income**, the function internally converts the provided
daily threshold into an **annual threshold** using `days_in_year`. The
default is `365` (calendar year), but can be adjusted. It is
**critical** that the daily poverty threshold be expressed in the **same
monetary unit** as the target income variable (`var_name`). For example,
if income is reported in PPP-adjusted USD, the threshold must also be in
PPP-adjusted USD. The function does not perform currency or PPP
conversions.

## Examples

``` r
if (FALSE) { # \dontrun{
library(lissyrtools)
library(purrr)
library(dplyr)

datasets <- lissyrtools::lissyuse(data = c("de", "es", "uk"), vars = c("dhi"), from = 2016)

# Poverty line is defined at 2.15$ 2017 PPP per day, by dafault. 

abs_pvt_rate_215 <- datasets %>% 
 map(~ .x %>% filter(!is.na(dhi))) %>%
 map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%
 apply_iqr_top_bottom_coding("dhi", "hwgt", type = "type_2") %>%
 apply_sqrt_equivalisation("dhi") %>% 
 apply_ppp_adjustment("dhi", database = "lis", transformation = "lisppp") %>%  
 run_weighted_absolute_poverty("dhi", "new_wgt")
 
print(abs_pvt_rate_215)  
 
 # It can be defined to any other threshold. 
 
abs_pvt_rate_685 <- datasets %>% 
 map(~ .x %>% filter(!is.na(dhi))) %>%
 map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%
 apply_iqr_top_bottom_coding("dhi", "hwgt", type = "type_2") %>%
 apply_sqrt_equivalisation("dhi") %>% 
 apply_ppp_adjustment("dhi", database = "lis", transformation = "lisppp") %>%  
 run_weighted_absolute_poverty("dhi", "new_wgt", daily_poverty_line = 6.85)
 
print(abs_pvt_rate_685)   
} # }
```
