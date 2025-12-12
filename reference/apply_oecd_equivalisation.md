# Apply OECD Equivalence Scale to a Variable

This function adjusts a household-level variable (such as income or
consumption) using either the standard or modified OECD equivalence
scale. It accounts for household composition by assigning different
weights to additional adults and children.

## Usage

``` r
apply_oecd_equivalisation(data_list, var_name, modified = TRUE)
```

## Arguments

- data_list:

  A named list of data frames, from LIS or LWS microdata.

- var_name:

  A string. The name of the variable to be equivalised.

- modified:

  Logical. If `TRUE` (default), the modified OECD scale is used (0.5 for
  additional adults, 0.3 for children). If `FALSE`, the original OECD
  scale is applied (0.7 for additional adults, 0.5 for children).

## Value

A list of data frames with the equivalised variable.

## Details

The OECD equivalence scale adjusts household income or consumption to
reflect differences in household needs. It requires:

- `nhhmem`: total number of household members, (always loaded by default
  already)

- `nhhmem13`: number of household members under 14 years old

The scale is computed as:

- **Standard**: 1 + 0.7 × (additional adults) + 0.5 × (children under
  14)

- **Modified**: 1 + 0.5 × (additional adults) + 0.3 × (children under
  14)

## Examples

``` r
if (FALSE) { # \dontrun{
library(lissyrtools)
library(dplyr)

# Import data, ideally at the level of the variable of interest.
# The variable "nhhmem13" must be imported in advance in order to apply OECD-type equivalisation methods.
data_hhd_with_nhhmem13 <- lissyrtools::lissyuse("it", vars = c("dhi", "nhhmem13"), from = 2010)

# No equivalisation case
data_hhd_with_nhhmem13  %>%
 purrr::map(~ .x %>% filter(!is.na(dhi))) %>%
 run_weighted_mean("dhi", "hpopwgt")  

# Default case: modified OECD scale equivalisation
data_hhd_with_nhhmem13  %>%
 purrr::map(~ .x %>% filter(!is.na(dhi))) %>%
 apply_oecd_equivalisation("dhi") %>% 
 run_weighted_mean("dhi", "hpopwgt")  

# Standard OECD scale equivalisation
data_hhd_with_nhhmem13  %>%
 purrr::map(~ .x %>% filter(!is.na(dhi))) %>%
 apply_oecd_equivalisation("dhi", modified = FALSE) %>% 
 run_weighted_mean("dhi", "hpopwgt") 
} # } 
```
