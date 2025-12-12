# Adjust Monetary Variables for Inflation and PPP

Applies price adjustments to a monetary variable in a list of LIS/LWS
datasets using LIS-provided deflators. Adjustments can be made for
domestic inflation (CPI), purchasing power parity (PPP), or both
(`lisppp`).

## Usage

``` r
apply_ppp_adjustment(
  data_list,
  var_name,
  database,
  transformation = "lisppp",
  base_year_ppp = 2017
)
```

## Arguments

- data_list:

  A named list of data frames, from LIS or LWS microdata.

- var_name:

  A string. The name of the monetary variable to be adjusted.

- database:

  A string, either `"lis"` or `"lws"`, indicating the source database.
  Required for determining how to handle the deflators and income
  reference years.

- transformation:

  A string specifying the type of adjustment:

  - `"lisppp"` (default): Adjusts by CPI and by PPP.

  - `"cpi"`: Adjusts only for domestic inflation (CPI).

  - `"ppp"`: Adjusts only for purchasing power parity (PPP), for
    cross-country comparability.

- base_year_ppp:

  Numeric. One of the unique values in the `version_year` column of the
  `deflators` data frame. Default is 2017 Indicates the reference year
  of the PPP series (e.g., PPPs expressed in 2017 USD prices, PPPs
  expressed in 2021 USD prices).

## Value

A list of data frames, with the specified variable adjusted based on the
chosen transformation.

## Details

For LWS datasets and income variables, the function accounts for
discrepancies between survey years and income reference years. It merges
the appropriate deflator tables before applying the requested
adjustment.

**Important:** When using `"ppp"` or `"lisppp"` transformations, the
monetary values are converted out of their original currency. These
adjustments are intended to support cross-country comparability, but the
result is no longer expressed in national currency units.

## Examples

``` r
if (FALSE) { # \dontrun{ 
library(lissyrtools)
library(dplyr)

# --- Example 1: CPI Adjustment (Domestic Inflation, Italy) ---

it_data <- lissyuse("it", vars = "dhi", from = 2010)
run_weighted_mean(it_data, var_name = "dhi")  # Nominal income

it_data_cpi <- apply_ppp_adjustment(it_data, "dhi", database = "lis", transformation = "cpi")
run_weighted_mean(it_data_cpi, var_name = "dhi")  # Real income (CPI-adjusted, base year = 2017)


# --- Example 2: PPP Adjustment Across Countries (France, Poland, US, UK, Mexico in 2016) ---
multi_2016 <- lissyuse(c("fr16", "pl16", "us16", "uk16", "mx16"), vars = "dhi")
run_weighted_mean(multi_2016, var_name = "dhi")  # Nominal

multi_2016_ppp <- apply_ppp_adjustment(multi_2016, "dhi", database = "lis", transformation = "ppp")
run_weighted_mean(multi_2016_ppp, var_name = "dhi")  # PPP-adjusted


# --- Example 3: LIS PPP (Across Time and Countries: Canada & Mexico, 2015–2020) ---
can_mex <- lissyuse(c("ca15", "ca18", "ca20", "mx16", "mx18", "mx20"), vars = "dhi")
run_weighted_mean(can_mex, var_name = "dhi")  # Nominal

can_mex_lisppp <- apply_ppp_adjustment(can_mex, "dhi", database = "lis", transformation = "lisppp")
run_weighted_mean(can_mex_lisppp, var_name = "dhi")  # Fully deflated (real + PPP)


# --- Example 4:  --- Changing PPP Base Years (2017 vs 2021 Prices) ---
lis_data <- lissyuse(data = c("se", "uk"), vars = "dhi", from = 2016)

apply_ppp_adjustment(lis_data, "dhi", database = "lis", transformation = "lisppp", base_year_ppp = 2017) %>% run_weighted_mean(var_name = "dhi", wgt_name = "hwgt")
apply_ppp_adjustment(lis_data, "dhi", database = "lis", transformation = "lisppp", base_year_ppp = 2021) %>% run_weighted_mean(var_name = "dhi", wgt_name = "hwgt")

} # } 
```
