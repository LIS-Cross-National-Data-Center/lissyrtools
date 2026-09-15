# Print all the existing years in LCS for a given country.

Print all the existing years in LCS for a given country.

## Usage

``` r
get_years_lcs(iso2)
```

## Arguments

- iso2:

  A character vector with valid iso2 codes of countries present in LCS.

## Value

A list, made of numeric vectors. Each elements corresponds to a country
in LCS.

## Examples

``` r
get_years_lcs("it")
#> $it
#>  [1] 2012 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024
#> 
get_years_lcs(iso2 = c("lu", "es"))
#> $lu
#> [1] 2019 2024
#> 
#> $es
#>  [1] 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020
#> [16] 2021 2022 2023 2024
#> 
```
