# Print the survey used to construct the LCS datasets for a given country.

Print the survey used to construct the LCS datasets for a given country.

## Usage

``` r
get_surveys_lcs(iso2)
```

## Arguments

- iso2:

  A character vector with valid iso2 codes of countries present in LCS

## Value

A list, made of character vectors. Each elements corresponds to a
country in LCS.

## Examples

``` r
get_surveys_lcs("it")
#> $it
#>                            2012                            2014 
#> "Household Budget Survey (HBS)" "Household Budget Survey (HBS)" 
#>                            2015                            2016 
#> "Household Budget Survey (HBS)" "Household Budget Survey (HBS)" 
#>                            2017                            2018 
#> "Household Budget Survey (HBS)" "Household Budget Survey (HBS)" 
#>                            2019                            2020 
#> "Household Budget Survey (HBS)" "Household Budget Survey (HBS)" 
#>                            2021                            2022 
#> "Household Budget Survey (HBS)" "Household Budget Survey (HBS)" 
#>                            2023                            2024 
#> "Household Budget Survey (HBS)" "Household Budget Survey (HBS)" 
#> 
get_surveys_lcs(iso2 = c("lu", "es"))
#> $lu
#>                            2019                            2024 
#> "Household Budget Survey (HBS)" "Household Budget Survey (HBS)" 
#> 
#> $es
#>                            2006                            2007 
#> "Household Budget Survey (HBS)" "Household Budget Survey (HBS)" 
#>                            2008                            2009 
#> "Household Budget Survey (HBS)" "Household Budget Survey (HBS)" 
#>                            2010                            2011 
#> "Household Budget Survey (HBS)" "Household Budget Survey (HBS)" 
#>                            2012                            2013 
#> "Household Budget Survey (HBS)" "Household Budget Survey (HBS)" 
#>                            2014                            2015 
#> "Household Budget Survey (HBS)" "Household Budget Survey (HBS)" 
#>                            2016                            2017 
#> "Household Budget Survey (HBS)" "Household Budget Survey (HBS)" 
#>                            2018                            2019 
#> "Household Budget Survey (HBS)" "Household Budget Survey (HBS)" 
#>                            2020                            2021 
#> "Household Budget Survey (HBS)" "Household Budget Survey (HBS)" 
#>                            2022                            2023 
#> "Household Budget Survey (HBS)" "Household Budget Survey (HBS)" 
#>                            2024 
#> "Household Budget Survey (HBS)" 
#> 
```
