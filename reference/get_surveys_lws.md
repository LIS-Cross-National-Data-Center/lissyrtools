# Print the survey used to construct the LWS datasets for a given country.

Print the survey used to construct the LWS datasets for a given country.

## Usage

``` r
get_surveys_lws(iso2)
```

## Arguments

- iso2:

  A character vector with valid iso2 codes of countries present in LWS

## Value

A list, made of character vectors. Each elements corresponds to a
country in LWS.

## Examples

``` r
get_surveys_lws("it")
#> $it
#>  [1] "Survey of Household Income and Wealth (SHIW)"
#>  [2] "Survey of Household Income and Wealth (SHIW)"
#>  [3] "Survey of Household Income and Wealth (SHIW)"
#>  [4] "Survey of Household Income and Wealth (SHIW)"
#>  [5] "Survey of Household Income and Wealth (SHIW)"
#>  [6] "Survey of Household Income and Wealth (SHIW)"
#>  [7] "Survey of Household Income and Wealth (SHIW)"
#>  [8] "Survey of Household Income and Wealth (SHIW)"
#>  [9] "Survey of Household Income and Wealth (SHIW)"
#> [10] "Survey of Household Income and Wealth (SHIW)"
#> [11] "Survey of Household Income and Wealth (SHIW)"
#> [12] "Survey of Household Income and Wealth (SHIW)"
#> [13] "Survey of Household Income and Wealth (SHIW)"
#> 
get_surveys_lws(iso2 = c("fr", "jp"))
#> $fr
#> [1] "Household Wealth Survey (Patrimoine)"
#> [2] "Household Wealth Survey (Patrimoine)"
#> [3] "Household Wealth Survey (HVP)"       
#> [4] "Household Wealth Survey (HVP)"       
#> 
#> $jp
#>  [1] "Keio Household Panel Survey (KHPS)"      
#>  [2] "Japan Household Panel Survey (JHPS/KHPS)"
#>  [3] "Japan Household Panel Survey (JHPS/KHPS)"
#>  [4] "Japan Household Panel Survey (JHPS/KHPS)"
#>  [5] "Japan Household Panel Survey (JHPS/KHPS)"
#>  [6] "Japan Household Panel Survey (JHPS/KHPS)"
#>  [7] "Japan Household Panel Survey (JHPS/KHPS)"
#>  [8] "Japan Household Panel Survey (JHPS/KHPS)"
#>  [9] "Japan Household Panel Survey (JHPS/KHPS)"
#> [10] "Japan Household Panel Survey (JHPS/KHPS)"
#> [11] "Japan Household Panel Survey (JHPS/KHPS)"
#> [12] "Japan Household Panel Survey (JHPS/KHPS)"
#> [13] "Japan Household Panel Survey (JHPS/KHPS)"
#> [14] "Japan Household Panel Survey (JHPS/KHPS)"
#> 
```
