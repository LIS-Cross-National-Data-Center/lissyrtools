# Print the survey used to construct the LIS datasets for a given country.

Print the survey used to construct the LIS datasets for a given country.

## Usage

``` r
get_surveys_lis(iso2)
```

## Arguments

- iso2:

  A character vector with valid iso2 codes of countries present in LIS

## Value

A list, made of character vectors. Each elements corresponds to a
country in LIS.

## Examples

``` r
get_surveys_lis("it")
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
#> [14] "Survey of Household Income and Wealth (SHIW)"
#> [15] "Survey of Household Income and Wealth (SHIW)"
#> [16] "Survey of Household Income and Wealth (SHIW)"
#> [17] "Survey of Household Income and Wealth (SHIW)"
#> [18] "Survey of Household Income and Wealth (SHIW)"
#> [19] "Survey of Household Income and Wealth (SHIW)"
#> [20] "Survey of Household Income and Wealth (SHIW)"
#> [21] "Survey of Household Income and Wealth (SHIW)"
#> [22] "Survey of Household Income and Wealth (SHIW)"
#> [23] "Survey of Household Income and Wealth (SHIW)"
#> [24] "Survey of Household Income and Wealth (SHIW)"
#> [25] "Survey of Household Income and Wealth (SHIW)"
#> [26] "Survey of Household Income and Wealth (SHIW)"
#> 
get_surveys_lis(iso2 = c("uy", "pe"))
#> $uy
#>  [1] "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)"
#>  [3] "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)"
#>  [5] "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)"
#>  [7] "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)"
#>  [9] "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)"
#> [11] "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)"
#> [13] "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)"
#> [15] "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)"
#> [17] "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)"
#> [19] "Continuous Household Survey (ECH)"
#> 
#> $pe
#>  [1] "National Household Survey (ENAHO)" "National Household Survey (ENAHO)"
#>  [3] "National Household Survey (ENAHO)" "National Household Survey (ENAHO)"
#>  [5] "National Household Survey (ENAHO)" "National Household Survey (ENAHO)"
#>  [7] "National Household Survey (ENAHO)" "National Household Survey (ENAHO)"
#>  [9] "National Household Survey (ENAHO)" "National Household Survey (ENAHO)"
#> [11] "National Household Survey (ENAHO)" "National Household Survey (ENAHO)"
#> [13] "National Household Survey (ENAHO)" "National Household Survey (ENAHO)"
#> [15] "National Household Survey (ENAHO)" "National Household Survey (ENAHO)"
#> [17] "National Household Survey (ENAHO)"
#> 
```
