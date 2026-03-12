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
#>                                           1977 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           1978 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           1979 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           1980 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           1981 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           1982 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           1983 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           1984 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           1986 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           1987 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           1989 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           1991 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           1993 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           1995 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           1998 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           2000 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           2002 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           2004 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           2006 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           2008 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           2010 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           2012 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           2014 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           2016 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           2020 
#> "Survey of Household Income and Wealth (SHIW)" 
#>                                           2022 
#> "Survey of Household Income and Wealth (SHIW)" 
#> 
get_surveys_lis(iso2 = c("uy", "pe"))
#> $uy
#>                                2004                                2005 
#> "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)" 
#>                                2006                                2007 
#> "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)" 
#>                                2008                                2009 
#> "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)" 
#>                                2010                                2011 
#> "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)" 
#>                                2012                                2013 
#> "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)" 
#>                                2014                                2015 
#> "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)" 
#>                                2016                                2017 
#> "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)" 
#>                                2018                                2019 
#> "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)" 
#>                                2022                                2023 
#> "Continuous Household Survey (ECH)" "Continuous Household Survey (ECH)" 
#>                                2024 
#> "Continuous Household Survey (ECH)" 
#> 
#> $pe
#>                                2004                                2005 
#> "National Household Survey (ENAHO)" "National Household Survey (ENAHO)" 
#>                                2006                                2007 
#> "National Household Survey (ENAHO)" "National Household Survey (ENAHO)" 
#>                                2008                                2009 
#> "National Household Survey (ENAHO)" "National Household Survey (ENAHO)" 
#>                                2010                                2011 
#> "National Household Survey (ENAHO)" "National Household Survey (ENAHO)" 
#>                                2012                                2013 
#> "National Household Survey (ENAHO)" "National Household Survey (ENAHO)" 
#>                                2014                                2015 
#> "National Household Survey (ENAHO)" "National Household Survey (ENAHO)" 
#>                                2016                                2017 
#> "National Household Survey (ENAHO)" "National Household Survey (ENAHO)" 
#>                                2018                                2019 
#> "National Household Survey (ENAHO)" "National Household Survey (ENAHO)" 
#>                                2021 
#> "National Household Survey (ENAHO)" 
#> 
```
