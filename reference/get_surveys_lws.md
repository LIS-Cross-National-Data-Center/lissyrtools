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
get_surveys_lws(iso2 = c("fr", "jp"))
#> $fr
#>                                   2009                                   2014 
#> "Household Wealth Survey (Patrimoine)" "Household Wealth Survey (Patrimoine)" 
#>                                   2017                                   2020 
#>        "Household Wealth Survey (HVP)"        "Household Wealth Survey (HVP)" 
#> 
#> $jp
#>                                       2004 
#>       "Keio Household Panel Survey (KHPS)" 
#>                                       2009 
#> "Japan Household Panel Survey (JHPS/KHPS)" 
#>                                       2010 
#> "Japan Household Panel Survey (JHPS/KHPS)" 
#>                                       2011 
#> "Japan Household Panel Survey (JHPS/KHPS)" 
#>                                       2012 
#> "Japan Household Panel Survey (JHPS/KHPS)" 
#>                                       2013 
#> "Japan Household Panel Survey (JHPS/KHPS)" 
#>                                       2014 
#> "Japan Household Panel Survey (JHPS/KHPS)" 
#>                                       2015 
#> "Japan Household Panel Survey (JHPS/KHPS)" 
#>                                       2016 
#> "Japan Household Panel Survey (JHPS/KHPS)" 
#>                                       2017 
#> "Japan Household Panel Survey (JHPS/KHPS)" 
#>                                       2018 
#> "Japan Household Panel Survey (JHPS/KHPS)" 
#>                                       2019 
#> "Japan Household Panel Survey (JHPS/KHPS)" 
#>                                       2020 
#> "Japan Household Panel Survey (JHPS/KHPS)" 
#>                                       2021 
#> "Japan Household Panel Survey (JHPS/KHPS)" 
#> 
```
