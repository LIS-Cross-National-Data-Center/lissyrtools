# Print all the existing years in LWS for a given country.

Print all the existing years in LWS for a given country.

## Usage

``` r
get_years_lws(iso2)
```

## Arguments

- iso2:

  A character vector with valid iso2 codes of countries present in LWS.

## Value

A list, made of numeric vectors. Each elements corresponds to a country
in LWS.

## Examples

``` r
get_years_lws("it")
#> $it
#>  [1] 1995 1998 2000 2002 2004 2006 2008 2010 2012 2014 2016 2020 2022
#> 
get_years_lws(iso2 = c("de", "jp"))
#> $de
#> [1] 2002 2007 2012 2017
#> 
#> $jp
#>  [1] 2004 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021
#> 
```
