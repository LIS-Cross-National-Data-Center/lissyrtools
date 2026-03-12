# Print all the existing years in LIS for a given country.

Print all the existing years in LIS for a given country.

## Usage

``` r
get_years_lis(iso2)
```

## Arguments

- iso2:

  A character vector with valid iso2 codes of countries present in LIS.

## Value

A list, made of numeric vectors. Each elements corresponds to a country
in LIS.

## Examples

``` r
get_years_lis("it")
#> $it
#>  [1] 1977 1978 1979 1980 1981 1982 1983 1984 1986 1987 1989 1991 1993 1995 1998
#> [16] 2000 2002 2004 2006 2008 2010 2012 2014 2016 2020 2022
#> 
get_years_lis(iso2 = c("de", "jp"))
#> $de
#>  [1] 1973 1978 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995
#> [16] 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010
#> [31] 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022
#> 
#> $jp
#>  [1] 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020
#> 
```
