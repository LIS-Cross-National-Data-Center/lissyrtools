# Inspect whether a given variable contains more than just non-missing or non-zero values for a selected group of countries in the LIS or LWS databases.

Inspect whether a given variable contains more than just non-missing or
non-zero values for a selected group of countries in the LIS or LWS
databases.

## Usage

``` r
variable_exists(variable, iso2, lws = FALSE, share = FALSE)
```

## Arguments

- variable:

  A unit-length character vector containing specified LIS/LWS variables.

- iso2:

  A character vector with valid iso2 codes of countries present in
  LIS/LWS.

- lws:

  A logical value, that guides the tool to search in the LIS or LWS
  database. The argument is FALSE by default, taking LIS as the database
  to be investigated if nothing is specified.

- share:

  A logical value indicating whether to output the share of datasets,
  across the entire time series for each country, where a variable has
  more than just non-missing or non-zero values, instead of displaying
  its presence year by year.

## Value

A list made of character vectors. If share = TRUE, then a list with a
numeric vector.

## Examples

``` r
variable_exists(variable = "area_c", iso2 = "br")
#> $br
#>  1981  1982  1983  1984  1985  1986  1987  1988  1989  1990  1992  1993  1995 
#> "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" 
#>  1996  1997  1998  1999  2001  2002  2003  2004  2005  2006  2007  2008  2009 
#> "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" 
#>  2011  2012  2013  2014  2015  2016  2017  2018  2019  2020  2021  2022 
#> "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" 
#> 
variable_exists(variable = "basb", iso2 = c("fr", "de", "us", "uk"), lws = TRUE)
#> $fr
#>  2009  2014  2017  2020 
#>  "No" "Yes" "Yes" "Yes" 
#> 
#> $de
#> 2002 2007 2012 2017 
#> "No" "No" "No" "No" 
#> 
#> $us
#>  1995  1998  2001  2004  2007  2010  2013  2016  2019  2022 
#> "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" 
#> 
#> $uk
#>  2007  2009  2011  2013  2015  2017  2019  2021 
#> "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes"  "No" 
#> 
variable_exists(variable = "basb", iso2 = c("fr", "de", "us", "uk"), lws = TRUE, share = TRUE)
#> $`Share of years across the series in LWS where: basb has values other than zeros and missings.`
#>    de    fr    uk    us 
#>   0.0  75.0  87.5 100.0 
#> 
```
