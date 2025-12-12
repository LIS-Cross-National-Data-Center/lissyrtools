# Retrieve the categories of a country-specific variable in LIS/LWS for a given country.

Retrieve the categories of a country-specific variable in LIS/LWS for a
given country.

## Usage

``` r
variable_country_specific_categories(
  variable,
  iso2,
  from = NULL,
  to = NULL,
  lws = FALSE,
  n_categories = FALSE
)
```

## Arguments

- variable:

  A unit-length character vector containing a LIS/LWS country-specific
  (with the "\_c" suffix) variable.

- iso2:

  A character vector with a valid iso2 code for countries present in
  LIS/LWS.

- from:

  A numeric value representing the year (inclusive) after which the
  LIS/LWS datasets should be considered.

- to:

  A numeric value representing the year (inclusive) up to which the
  LIS/LWS datasets should be considered.

- lws:

  A logical value, that guides the tool to search in the LIS or LWS
  database. The argument is FALSE by default, taking LIS as the databse
  to be investigated if nothing is specified.

- n_categories:

  A logical value indicating whether to output the number of categories
  of a single country-specific variable, across the entire time series
  for a given country.

## Value

A list made of character vectors. If n_categories = TRUE, then a list
with a numeric vector.

## Examples

``` r
library(lissyrtools)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

# In years where no data is recorded for a given variable, it is automatically hidden from the output
variable_exists(variable = "health_c", iso2 = "it")
#> $it
#>  1977  1978  1979  1980  1981  1982  1983  1984  1986  1987  1989  1991  1993 
#>  "No"  "No"  "No"  "No"  "No"  "No"  "No"  "No"  "No"  "No"  "No"  "No"  "No" 
#>  1995  1998  2000  2002  2004  2006  2008  2010  2012  2014  2016  2020 
#> "Yes"  "No"  "No"  "No"  "No" "Yes" "Yes" "Yes"  "No"  "No"  "No"  "No" 
#> 
variable_country_specific_categories(variable = "health_c", iso2 = "it", from = 1995, to = 2020) 
#> $`it10 - health status as described by the respondent`
#>           1           2           3           4           5 
#> "very good"      "good"      "fair"       "bad"  "very bad" 
#> 
#> $`it08 - health status as described by the respondent`
#>           1           2           3           4           5 
#> "very good"      "good"      "fair"       "bad"  "very bad" 
#> 
#> $`it06 - health status as described by the respondent`
#>           1           2           3           4           5 
#> "very good"      "good"      "fair"       "bad"  "very bad" 
#> 
#> $`it95 - health status as described by the respondent`
#>           1           2           3           4           5 
#> "very good"      "good"      "fair"       "bad"  "very bad" 
#> 

# To retrieve information on LWS datasets
variable_country_specific_categories(variable = "bus1_c", iso2 = "fi", lws = TRUE)
#> $`fi19 - legal form of non-traded self-employment businesses with an active role, main bu`
#>                                                                       0 
#> "does not own a business or has no active role in running the business" 
#>                                                                     111 
#>                        "sole proprietorship / independent professional" 
#>                                                                     112 
#>                                                           "partnership" 
#>                                                                     113 
#>                                           "limited liability companies" 
#> 
#> $`fi16 - legal form of non-traded self-employment businesses with an active role, main bu`
#>                                                                       0 
#> "does not own a business or has no active role in running the business" 
#>                                                                     111 
#>                        "sole proprietorship / independent professional" 
#>                                                                     112 
#>                                                           "partnership" 
#>                                                                     113 
#>                                           "limited liability companies" 
#> 
#> $`fi13 - legal form of the business, main business`
#>                                                0 
#>                        "does not own a business" 
#>                                              111 
#> "sole proprietorship / independent professional" 
#>                                              112 
#>                                    "partnership" 
#>                                              113 
#>                    "limited liability companies" 
#>                                              120 
#>         "no active role in running the business" 
#> 
#> $`fi09 - owns a business (or part of it) that is not publicly traded`
#>                         0                         1 
#> "does not own a business"         "owns a business" 
#> 

# Using the `n_categories` argument
variable_country_specific_categories(variable = "region_c", iso2 = "es", n_categories = TRUE)
#> $`LIS database. Number of distinct categories in variable: region_c.`
#> es22 es21 es20 es19 es18 es17 es16 es15 es14 es13 es12 es11 es10 es09 es08 es07 
#>   19   19   19   19   19   19   19   19   19   19   19   19   19   19   19   19 
#> es06 es05 es04 es00 es99 es98 es97 es96 es95 es94 es93 es90 es80 
#>   19   19   18    7    7    7    7    7    7    7    7   18   18 
#> 

# To use this function acroos multiples countries one could make use of the `purrr::map()` function 
purrr::map(lissyrtools::get_countries_lws(), ~variable_country_specific_categories(variable = "bus1_c", iso2 = .x, lws = TRUE , n_categories = TRUE))
#> Warning: The selected variable: bus1_c, does not have values other than zeros or missings for the selectd years.
#> Warning: The selected variable: bus1_c, does not have values other than zeros or missings for the selectd years.
#> Warning: The selected variable: bus1_c, does not have values other than zeros or missings for the selectd years.
#> Warning: The selected variable: bus1_c, does not have values other than zeros or missings for the selectd years.
#> Warning: The selected variable: bus1_c, does not have values other than zeros or missings for the selectd years.
#> Warning: The selected variable: bus1_c, does not have values other than zeros or missings for the selectd years.
#> Warning: The selected variable: bus1_c, does not have values other than zeros or missings for the selectd years.
#> Warning: The selected variable: bus1_c, does not have values other than zeros or missings for the selectd years.
#> Warning: The selected variable: bus1_c, does not have values other than zeros or missings for the selectd years.
#> Warning: The selected variable: bus1_c, does not have values other than zeros or missings for the selectd years.
#> $Australia
#> $Australia$`LWS database. Number of distinct categories in variable: bus1_c.`
#> named integer(0)
#> 
#> 
#> $Austria
#> $Austria$`LWS database. Number of distinct categories in variable: bus1_c.`
#> at21 at17 at14 at11 
#>    5    5    6    5 
#> 
#> 
#> $Canada
#> $Canada$`LWS database. Number of distinct categories in variable: bus1_c.`
#> ca19 ca16 ca12 ca05 ca99 
#>    2    2    2    2    2 
#> 
#> 
#> $Chile
#> $Chile$`LWS database. Number of distinct categories in variable: bus1_c.`
#> named integer(0)
#> 
#> 
#> $Denmark
#> $Denmark$`LWS database. Number of distinct categories in variable: bus1_c.`
#> named integer(0)
#> 
#> 
#> $Estonia
#> $Estonia$`LWS database. Number of distinct categories in variable: bus1_c.`
#> ee21 ee17 ee13 
#>    6    6    5 
#> 
#> 
#> $Finland
#> $Finland$`LWS database. Number of distinct categories in variable: bus1_c.`
#> fi19 fi16 fi13 fi09 
#>    4    4    5    2 
#> 
#> 
#> $France
#> $France$`LWS database. Number of distinct categories in variable: bus1_c.`
#> fr20 fr17 fr14 fr09 
#>   62   64   63   72 
#> 
#> 
#> $Germany
#> $Germany$`LWS database. Number of distinct categories in variable: bus1_c.`
#> named integer(0)
#> 
#> 
#> $Greece
#> $Greece$`LWS database. Number of distinct categories in variable: bus1_c.`
#> gr21 gr18 gr14 gr09 
#>    7    5    6    6 
#> 
#> 
#> $India
#> $India$`LWS database. Number of distinct categories in variable: bus1_c.`
#> named integer(0)
#> 
#> 
#> $Italy
#> $Italy$`LWS database. Number of distinct categories in variable: bus1_c.`
#> it20 it16 it14 it12 it10 it08 it06 it04 it02 it00 it98 it95 
#>    2    7    6    7    6    7    7    7    2    2    2    2 
#> 
#> 
#> $Japan
#> $Japan$`LWS database. Number of distinct categories in variable: bus1_c.`
#> named integer(0)
#> 
#> 
#> $Luxembourg
#> $Luxembourg$`LWS database. Number of distinct categories in variable: bus1_c.`
#> lu21 lu18 lu14 lu10 
#>    2    6    7    5 
#> 
#> 
#> $Mexico
#> $Mexico$`LWS database. Number of distinct categories in variable: bus1_c.`
#> named integer(0)
#> 
#> 
#> $Norway
#> $Norway$`LWS database. Number of distinct categories in variable: bus1_c.`
#> named integer(0)
#> 
#> 
#> $Slovakia
#> $Slovakia$`LWS database. Number of distinct categories in variable: bus1_c.`
#> sk21 sk17 sk14 sk10 
#>    4    5    6    6 
#> 
#> 
#> $Slovenia
#> $Slovenia$`LWS database. Number of distinct categories in variable: bus1_c.`
#> si21 si17 si14 
#>    5    5    6 
#> 
#> 
#> $`South Africa`
#> $`South Africa`$`LWS database. Number of distinct categories in variable: bus1_c.`
#> named integer(0)
#> 
#> 
#> $`South Korea`
#> $`South Korea`$`LWS database. Number of distinct categories in variable: bus1_c.`
#> named integer(0)
#> 
#> 
#> $Spain
#> $Spain$`LWS database. Number of distinct categories in variable: bus1_c.`
#> es22 es21 es17 es14 es11 es08 
#>    7    6    6    6    5    5 
#> 
#> 
#> $Sweden
#> $Sweden$`LWS database. Number of distinct categories in variable: bus1_c.`
#> se02 
#>    2 
#> 
#> 
#> $`United Kingdom`
#> $`United Kingdom`$`LWS database. Number of distinct categories in variable: bus1_c.`
#> uk21 uk19 uk17 uk15 uk13 uk11 uk09 uk07 
#>    2    2   59   51   52   56   52   54 
#> 
#> 
#> $`United States`
#> $`United States`$`LWS database. Number of distinct categories in variable: bus1_c.`
#> us22 us19 us16 us13 us10 us07 us04 us01 us98 us95 
#>    8    8    8    8    9    7    8    7    7    8 
#> 
#> 
```
