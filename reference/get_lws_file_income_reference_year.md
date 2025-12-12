# Retrieve the reference year for LWS files

**\[deprecated\]**

## Usage

``` r
get_lws_file_income_reference_year(file_name)
```

## Arguments

- file_name:

  String with the name of the dataset in one of the following formats:

  1.  'ccyy' Two digit country, two digit year. E.g. "fr84"

  2.  'ccyyyy' Two digit country, four digit year. E.g. "fr1984"

  3.  'ccyyl' Two digit country, two digit year, one digit level. E.g.
      "fr84h"

  4.  'ccyyyyl' Two digit country, four digit year, one digit
      household/person ('h' or 'p'). E.g. "fr1984h"

  5.  'ccyydl' Two digit country, two digit year, one digit indicating
      LIS, LWS or ERFLIS (i/w/e/r), one digit household/person. E.g.
      "fr84ih"

  6.  'ccyyyydl' Two digit country, two digit year,one digit indicating
      LIS, LWS or ERFLIS (i/w/e/r), one digit household/person. E.g.
      "fr1984ih"
