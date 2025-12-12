# Change the format of a file name

**\[deprecated\]**

## Usage

``` r
change_file_name_format(file_names, to_format)
```

## Arguments

- file_names:

  A string with the name of the file.

- to_format:

  A string with the desired format. This can be one of the following:

  - 'ccyy' Two digit country, two digit year. E.g. "fr84"

  - 'ccyyyy' Two digit country, four digit year. E.g. "fr1984"

  - 'ccyyl' Two digit country, two digit year, one digit level ('h' or
    'p' for household/person). E.g. "fr84h"

  - 'ccyyyyl' Two digit country, four digit year, one digit level. E.g.
    "fr1984h"

  - 'ccyyd' Two digit country, two digit year, one digit database (LIS,
    LWS or ERFLIS - i/w/e). E.g. "fr84i"

  - 'ccyyyyd' Two digit country, four digit year, one digit database
    (LIS, LWS or ERFLIS - i/w/e). E.g. "fr1984i"

  - 'ccyydl' Two digit country, two digit year, one digit indicating
    LIS, LWS or ERFLIS (i/w/e/r), one digit household/person. E.g.
    "fr84ih"

  - 'ccyyyydl' Two digit country, two digit year,one digit indicating
    LIS, LWS or ERFLIS (i/w/e/r), one digit household/person. E.g.
    "fr1984ih"

## Details

Converts a character vector with a file names to a different format.

Is used within 'implement_adjust_by_lisppp()', .
