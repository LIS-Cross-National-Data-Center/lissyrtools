# Return a file name with the requested format

Combines the information passed and returns it in the format stipulated
in argument 'to_format'.

## Usage

``` r
return_desired_format(country, year, database, level, to_format)
```

## Arguments

- country:

  A string with the country as 'cc'.

- year:

  A string with four digit year as 'yyyy'.

- database:

  A string with database as 'd'.

- level:

  A string with level as 'l'.

- to_format:

  A string with the desired format.

## Value

A string with the file name in the stipulated format.

## Details

To be used inside 'change_file_name_format()'.
