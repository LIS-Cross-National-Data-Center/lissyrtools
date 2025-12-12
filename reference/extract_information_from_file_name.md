# Extract information from file name

Extracts the information regarding country, year, database and level.
The latter two only if available.

## Usage

``` r
extract_information_from_file_name(file_name, current_format)
```

## Arguments

- file_name:

  A string with the name of the file.

- current_format:

  A string with the currant format.

## Value

A list with the following elements:

- 'country\_' ('cc')

- 'year\_' ('yyyy')

- 'database\_' ('d')

- 'level\_' ('l')

## Details

To be used inside 'change_file_name_format()'.
