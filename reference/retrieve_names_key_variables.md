# Retrieve names of key variables

**\[superseded\]** Retrieve the names of key variables for a given
database and level. To be used only within 'read_lissy_files()' and
'read_lissy_files_locally()'.

## Usage

``` r
retrieve_names_key_variables(database, level)
```

## Arguments

- database:

  A string indicating the database of the key variables to be retrieved
  (e.g. 'lis', 'i', 'lws', 'w')

- level:

  A string indicating the level of the key variables to be retrieved
  (e.g. 'person', 'p', 'household', 'h')

## Value

A character vector with the names of key variables for a given database
and level.
