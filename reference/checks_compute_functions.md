# Perform checks for 'compute' functions.

**\[deprecated\]** Perform checks within 'compute\_\*()' functions. The
checks performed are:

- The file is a data.frame type of object.

- 'varaible' is a column in file

- 'variable' is a numeric variable.

- The weight variable is a column in file.

- 'weight' variable contains valid values (i.e. different than NAs and
  0s).

## Usage

``` r
checks_compute_functions(file, file_name, variable, weight = NULL)
```

## Arguments

- file:

  A LIS or LWS file.

- file_name:

  The name of the LIS or LWS file.

- variable:

  A string with the name of the variable for which checks should be
  computed.

- weight:

  A string with the name of the variable in 'file' that should be used
  as sample weights. If NULL (default), checks for weight variable are
  not performed.

## Details

The checks related to the 'weight' variable are only performed if
argument 'weight' is not NULL.
