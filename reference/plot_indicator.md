# Plot an Indicator.

**\[deprecated\]** Computes an indicator and plots it for one or
multiple LIS or LWS datasets.

## Usage

``` r
plot_indicator(
  lissy_files,
  variable,
  indicator,
  weight = NULL,
  type = NULL,
  plot_theme = NULL,
  ratio = NULL,
  epsilon = NULL,
  na.rm = FALSE,
  files_level = NULL,
  variable_level = NULL
)
```

## Arguments

- lissy_files:

  A list of LIS or LWS files.

- variable:

  A character string indicating the aggregate for which the indicator
  needs to be computed.

- indicator:

  A character string indicating the type of indicator statistic to be
  computed. Currently the function supports only 'mean', 'median',
  'ratio' and 'gini'.

- weight:

  A string with the name of the variable in 'file' that should be used
  as sample weights. If NULL (default), the function tries to guess the
  needed weight to compute the indicator. This guess is made on the
  information from files_level and variable_level.

- type:

  A character vector indicating the type of plot to be used. Valid
  inputs are 'line' and 'bar'. If NULL (default), the function tries to
  guess the appropriate plot type based on the structure of the input
  data frame.

- plot_theme:

  A character vector.

- ratio:

  A vector of two numeric values between 0 and 1.Only used in the
  computation of 'ratio' indicator. Defines the percentiles in the
  numerator and denominator respectively. E.g. (0.9, 0.1) computes the
  90/10 ratio.

- epsilon:

  A numeric vector of length one. Only used in the computation of
  'atkinson' indicator'. The inequality adversion parameter. Needs to be
  epsilon \> 0.

- na.rm:

  A boolean. Indicates if NAs should be ignored. Defaults to FALSE.

- files_level:

  A string indicating the level of the file. Valid inputs are:
  'household', 'h', 'person' or 'p'. If NULL (default), the file level
  will be retrived from the 'lissy_files' attributes.

- variable_level:

  Level of the variable. Should be either 'household', 'h', 'person' or
  'p'. If NULL (default), the function will try to guess the level of
  the variable. This is done by comparing the value in 'variable' with
  pre-set lists of variables.

## Value

A ggplot2 plot.
