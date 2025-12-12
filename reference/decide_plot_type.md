# Decide plot type based on input data frame

This function determines the appropriate plot type based on the
structure of the input data frame. Use only within 'plot_indicator()'

## Usage

``` r
decide_plot_type(results_df)
```

## Arguments

- results_df:

  A data frame containing the results of the indicator. Needs to contain
  'file', 'country', 'year' and 'value' columns. Use only within
  'plot_indicator()

## Value

A character string indicating the appropriate plot type. The possible
values are:

- "line": A line plot with one or multiple lines.

- "bar": A bar plot with one or multiple bars.

## Details

The function determines the plot type based on the number of countries
and years in the input data frame. If the data frame has multiple
countries and multiple years, the plot type is a line plot with multiple
lines. If the data frame has multiple years and only one country, the
plot type is a line plot with one line. If the data frame has only one
year and multiple countries, the plot type is a bar plot with multiple
bars (sorted). If the data frame has only one year and only one country,
the plot type is a bar plot with one bar.

## Examples

``` r
# Create a test data frame
results_df <- data.frame(
  file = c("us2010h", "us2011h", "ca2010h", "ca2011h"),
  year = c(2010, 2011, 2010, 2011),
  value = c(1, 2, 3, 4)
)

# Determine the appropriate plot type
decide_plot_type(results_df)
#> [1] "line"
# Output: "line"
```
