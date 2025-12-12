# Transform Structured Data Lists into a Tidy Data Frame for Plotting

This function takes a nested list of data (with up to three possible
structures) and transforms it into a tidy `data.frame` suitable for
visualization in LISSY. It supports three structures, typically outputs
by weighted summary functions in lissyrtools such as:
[`run_weighted_mean()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_mean.md),
[`run_weighted_percentiles()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_percentiles.md),
and
[`run_weighted_count()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/run_weighted_count.md).

## Usage

``` r
structure_to_plot(data_list, print_columns = TRUE)
```

## Arguments

- data_list:

  A named list containing the data to be transformed. The structure and
  naming conventions of this list determine how the data is processed.

  - **1st structure:** List with country names as keys and named vectors
    of year-values. Expected when `names(data_list)` are country codes
    matching
    [`get_countries_lis()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/get_countries_lis.md)
    or
    [`get_countries_lws()`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/get_countries_lws.md).

  - **2nd structure:** List with `ccyy` abbreviations as keys, and named
    numeric/integer vectors, where names represent categories. Typically
    output from functions grouped by a categorical variable, e.g.
    `"educ"`.

  - **3rd structure:** List with `ccyy` abbreviations as keys, each
    containing sublists of named vectors. Represents more complex
    summaries with multiple grouping variables, percentiles, and shares
    or averages across the distribution.

- print_columns:

  Logical. If `TRUE`, it prints an informative message on the resulting
  data structure, and the names of its columns.

## Value

A tidy `data.frame` with the following columns (depending on input
structure):

- `cname`: Country name.

- `year`: Year.

- `dname`: Dataset country-year code in the `ccyy` form.

- `category`: (2nd and 3rd structure) Grouping variable categories.

- `distribution_group` / `percentile` / `by_var`: (3rd structure)
  Variable describing the vector names.

- `value`: Numeric values from the input list.

## Details

Depending on the summary statistics computed before, the function
renames one of the columns in the 3rd structure based on pattern
matching:

- If the values contains "%" and "-", the column is renamed to
  `distribution_group`.

- If the values contains "%", renamed to `percentile`.

- Otherwise, renamed to `by_var`.

## Examples

``` r
if (FALSE) { # \dontrun{

library(ggplot2)
library(lissyrtools)
library(RColorBrewer)
library(ggthemes)
library(purrr)
library(forcats)
library(dplyr)

data <- lissyrtools::lissyuse(data = c("es", "de"), vars = c("dhi", "educ", "pi11", "rural"), from = 2016)

# Example usage for 1st structure
weighted_means <- run_weighted_mean(data, "pi11")
df1 <- structure_to_plot(weighted_means)

# Example usage for 2nd structure
weighted_means_educ <- run_weighted_mean(data, "pi11", by = "educ")
df2 <- structure_to_plot(weighted_means_educ)

# Example usage for 3rd structure
weighted_percentiles <- run_weighted_percentiles(data, "pi11", by = "educ")
df3 <- structure_to_plot(weighted_percentiles)

# Example usage for 3rd structure but with shares or averages 
weighted_percentiles <- run_weighted_percentiles(data, "pi11", by = "educ", share = TRUE)
df3 <- structure_to_plot(weighted_percentiles)



# Chart example: Plotting education group shares over years by country
 

run_weighted_count(data, "educ", percent = TRUE, na.rm = TRUE) %>%
  structure_to_plot() %>%
  ggplot(aes(x = year, y = value, color = cname, group = interaction(cname, category))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 0.6) +
  scale_color_stata() +
  labs(
    x = "Year",
    y = "Share of Education Group (%)",
    color = "Country"
  ) + 
  expand_limits(y = 0) + 
  facet_grid(~fct_relevel(category, 'low', 'medium', 'high')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
  

# Another example: plotting weighted mean of 'dhi' over years by country
data %>%
  purrr::map(~ .x %>% filter(relation == 1000) %>% mutate(new_wgt = nhhmem * hwgt)) %>%
  apply_ppp_adjustment("dhi", "lis", "lisppp") %>%
  run_weighted_mean("dhi", "new_wgt") %>%
  structure_to_plot() %>%
  ggplot(aes(x = year, y = value, color = cname, group = cname)) +
  geom_point() +
  geom_line() +
  labs(
    title = "dhi trend",
    caption = "Source: Luxembourg Income Study"
  ) +
  scale_color_stata() +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
} # }
```
