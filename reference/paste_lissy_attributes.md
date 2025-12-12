# Paste lissy attributes

Implements returns the 'lissy_object' with the 'lissy_attributes' if
'lissy_attributes is not NULL. Else, it just returns 'lissy_object'.

## Usage

``` r
paste_lissy_attributes(lissy_object, lissy_attributes)
```

## Arguments

- lissy_object:

  A list of tibbles or a tibble.

- lissy_attributes:

  A list of attributes or a NULL value, as obtained from
  'get_lissy_attributes()'.

## Value

A tibble.
