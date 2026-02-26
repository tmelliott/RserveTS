# Typed dataframe

This is essentially a list, but the elements must have names and are all
the same length.

## Usage

``` r
ts_dataframe(..., default = NULL)
```

## Arguments

- ...:

  Named types.

- default:

  Default value for the type (optional).

## Value

A ts object that accepts data frames with the specified types.

## Examples

``` r
x <- ts_dataframe(a = ts_integer(1), b = ts_character(1))
x$check(data.frame(a = 1L, b = "a"))
#>   a b
#> 1 1 a
```
