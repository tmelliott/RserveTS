# Typed factor

Factors are integers with labels. On the JS side, these are *always*
represented as a string array (even if only one value - yay!).

## Usage

``` r
ts_factor(levels = NULL)
```

## Arguments

- levels:

  A character vector of levels (optional).

## Value

A ts object that accepts factors with the specified levels.

## Examples

``` r
x <- ts_factor(levels = c("a", "b"))
x$check(factor("a", levels = c("a", "b")))
#> [1] a
#> Levels: a b

if (FALSE) { # \dontrun{
# this will fail
x$check("a")
x$check(factor("c", levels = c("a", "b", "c")))
} # }
```
