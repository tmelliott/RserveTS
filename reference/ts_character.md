# Character or string type

Strings are represented in Zod schema as either a string (`z.string()`),
or a string array (`z.array(z.string())`).

## Usage

``` r
ts_character(n = -1L, default = NULL)
```

## Arguments

- n:

  The length of the string vector. If `n = 1` then a single string is
  expected. If `n = 0` then any length is expected. If `n > 1` then a
  string vector of length `n` is expected.

- default:

  Default value for the type (optional).

## Value

A ts object that accepts strings or string vectors of length `n`.

## Examples

``` r
x <- ts_character(1)
x$check("a")
#> [1] "a"
```
